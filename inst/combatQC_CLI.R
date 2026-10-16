suppressMessages(library(argparser))
suppressMessages(library(readr))
suppressMessages(library(stringr))
suppressMessages(library(readxl))
suppressMessages(library(ComBatFamQC))
suppressMessages(library(parallel))

## Read in arguments
p <- arg_parser("Streamline interactive batch effect diagnostics and apply ComBat harmonization", hide.opts = FALSE)
p <- add_argument(p, "data", help = "path to the CSV or EXCEL file that contains data to be harmonized, covariates and batch information")
p <- add_argument(p, "--diagnosis", short = '-d', help = "a boolean variable indicating whether to run harmonization or batch effect diagnosis. TRUE indicates batch effect diagnosis.", default = TRUE)
p <- add_argument(p, "--visualization", short = '-v', help = "a boolean variable indicating whether to start the Shiny app or save only the diagnosis result. TRUE indicates that the Shiny app will be started. False indicates that the diagnosis result will be exported to an EXCEL file or a quarto report.", default = TRUE)
p <- add_argument(p, "--after", short = '-a', help = "a boolean variable indicating whether the dataset is before or after harmonization. TRUE indicates the dataset is after harmonization.", default = FALSE)
p <- add_argument(p, "--family", short = '-f', help = "combat family to use: comfam, covfam, or combatls", default = "comfam")
p <- add_argument(p, "--features", help = "position of data to be harmonized(column numbers), eg: 1-5,9")
p <- add_argument(p, "--covariates", short = '-c', help = "position of covariates (column numbers)", default = "NULL")
p <- add_argument(p, "--batch", short = '-b', help = "position of batch column (column number)")
p <- add_argument(p, "--model", short = '-m', help = paste("Regression model for ComBat/CovBat (lm, gam, lmer).",
                                                   "ComBatLS always uses GAMLSS; --model is ignored."), default = "lm")
p <- add_argument(p, "--formula", help = "ComBatLS only, required: quoted mean formula using column names and response y, e.g. 'y ~ pb(AGE) + SEX' or 'y ~ 1'", default = "NULL")
p <- add_argument(p, "--sigma.formula", help = "ComBatLS only: quoted one-sided scale formula using column names, e.g. '~ pb(AGE) + SEX'; omitted defaults to '~ 1'. Include variables from both formulas in --covariates", default = "NULL")
p <- add_argument(p, "--smooth", short = '-s', help = "provide the variables that require a smooth function", default = "NULL")
p <- add_argument(p, "--interaction", help = "specify the interaction terms in the format of col_index1*col_index2, eg 2*3,11*12", default = "NULL")
p <- add_argument(p, "--int_type", help = "specify an interaction type for gam models, eg: linear, categorical-continuous, factor-smooth, tensor, smooth-smooth", default = "linear")
p <- add_argument(p, "--random", short = '-r', help = "specify the random intercept-effects", default = "NULL")
p <- add_argument(p, "--eb", help = "whether to use ComBat model with empirical Bayes for mean and variance", default = TRUE)
p <- add_argument(p, "--robust.LS", help = "whether to use robust location and scale estimators for error variance and site effect parameters", default = FALSE)
p <- add_argument(p, "--ref.batch", help = "reference batch", default = "NULL")
p <- add_argument(p, "--score.model", help = "model for scores, defaults to NULL for fitting basic location and scale model without covariates on the scores", default = "NULL")
p <- add_argument(p, "--score.args", help = "list of arguments for score model, input should have the following format arg1,arg2,...", default = "NULL")
p <- add_argument(p, "--percent.var", help = "the number of harmonized principal component scores is selected to explain this proportion of the variance", default = 0.95)
p <- add_argument(p, "--n.pc", help = "if specified, this number of principal component scores is harmonized", default = "NULL")
p <- add_argument(p, "--std.var", help = "if TRUE, scales variances to be equal to 1 before PCA", default = TRUE)
p <- add_argument(p, "--predict", help = "a boolean variable indicating whether to run ComBat from scratch or apply existing model to new dataset (currently only work for original ComBat and ComBat-GAM)", default = FALSE)
p <- add_argument(p, "--object", help = "path to the ComBat model to be used for prediction, should have an extension of .rds", default = "NULL")
p <- add_argument(p, "--reference", help = "path to the CSV or EXCEL file that contains dataset to be considered as the reference group", default = "NULL")
p <- add_argument(p, "--outdir", short = '-o', help = "full path (including the file name) where harmonized data should be written, or the directory path to save the diagnosis result.")
p <- add_argument(p, "--mout", help = "full path where ComBat model to be saved")
p <- add_argument(p, "--cores", help = "number of cores used for paralleling computing, please provide a numeric value", default = "all")
p <- add_argument(p, "--mdmr", help = "a boolean variable indicating whether to run the MDMR test.", default = TRUE)
p <- add_argument(p, "--quarto", help = "a boolean variable indicating whether to export a quarto report.", default = TRUE)
argv <- parse_args(p)

# Preprocess inputs
message('Checking inputs...')

# ComBatLS selects GAMLSS through its family; lm is only the internal API default.
model_type <- if (argv$family == "combatls" && !argv$diagnosis) "lm" else argv$model

## ComBatLS formula options apply only to first-time harmonization.
formula_supplied <- argv$formula != "NULL" || argv$sigma.formula != "NULL"
if (formula_supplied && (argv$family != "combatls" || argv$diagnosis || argv$predict)) {
  stop("--formula and --sigma.formula require --family combatls --diagnosis FALSE and cannot be used with --predict TRUE.", call. = FALSE)
}
if (argv$family == "combatls" && !argv$diagnosis) {
  if (argv$predict)
    stop("Saved-model prediction is not supported for ComBatLS; use --reference instead.", call. = FALSE)
  if (argv$smooth != "NULL" || argv$random != "NULL" || argv$interaction != "NULL" || argv$int_type != "linear")
    stop("ComBatLS does not support --smooth, --interaction, --random, or --int_type; specify GAMLSS effects directly in --formula and --sigma.formula.", call. = FALSE)
  if (argv$formula == "NULL")
    stop("ComBatLS requires --formula, for example 'y ~ pb(AGE) + SEX' (or 'y ~ 1').", call. = FALSE)
}
parse_cli_formula <- function(value, option, location) {
  if (value == "NULL") return(NULL)
  formula <- tryCatch(stats::as.formula(value), error = function(e) {
    stop("Invalid ", option, ": ", conditionMessage(e), call. = FALSE)
  })
  expected_length <- if (location) 3L else 2L
  if (length(formula) != expected_length)
    stop(option, if (location) " must be a two-sided formula with response y." else
      " must be a one-sided formula, e.g. '~ AGE + SEX'.", call. = FALSE)
  if (location && !identical(formula[[2L]], quote(y)))
    stop(option, " must use y as its response.", call. = FALSE)
  formula
}
location_formula <- parse_cli_formula(argv$formula, "--formula", location = TRUE)
scale_formula <- parse_cli_formula(argv$sigma.formula, "--sigma.formula", location = FALSE)

## Checking the Data input
if(is.na(argv$data)) stop("Missing input data") else {
  if(!grepl("csv$|xls$", argv$data)) stop("Input file must be a csv or an excel file") else {
    message('Loading data...')
    if(grepl("csv$", argv$data)) df <- read_csv(argv$data) else df <- read_excel(argv$data)
  }
}

df <- data.frame(df)

## Checking data
if(!argv$predict){
## Checking the Batch input
  message('Beginning data verification...')
  if(is.na(argv$batch)) stop("Please identify the position of batch column") else {
    bat_col <- as.numeric(argv$batch)
    batch <- colnames(df)[bat_col]
    message(paste0('The provided batch column is: ', batch))
  }

  ## Checking the Features input
  if(is.na(argv$features)) stop("Please identify the position of data to be harmonized") else {
    col <- gsub("-",":",argv$features)
    col_vec <- eval(parse(text = paste0("c(", col, ")")))
    features <- colnames(df)[col_vec]
    features_wrong <- colnames(df[features])[sapply(df[features], is.character)]
    if(length(features) == 1){
      if(length(features_wrong) == 0){
        message('Only one feature needs to be harmonized, and the data type is correct!')
      }else{
        message('Only one feature needs to be harmonized, and the data type is incorrect: it is character data!')
        stop("Incorrect feature type!")
      }
    }else{
      if(length(features_wrong) == 0){
        message(paste0('There are ', length(features), ' features to be harmonized, and the data types of all the feature columns are correct!'))
      }else{
        message(paste0('There are ', length(features), ' features to be harmonized, and the data types of ', length(features_wrong), ' feature columns are incorrect: they are character data!'))
        message('The wrong features are: ', paste0(features_wrong, collapse = ","))
        stop(paste0("Incorrect feature type! Considering removing ", paste0(which(colnames(df) %in% features_wrong), collapse = ","), " column."))
      }
    }
  }

  ## Checking the Covariates input
  if(argv$covariates == "NULL") {
    cov_col <- NULL
    covariates <- NULL
  } else {
    cov_col <- gsub("-",":",argv$covariates)
    cov_col <- eval(parse(text = paste0("c(", cov_col, ")")))
    covariates <- colnames(df)[cov_col]
    if(length(covariates) == 1){
      message(paste0('The provided covariate column is: ', covariates))
    }else{
      message(paste0('The provided covariate columns are: ', paste0(covariates, collapse = ",")))
    }
  }

  ## Check formula variables before reference loading or harmonization.
  if (argv$family == "combatls" && !argv$diagnosis) {
    needed <- unique(c(
      all.vars(location_formula[[3L]]),
      all.vars(if (is.null(scale_formula)) ~ 1 else scale_formula)
    ))
    missing_vars <- setdiff(needed, covariates)
    if (length(missing_vars)) {
      stop("Formula variables missing from --covariates: ",
           paste(missing_vars, collapse = ", "), call. = FALSE)
    }
    # Function availability is resolved separately by the GAMLSS backend.
  }

  ## Checking the Smooth Terms input
  if(model_type == "gam"){
    if(argv$covariates == "NULL") stop("Please provide covariates for gam model")
    if(argv$smooth == "NULL") stop("Please provide variables that require a smoothing function") else {
      smooth_col <- gsub("-",":",argv$smooth)
      smooth_col <- eval(parse(text = paste0("c(", smooth_col, ")")))
      smooth_var <- colnames(df)[smooth_col]
      smooth <- smooth_var
      if(length(smooth) == 1){
        message(paste0('The provided smooth term is: ', smooth))
      }else{
        message(paste0('The provided smooth terms are: ', paste0(smooth, collapse = ",")))
      }
    }
  }else{
    smooth <- eval(parse(text = argv$smooth))
  }

  ## Checking the Random Effects input
  if(model_type == "lmer"){
    if(argv$random == "NULL") stop("Please specify random intercept-effects") else {
      random_col <- gsub("-",":",argv$random)
      random_col <- eval(parse(text = paste0("c(", random_col, ")")))
      random_var <- colnames(df)[random_col]
      random <- random_var
      if(length(random) == 1){
        message(paste0('The provided random effect variable is: ', random))
      }else{
        message(paste0('The provided random effect variables are: ', paste0(random, collapse = ",")))
      }
    }
  }else{
    random_col <- eval(parse(text = argv$random))
    random <- eval(parse(text = argv$random))
  }

  ## Read in the Reference Data (if it is provided)
  if(argv$reference != "NULL"){
    if(!grepl("csv$|xls$", argv$reference)) stop("Input file must be a csv or an excel file") else {
      if(grepl("csv$", argv$reference)) reference_df <- read_csv(argv$reference) else reference_df <- read_excel(argv$reference)
    }
    if(sum(!c(features, batch, covariates, random) %in% colnames(reference_df)) > 0) stop("Columns do not match between your reference dataset and the dataset to be harmonized!")
    reference_df <- reference_df[complete.cases(reference_df[c(features, batch, covariates, random)]),]
  }else{
    reference_df <- NULL
  }

  ## Interaction Wranggling
  if(argv$interaction == "NULL"){
    interaction <- eval(parse(text = argv$interaction))
    smooth_int_type <- NULL
    }else{
    interaction_l <- lapply(str_split(argv$interaction, ",")[[1]], function(x) str_split(x,  "\\*")[[1]])
    interaction <- sapply(interaction_l, function(x){
      x1 <- colnames(df)[as.numeric(x[1])]
      x2 <- colnames(df)[as.numeric(x[2])]
      element <- paste0(x1, ",", x2)
    }, USE.NAMES = FALSE)
    smooth_int_type <- str_split(argv$int_type, ",")[[1]]
    if(length(interaction) != length(smooth_int_type)) stop("Please ensure that the number of interactions matches the number of interaction types!")
    }
  message('The interaction term can be found below: ', paste0(gsub(",", "*", interaction), collapse = ","))
  message('The interaction term type can be found below: ', paste0(smooth_int_type, collapse = ","))
  object <- NULL
}else{
  message("Starting data check for out-of-sample harmonization......")
  if(argv$object == "NULL") stop("Please provide a ComBat model for out-of-sample harmonization") else {
    object <- readRDS(argv$object)
  }
}



# Batch Effect Visualization
if(argv$diagnosis){
  if(!argv$visualization){
    if(is.na(argv$outdir)) stop("Please specify a path to save the diagnosis result using --outdir!")
    if (!dir.exists(argv$outdir)) {
      stop("Error: The provided path is not a directory. Please provide a valid directory path.")
    }
  }
  message("Preparing Datasets for Batch Effect Diagnosis......")
  result <- visual_prep(type = argv$model, features = features, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, smooth_int_type = smooth_int_type, df = df,
                       cores = if(argv$cores == "all") detectCores() else as.numeric(argv$cores),
                       mdmr = argv$mdmr)
  if(argv$visualization){
    message("Datasets are ready! Starting shiny app......")
    comfam_shiny(result, argv$after)
  }else{
    diag_save(path = argv$outdir, result = result, use_quarto = argv$quarto)
  }

}else{
  message("Starting harmonization......")

  if(is.na(argv$outdir)) stop("Please specify a path to save the harmonized data using --outdir!")
  if (!grepl("\\.csv$", argv$outdir)) {
    stop("The provided file path must end with '.csv'. Please provide a valid CSV file.")
  }

  if(argv$predict){
    harm_result <- combat_harm(df = df, predict = argv$predict, object = object)
  }else{
    harm_args <- list(features = features, batch = batch, covariates = covariates,
                      df = df, type = model_type, random = random, smooth = smooth,
                      interaction = interaction, smooth_int_type = smooth_int_type,
                      family = argv$family, eb = argv$eb, robust.LS = argv$robust.LS,
                      ref.batch = if(argv$ref.batch == "NULL") NULL else argv$ref.batch,
                      predict = FALSE, object = object, reference = reference_df)
    if (!is.null(location_formula)) harm_args$formula <- location_formula
    if (!is.null(scale_formula)) harm_args$sigma.formula <- scale_formula
    harm_result <- do.call(combat_harm, harm_args)
  }

  comf_df <- harm_result$harmonized_df

  write_csv(comf_df, paste0(argv$outdir))
  message(sprintf("Results saved at %s", paste0(argv$outdir)))

  if(!is.na(argv$mout)){
    if (!grepl("\\.rds$", argv$mout, ignore.case = TRUE)) {
      stop("Error: Please provide a path ending with '.rds' for the output file.")
    }
    message("Saving ComBat model......")
    if(harm_result$com_family %in% c("comfam", "combatls")){
      harm_result$combat.object$ComBat.model$dat.combat <- NULL
    }else{
      harm_result$combat.object$ComBat.model$dat.covbat <- NULL
    }
    saveRDS(harm_result$combat.object, argv$mout)
    message(sprintf("ComBat model saved at %s", argv$mout))
  }
}

