suppressMessages(library(argparser))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(ComBatFamQC))
suppressMessages(library(parallel))

## Read in arguments
p <- arg_parser("Post-harmonization processing step", hide.opts = FALSE)
p <- add_argument(p, "data", help = "path to the CSV or EXCEL file that contains data to be harmonized, covariates and batch information")
p <- add_argument(p, "--type", short = '-t', help = "post-harmonization processing type, eg: residual or age_trend", default = "age_trend")
p <- add_argument(p, "--visualization", short = '-v', help = "a boolean variable indicating whether to start the Shiny app for age trend visualization. TRUE indicates that the Shiny app will be started. False indicates that the age trend estimates will be saved to a CSV file.", default = TRUE)
p <- add_argument(p, "--features", short = '-f', help = "position of features/rois data(column numbers), eg: 1-5,9")
p <- add_argument(p, "--AGE", help = "column position of the age variable")
p <- add_argument(p, "--SEX", help = "column position of the sex variable")
p <- add_argument(p, "--ICV", help = "column position of the ICV variable")
p <- add_argument(p, "--Female", help = "female indicator, the value represent female in sex column.")
p <- add_argument(p, "--lowerquantile", short = '-l', help = "Specify a lower bound quantile. eg: 0.05, 0.25.", default = 0.25)
p <- add_argument(p, "--upperquantile", short = '-u', help = "Specify a upper bound quantile. eg: 0.75, 0.95.", default = 0.75)
p <- add_argument(p, "--Mu", help = "An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the mu formula. smooth: y ~ pb(age), linear: y ~ age, default: y ~ 1.", default = "smooth")
p <- add_argument(p, "--Sigma", help = "An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the sigma formula. smooth: ~ pb(age), linear: ~ age, default: ~ 1.", default = "smooth")
p <- add_argument(p, "--Nu", help = "An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the nu formula. smooth: ~ pb(age), linear: ~ age, default: ~ 1.", default = "default")
p <- add_argument(p, "--Tau", help = "An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the tau formula. smooth: ~ pb(age), linear: ~ age, default: ~ 1.", default = "default")
p <- add_argument(p, "--covariates", short = '-c', help = "position of covariates (column numbers)", default = "NULL")
p <- add_argument(p, "--model", short = '-m', help = "select the model function for harmonization, eg: lm, gam", default = "lm")
p <- add_argument(p, "--smooth", short = '-s', help = "provide the variables that require a smooth function", default = "NULL")
p <- add_argument(p, "--interaction", help = "specify the interaction terms in the format of col_index1*col_index2, eg 2*3,11*12", default = "NULL")
p <- add_argument(p, "--int_type", help = "specify an interaction type for gam models, eg: linear, factor-smooth, tensor", default = "linear")
p <- add_argument(p, "--random", short = '-r', help = "specify the random intercept-effects", default = "NULL")
p <- add_argument(p, "--rm", help = "specify the covariates (column numbers) to remove effects from, eg: 1-5,9", default = "NULL")
p <- add_argument(p, "--exist.model", help = "A boolean variable indicating whether an existing model is to be used", default = FALSE)
p <- add_argument(p, "--model.path", help = "path to the existing model", default = "NULL")
p <- add_argument(p, "--outdir", short = '-o', help = "full path (including the file name) where residual data should be written, or the directory path to save the age trend table.")
p <- add_argument(p, "--mout", help = "full path where regression models to be saved")
p <- add_argument(p, "--cores", help = "number of cores used for paralleling computing, please provide a numeric value", default = "all")
argv <- parse_args(p)

# Preprocess inputs
message('Checking inputs...')
if(is.na(argv$data)) stop("Missing input data") else {
  if(!grepl("csv$|xls$", argv$data)) stop("Input file must be a csv or an excel file") else {
    if(grepl("csv$", argv$data)) df <- read.csv(argv$data) else df <- read_excel(argv$data)
  }
}
df <- data.frame(df)

if (argv$cores == "all"){
  cores <- detectCores()
}else{
  cores <- as.numeric(argv$cores)
}

if(argv$type == "age_trend"){
  if(is.na(argv$features)) stop("Please identify the position of features/rois.") else {
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

  if(is.null(argv$AGE)) stop("Please provide the age column position!")
  if(is.null(argv$SEX)) stop("Please provide the sex column position!")
  if(is.null(argv$ICV)) stop("Please provide the ICV column position!")
  if(is.null(argv$Female)) stop("Please specify how 'female' is represented in the sex column!")
  if(!argv$visualization){
    if(is.na(argv$outdir)) stop("Please specify a path to save the age trend table using --outdir!")
    if (!dir.exists(argv$outdir)) {
      stop("Error: The provided path is not a directory. Please provide a valid directory path.")
    }
  }
  message("Start preparing datasets for age trend estimation ....")
  age <- colnames(df)[as.numeric(argv$AGE)]
  sex <- colnames(df)[as.numeric(argv$SEX)]
  icv <- colnames(df)[as.numeric(argv$ICV)]
  message(paste0('The provided age, sex, and ICV columns are: ', paste0(c(age, sex, icv), collapse = ",")))
  df[[sex]] <- as.factor(df[[sex]])
  df[[sex]] <- sapply(df[[sex]], function(x){
    if(x == argv$Female){return("F")}else{return("M")}
  }, USE.NAMES = FALSE)
  message("Create a list of sub_df objects ....")
  # Create sub_df for different features

  sub_df_list <- lapply(seq_len(length(features)), function(i){
    sub_df <- df[,c(features[i], age, sex, icv)] %>% na.omit()
    colnames(sub_df) <- c(features[i], "age", "sex", "icv")
    return(sub_df)
  })

  # Create age_list
  age_list <- mclapply(seq_len(length(features)), function(w){
    age_sub <- age_list_gen (sub_df = sub_df_list[[w]],  lq = as.numeric(argv$lowerquantile), hq = as.numeric(argv$upperquantile), mu = argv$Mu, sigma = argv$Sigma, nu = argv$Nu, tau = argv$Tau)
    return(age_sub)
  }, mc.cores = cores)

  names(age_list) <- features

  quantile_type <- c(paste0("quantile_", 100*as.numeric(argv$lowerquantile)), "median", paste0("quantile_", 100*as.numeric(argv$upperquantile)))

  if(argv$visualization){
    ComBatFamQC::age_shiny(age_list, features, quantile_type)
  }else{
    age_save(path = argv$outdir, age_list = age_list)
    if(!is.na(argv$mout)) {
      if (!grepl("\\.rds$", argv$mout, ignore.case = TRUE)) {
        stop("Error: Please provide a path ending with '.rds' for the output file.")
      }

      gamlss_model <- lapply(seq_len(length(age_list)), function(i){
        g_model <- age_list[[i]]$model
        return(g_model)})
      names(gamlss_model) <- names(age_list)
      saveRDS(gamlss_model, file = argv$mout)
    }
  }
}else if(argv$type == "residual"){

  if(is.na(argv$outdir)) stop("Please specify a path to save the residual dataset using --outdir!")
  if (!grepl("\\.csv$", argv$outdir)) {
    stop("The provided file path must end with '.csv'. Please provide a valid CSV file.")
  }

  if(!argv$exist.model){

    if(is.na(argv$features)) stop("Please identify the position of features/rois.") else {
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

    if(argv$model == "gam"){
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

    if(argv$model == "lmer"){
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
  }else{
    message("Starting data check for out-of-sample residual generation ......")
    if(argv$model.path == "NULL") stop("Please provide a regression model for out-of-sample residual generation") else {
      if (!grepl("\\.rds$", argv$model.path, ignore.case = TRUE)) {
        stop("Error: Please provide a path ending with '.rds'.")
      }
    }
  }

  if(argv$rm == "NULL"){
    rm <- NULL
  }else{
    rm_col <- gsub("-",":",argv$rm)
    rm_col <- eval(parse(text = paste0("c(", rm_col, ")")))
    rm <- colnames(df)[rm_col]
    message(paste0('The covariates from which effects need to be removed are: ', paste0(rm, collapse = ",")))
  }

  # Generate residuals
  if(!argv$exist.model){
    result <- residual_gen(type = argv$model, features = features, covariates = covariates, interaction = interaction, smooth = smooth, smooth_int_type = smooth_int_type, random = random, df = df, rm = rm, model = argv$exist.model, model_path = argv$model.path, cores = cores)
  }else{
    result <- residual_gen(model = TRUE, model_path = argv$model.path, df = df, rm = rm, cores = cores)
  }


  message("Saving residual data......")
  write_csv(result$residual, argv$outdir)
  message(sprintf("Results saved at %s", argv$outdir))


  if(!is.na(argv$mout)){
    if (!grepl("\\.rds$", argv$mout, ignore.case = TRUE)) {
      stop("Error: Please provide a path ending with '.rds' for the output file.")
    }
    message("Saving model......")
    saveRDS(result$model, argv$mout)
    message(sprintf("Model saved at %s", argv$mout))
  }
}

