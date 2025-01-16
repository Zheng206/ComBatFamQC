#' Data Preparation
#'
#' Prepares the dataset for effective use in batch effect diagnostics, harmonization, and post-harmonization downstream analysis processes within the `ComBatFamQC` package.
#'
#' @param stage Specifies the stage of analysis for which the data preparation is intended: harmonization or residual.
#' @param result A list derived from `visual_prep()` that contains dataset and batch effect diagnostic information for Shiny visualization. Can be skipped if `features`, `batch`, `covariates` and `df` are provided.
#' @param features The name of the features to be harmonized. This can be skipped if `result` is provided.
#' @param batch The name of the batch variable. Can be skipped if `result` is provided.
#' @param covariates The names of covariates supplied to `model`. This can be be skipped if `result` is provided.
#' @param df The dataset to be harmonized. This can be be skipped if `result` is provided.
#' @param type The name of a regression model to be used in batch effect diagnostics, harmonization, and the post-harmonization stage: "lmer", "lm", "gam".
#' @param random The variable name of a random effect in linear mixed effect model.
#' @param smooth The name of the covariates that require a smooth function.
#' @param interaction Expression of interaction terms supplied to `model` (eg: "age,diagnosis").
#' @param smooth_int_type A vector that indicates the types of interaction in `gam` models. By default, smooth_int_type is set to be NULL, "linear" represents linear interaction terms.
#' "categorical-continuous", "factor-smooth" both represent categorical-continuous interactions ("factor-smooth" includes categorical variable as part of the smooth),
#' "tensor" represents interactions with different scales, and "smooth-smooth" represents interaction between smoothed variables.
#' @param predict A boolean variable indicating whether to run ComBat from scratch or apply existing model to new dataset (currently only work for "original ComBat" and "ComBat-GAM").
#' @param object Existing ComBat model.
#'
#' @return `data_prep` returns a list containing the processed data and parameter-related information for batch effect diagnostics, harmonization, and post-harmonization downstream analysis.
#'
#' @import dplyr
#' @import magrittr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom  stats complete.cases
#'
#' @export
#'
#' @examples
#' data_prep(stage = "harmonization", result = NULL, features = colnames(adni)[43:53],
#' batch = "manufac", covariates = "AGE", df = head(adni, 100), type = "lm", random = NULL,
#' smooth = NULL, interaction = NULL, smooth_int_type = NULL, predict = FALSE, object = NULL)
#'


data_prep <- function(stage = "harmonization", result = NULL, features = NULL, batch = NULL, covariates = NULL, df = NULL, type = "lm", random = NULL, smooth = NULL, interaction = NULL, smooth_int_type = NULL, predict = FALSE, object = NULL){
  if(stage == "harmonization"){
    message("Starting data preparation for the batch effect diagnostic and harmonization stage...")
    ## When the result from the visual preparation stage is provided
    if(!is.null(result)){
      message("Taking the result from the visual preparation stage as input...")
      df <- result$info$df
      char_var <- result$info$char_var
      batch <- result$info$batch
      features <- result$info$features
      cov_shiny <- result$info$cov_shiny
      obs_n <- nrow(df)
      df <- df[complete.cases(df[c(features, batch, cov_shiny, random)]),]
      obs_new <- nrow(df)
      if((obs_n - obs_new) != 0){
        print(paste0(obs_n - obs_new, " observation(s) are dropped due to missing values."))
      }else{
        print("No observation is dropped due to missing values.")
      }
      df[[batch]] <- as.factor(df[[batch]])
      df[char_var] <- lapply(df[char_var], as.factor)
      summary_df <- result$info$summary_df
    }else{
      ## When the result from the visual preparation is not provided
      if(!predict){
        message("The result from the visual prepration stage is not provided! The required parameters should be specified...")
        if(is.null(features) || is.null(batch) || is.null(df)) stop("Please identify the features, batch, and df parameter!")
        }else{
        if(is.null(object)) stop("Please provide the saved ComBat model!")
        batch <- object$batch.name
        eb <- object$eb
        model_type <- class(object$ComBat.model$fits[[1]])[1]
        features <- colnames(object$ComBat.model$estimates$stand.mean)
        form_str <- as.character(formula(object$ComBat.model$fits[[1]]))[3]
        if(model_type == "lm"){
          covariates <- strsplit(form_str, "\\+")[[1]][which(!grepl("batch|:", strsplit(form_str, "\\+")[[1]]))]
          covariates <- sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          random <- NULL
          type <- "lm"
        }else if(model_type == "lmerMod"){
          covariates <- strsplit(form_str, "\\+")[[1]][which(!grepl("batch|:|\\(1", strsplit(form_str, "\\+")[[1]]))]
          covariates <- sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          random <- strsplit(form_str, "\\+")[[1]][which(grepl("\\(1", strsplit(form_str, "\\+")[[1]]))]
          random <- sapply(random, function(x) gsub(" ", "", gsub("\\)", "", strsplit(x, "\\|")[[1]][2])), USE.NAMES = FALSE)
          type <- "lmer"
        }else if(model_type == "gam"){
          covariates <- strsplit(form_str, "\\+")[[1]][which(!grepl("batch|:|s\\(|ti\\(", strsplit(form_str, "\\+")[[1]]))]
          covariates <- sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          smooth_term <- strsplit(form_str, "\\+")[[1]][which(grepl("s\\(|ti\\(", strsplit(form_str, "\\+")[[1]]))]
          smooth_term <- lapply(smooth_term, function(x) strsplit(x, ",")[[1]]) |> unlist()
          smooth_term <- smooth_term[which(!grepl("by =|bs =", smooth_term))]
          smooth_term <- sapply(smooth_term, function(x) trimws(gsub(" s\\(|ti\\(|\\)", "", x)), USE.NAMES = FALSE)
          covariates <- c(covariates, smooth_term) |> unique()
          random <- NULL
          type <- "gam"
        }
        if(sum(!c(covariates, random, features) %in% colnames(df)) > 0) stop("Columns do not match!")
      }
      obs_n <- nrow(df)
      df <- df[complete.cases(df[c(features, batch, covariates, random)]),]
      obs_new <- nrow(df)
      if((obs_n - obs_new) != 0){
        print(paste0(obs_n - obs_new, " observation(s) are dropped due to missing values."))
      }else{
        print("No observation is dropped due to missing values.")
      }
      df[[batch]] <- as.factor(df[[batch]])
      char_var <- covariates[sapply(df[covariates], function(col) is.character(col) || is.factor(col))]
      enco_var <- covariates[sapply(df[covariates], function(col) length(unique(col)) == 2 && all(unique(col) %in% c(0,1)))]
      df[char_var] <- lapply(df[char_var], as.factor)
      df[enco_var] <- lapply(df[enco_var], as.factor)
      cov_shiny <- covariates
      char_var <- c(char_var, enco_var)

      # Summary
      summary_df <- df %>% group_by(.data[[batch]]) %>% summarize(count = n(), percentage = 100 * n() / nrow(df))
      colnames(summary_df) <- c(batch, "count", "percentage (%)")
      summary_df <- summary_df %>% mutate(remove = case_when(.data[["count"]] < 3 ~ "removed",
                                                            .default = "keeped"))
      batch_rm <- summary_df %>% filter(.data[["remove"]] == "removed") %>% pull(.data[[batch]]) %>% droplevels()
      if(length(batch_rm) > 0){
        print(paste0("Batch levels that contain less than 3 observations are dropped: ", length(batch_rm), " level(s) are dropped, corresponding to ", df %>% filter(.data[[batch]] %in% batch_rm) %>% nrow(), " observations."))
      }else{print("Batch levels that contain less than 3 observations are dropped: no batch level is dropped.")}
      df <- df %>% filter(!.data[[batch]] %in% batch_rm)
      df[[batch]] <- df[[batch]] %>% droplevels()
    }

    if(!is.null(random)){
      for (r in random){
        df[[r]] <- as.factor(df[[r]])
      }
    }

    ## drop univariate features
    features_orig <- df[features]
    n_orig <- length(colnames(features_orig))
    features_new <- features_orig[, apply(features_orig, 2, function(col) { length(unique(col)) > 1 })]
    n_new <- length(colnames(features_new))
    dropped_col <- NULL
    if (n_orig > n_new){
      dropped_col <- setdiff(colnames(features_orig), colnames(features_new))
      print(paste0(n_orig - n_new, " univariate feature column(s) are dropped: ", dropped_col))
    }
    features <- colnames(features_new)

    int_result <- interaction_gen(type = type, covariates = cov_shiny, interaction = interaction, smooth = smooth, smooth_int_type = smooth_int_type)
    interaction_orig <- interaction
    smooth_orig <- smooth
    covariates <- int_result$covariates
    interaction <- int_result$interaction
    smooth <- int_result$smooth

    return(list("batch" = batch, "features" = features, "type" = type, "covariates" = covariates, "interaction" = interaction, "random" = random, "smooth" = smooth, "df" = df, "cov_shiny" = cov_shiny, "char_var" = char_var, "smooth_int_type" = smooth_int_type, "interaction_orig" = interaction_orig, "smooth_orig" = smooth_orig, "summary_df" = summary_df))
  }else{
    message("Starting data preparation for the post-harmonization stage...")
    if(!predict){
      message("No existing model is provided. Fitting the regression model from scratch!")
      if(is.null(features)) stop("Please identify the features required to generate residuals!")
      }else{
        if(is.null(object)) stop("Please provide the saved model!")
        model_type <- class(object[[1]])[1]
        form_str <- as.character(formula(object[[1]]))[3]
        features <- lapply(seq_len(length(object)), function(i) as.character(formula(object[[i]]))[2]) |> unlist()
        if(model_type == "lm"){
          covariates <- strsplit(form_str, "\\+")[[1]][which(!grepl(":", strsplit(form_str, "\\+")[[1]]))]
          covariates <- sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          random <- NULL
          type <- "lm"
        }else if(model_type == "lmerMod"){
          covariates <- strsplit(form_str, "\\+")[[1]][which(!grepl(":|\\(1", strsplit(form_str, "\\+")[[1]]))]
          covariates <- sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          random <- strsplit(form_str, "\\+")[[1]][which(grepl("\\(1", strsplit(form_str, "\\+")[[1]]))]
          random <- sapply(random, function(x) gsub(" ", "", gsub("\\)", "", strsplit(x, "\\|")[[1]][2])), USE.NAMES = FALSE)
          type <- "lmer"
        }else if(model_type == "gam"){
          covariates <- strsplit(form_str, "\\+")[[1]][which(!grepl(":|s\\(|ti\\(", strsplit(form_str, "\\+")[[1]]))]
          covariates <- sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          smooth_term <- strsplit(form_str, "\\+")[[1]][which(grepl("s\\(|ti\\(", strsplit(form_str, "\\+")[[1]]))]
          smooth_term <- lapply(smooth_term, function(x) strsplit(x, ",")[[1]]) |> unlist()
          smooth_term <- smooth_term[which(!grepl("by =|bs =", smooth_term))]
          smooth_term <- sapply(smooth_term, function(x) trimws(gsub(" s\\(|ti\\(|\\)", "", x)), USE.NAMES = FALSE)
          covariates <- c(covariates, smooth_term) |> unique()
          random <- NULL
          type <- "gam"
        }
    }
    obs_n <- nrow(df)
    df <- df[complete.cases(df[c(features, covariates, random)]),]
    obs_new <- nrow(df)
    if((obs_n - obs_new) != 0){
      print(paste0(obs_n - obs_new, " observation(s) are dropped due to missing values."))
    }else{
      print("No observation is dropped due to missing values.")
    }
    char_var <- covariates[sapply(df[covariates], function(col) is.character(col) || is.factor(col))]
    enco_var <- covariates[sapply(df[covariates], function(col) length(unique(col)) == 2 && all(unique(col) %in% c(0,1)))]
    df[char_var] <-  lapply(df[char_var], as.factor)
    df[enco_var] <-  lapply(df[enco_var], as.factor)
    cov_shiny <- covariates
    char_var <- c(char_var, enco_var)

    if(!is.null(random)){
      for (r in random){
        df[[r]] <- as.factor(df[[r]])
      }
    }

    ## drop univariate features
    features_orig <- df[features]
    n_orig <- length(colnames(features_orig))
    features_new <- features_orig[, apply(features_orig, 2, function(col) { length(unique(col)) > 1 })]
    n_new <- length(colnames(features_new))
    dropped_col <- NULL
    if (n_orig > n_new){
      dropped_col <- setdiff(colnames(features_orig), colnames(features_new))
      print(paste0(n_orig - n_new, " univariate feature column(s) are dropped: ", dropped_col))
    }

    features <- colnames(features_new)

    ## generate interactions
    int_result <- interaction_gen(type = type, covariates = cov_shiny, interaction = interaction, smooth = smooth, smooth_int_type = smooth_int_type)
    interaction_orig <- interaction
    smooth_orig <- smooth
    covariates <- int_result$covariates
    interaction <- int_result$interaction
    smooth <- int_result$smooth
    return(list("features" = features, "type" = type, "covariates" = covariates, "interaction" = interaction, "random" = random, "smooth" = smooth, "df" = df, "cov_shiny" = cov_shiny, "char_var" = char_var, "smooth_int_type" = smooth_int_type, "interaction_orig" = interaction_orig, "smooth_orig" = smooth_orig))
  }
}


#' EB Assumption Check
#'
#' Generate the empirical and prior distribution of both the location parameter `gamma` and the scale parameter `delta`.
#'
#' @param data \emph{n x p} data frame or matrix of observations where
#'   \emph{p} is the number of features and \emph{n} is the number of subjects.
#' @param bat Factor indicating batch (often equivalent to site or scanner).
#' @param covar Data frame or matrix of covariates supplied to `model`.
#' @param model The model function that ComBat Family supports: `lm`, `lmer`, `gam`.
#' @param formula Formula for `model` starting with `y ~` where `y` represents each feature.
#' @param robust.LS If \code{TRUE}, uses robust location and scale estimators for error variance and site effect parameters. Currently uses median and
#'   biweight midvariance.
#' @param ref.batch Reference batch, must take value in `levels(bat)`.
#' @param ... Additional arguments to `model`.
#'
#' @return `eb_check` returns a dataframe containing the empirical and prior distribution of both the location parameter (gamma) and the scale parameter (delta).
#'
#' @importFrom methods hasArg
#' @importFrom invgamma rinvgamma
#' @importFrom stats rnorm
#'
#' @export
#'
#' @examples
#' eb_check(data = adni[1:500,43:53], bat = as.factor(adni$manufac[1:500]),
#' covar = adni[1:500, c("AGE", "SEX")], model = lm, formula = y ~ AGE + SEX)
#'


eb_check <- function(data, bat, covar = NULL, model = lm, formula = NULL, robust.LS = FALSE, ref.batch = NULL, ...){
  data <- as.matrix(data)
  n <- nrow(data)
  p <- ncol(data)

  if (p == 1) {
    warning("EB step skipped for univariate data.")
  }

  if (!is.null(ref.batch)) {
    if (!(ref.batch %in% levels(bat))) {
      stop("Reference batch must be in the batch levels")
    }
    ref <- bat == ref.batch
    nref <- sum(ref)
  }

  bat <- droplevels(bat)
  batch <- model.matrix(~ -1 + bat)
  batches <- lapply(levels(bat), function(x) which(bat == x))
  n_batches <- sapply(batches, length)

  if (robust.LS) {
    loc <- median
    scl <- .biweight_midvar
  } else {
    loc <- mean
    scl <- var
  }

  if (is.null(covar)) {
    mod <- data.frame(I(batch))
  } else {
    mod <- data.frame(covar, I(batch))
  }

  if (is.null(covar) | is.null(formula)) {
    formula <- y ~ 1
  }

  # case when using nlme::lme
  if (hasArg("fixed")) {
    fits <- apply(data, 2, function(y) {
      dat <- data.frame(y = y, mod)

      addargs <- list(...)
      addargs$fixed <- NULL
      bat_formula <- update(formula, ~ . + batch + -1)
      do.call(model, c(list(fixed = bat_formula, data = dat), addargs))
    })
  } else {
    fits <- apply(data, 2, function(y) {
      dat <- data.frame(y = y, mod)
      bat_formula <- update(formula, ~ . + batch + -1)
      do.call(model, list(formula = bat_formula, data = dat, ...))
    })
  }

  #### Standardize the data ####
  # Model matrix for obtaining pooled mean
  pmod <- mod
  pmod$batch[] <- matrix(n_batches/n, n, nlevels(bat), byrow = TRUE)

  # Reference batch
  if (!is.null(ref.batch)) {
    pmod$batch[] <- 0
    pmod$batch[,which(levels(bat) == ref.batch)] <- 1
  }

  stand_mean <- sapply(fits, predict, newdata = pmod, type = "response")
  resid_mean <- sapply(fits, predict, newdata = mod, type = "response")

  if (!is.null(ref.batch)) {
    var_pooled <- apply((data - resid_mean)[ref, , drop = FALSE], 2, scl) *
      (nref - 1)/nref
  } else {
    var_pooled <- apply(data - resid_mean, 2, scl) * (n - 1)/n
  }

  if (hasArg("sigma.formula")) {
    sd_mat <- sapply(fits, predict, newdata = pmod, what = "sigma",
                    type = "response")
  } else {
    sd_mat <- sapply(sqrt(var_pooled), rep, n)
  }

  data_stand <- (data-stand_mean)/sd_mat

  #### Obtain location and scale adjustments ####
  gamma_hat <- Reduce(rbind, by(data_stand, bat, function(x) apply(x, 2, loc)))
  delta_hat <- Reduce(rbind, by(data_stand, bat, function(x) apply(x, 2, scl)))

  rownames(gamma_hat) <- rownames(delta_hat) <- levels(bat)

  # Empirical Bayes adjustments

  eb_df <- lapply(1:nlevels(bat), function(i){
    n_b <- n_batches[i]

    # method of moments estimates
    g_bar <- mean(gamma_hat[i,])
    g_var <- var(gamma_hat[i,])

    d_bar <- mean(delta_hat[i,])
    d_var <- var(delta_hat[i,])

    d_a <- (2 * d_var + d_bar^2)/d_var
    d_b <- (d_bar * d_var + d_bar^3)/d_var

    # generate prior distribution
    g_prior <- rnorm(length(gamma_hat[i,]), g_bar, g_var)
    d_prior <- rinvgamma(length(gamma_hat[i,]), d_a, d_b)
    eb_df <- data.frame(cbind("batch" = rep(rownames(gamma_hat)[i], length(gamma_hat[i,])), "features" = names(gamma_hat[i,]), "gamma_hat" = gamma_hat[i,], "gamma_prior" = g_prior, "delta_hat" = delta_hat[i,], "delta_prior" = d_prior))
    return(eb_df)
  }) %>% bind_rows() %>% pivot_longer(cols = c(3:6), names_to = "type", values_to = "eb_values") %>% mutate(eb_values = as.numeric(.data[["eb_values"]]),
                                                                                                            batch = as.factor(.data[["batch"]]))
  return(eb_df)
}

#' Biweight Midvariance Calculation
#'
#' Compute a robust estimate of midvariance using the biweight method, which reduces the influence of outliers by applying a weighting function to the data based on their deviation from a central value.
#'
#' @param data A numeric vector containing the data points for which the biweight midvariance is to be calculated.
#' @param center An optional parameter specifying the central location of the data. If not provided, the function defaults to using the median of the data.
#' @param norm.unbiased A logical parameter (default: `TRUE`) indicating whether to use a normalization constant for unbiased estimation. When `TRUE`, the constant is adjusted to 9 divided by the quantile function of 0.75 from the standard normal distribution.
#'
#' @return A numeric value representing the robust biweight midvariance estimate.
#'
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4, 100)
#' biweight_var <- .biweight_midvar(data)
#' print(biweight_var)

.biweight_midvar <- function(data, center=NULL, norm.unbiased = TRUE) {
  if (is.null(center)) {
    center <- median(data)
  }

  mad <- median(abs(data - center))
  d <- data - center
  c <- ifelse(norm.unbiased, 9/qnorm(0.75), 9)
  u <- d/(c*mad)

  n <- length(data)
  indic <- abs(u) < 1

  num <- sum(indic * d^2 * (1 - u^2)^4)
  dem <- sum(indic * (1 - u^2) * (1 - 5*u^2))^2

  return(n * num/dem)
}


#' Model Generations
#'
#' Generate appropriate regression models based on the model type and formula
#'
#' @param y Dependent variable in the model.
#' @param type A model function name that is used or to be used in the ComBatFamily Package (eg: "lmer", "lm", "gam").
#' @param batch Name of batch variable (often equivalent to site or scanner).
#' @param covariates Name of covariates supplied to `model`.
#' @param interaction Expression of interaction terms supplied to `model` (eg: "age:diagnosis").
#' @param random Variable name of a random effect in linear mixed effect model.
#' @param smooth Variable name that requires a smooth function.
#' @param df Dataset to be harmonized.
#'
#' @return A regression model object to be used for batch effect diagnostics and the post-harmonization stage.
#'
#' @export
#'
#' @examples
#' model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "lm",
#' batch = "manufac", covariates = c("AGE", "SEX"), df = adni)
#'

model_gen <- function(y, type = "lm", batch = NULL, covariates = NULL, interaction = NULL, random = NULL, smooth = NULL, df){
  if(!is.null(batch)){
    if(type == "lmer"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model <- lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", batch, " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)
        }else{
          model <- lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch, " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
      }else{model <- lmer(as.formula(paste0(y, " ~ ", batch, " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
    }else if(type == "lm"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model <- lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", batch)), data = df)
        }else{
          model <- lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch)), data = df)}
      }else{
        model <- lm(as.formula(paste0(y, " ~ ", batch)), data = df)
      }
    }else if(type == "gam"){
      if(is.null(interaction)){
        model <- gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", batch)), data = df)
      }else{
        if(length(smooth) > 0){
          model <- gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch)), data = df)
        }else{
          model <- gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch)), data = df)
        }
      }
    }
  }else{
    if(type == "lmer"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model <- lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)
        }else{
          model <- lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
      }else{model <- lmer(as.formula(paste0(y, " ~ ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
    }else if(type == "lm"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model <- lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "))), data = df)
        }else{
          model <- lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "))), data = df)}
      }else{
        model <- lm(as.formula(paste0(y, " ~ 1")), data = df)
      }
    }else if(type == "gam"){
      if(is.null(interaction)){
        model <- gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "))), data = df)
      }else{
        if(length(smooth) > 0){
          model <- gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", paste(interaction, collapse = " + "))), data = df)
        }else{
          model <- gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "))), data = df)
        }
      }
    }
  }
  return(model)
}

#' ComBatFamily Model Formula Generations
#'
#' Generate appropriate formula for ComBatFamily models
#'
#' @param x A model function name that is used or to be used in the ComBatFamily Package (eg: "lmer", "lm", "gam").
#' @param c Data frame or matrix of covariates supplied to `model`.
#' @param i Expression of interaction terms supplied to `model`. (eg: "age:diagnosis").
#' @param random Variable name of a random effect in linear mixed effect model.
#' @param smooth Variable name that requires a smooth function.
#'
#' @return A string of formula
#'
#' @export
#'
#' @examples
#' covariates <- adni[, c("AGE", "SEX")]
#' form_gen(x = "lm", c = covariates)
#'


form_gen <- function(x, c = NULL, i = NULL, random = NULL, smooth = NULL){
  if (x == "lm"){
    if(!is.null(c)){
      if (is.null(i)){form = paste0("y ~", paste(colnames(c), collapse = "+"))}else{
        form <- paste0("y ~", paste(colnames(c), collapse = "+"),  " + ", paste(i, collapse = " + "))
      }
    }else{form <- NULL}
  }else if (x == "lmer"){
    if(!is.null(c)){
      if (is.null(i)){form <- paste0("y ~", paste(colnames(c), collapse = " + "),  " + ", paste("(1 |", random, ")", collapse = " + "))}else{
        form <- paste0("y ~", paste(colnames(c), collapse = " + "),  " + ", paste(i, collapse = " + "), " + ", paste("(1 |", random, ")", collapse = " + "))
      }
    }else{form = paste0("y ~", paste("(1 |", random, ")", collapse = " + "))}
  }else if(x == "gam"){
    if (is.null(i)){form <- paste0("y ~ ", paste(colnames(c), collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "))}else{
      if(length(smooth) > 0){
        form <- paste0("y ~ ", paste(colnames(c), collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", paste(i, collapse = " + "))
      }else{
        form <- paste0("y ~ ", paste(colnames(c), collapse = " + "), " + ", paste(i, collapse = " + "))
      }
    }
  }
  return(form)
}


#' Interaction Term Generation
#'
#' Generate appropriate interaction terms for regression models.
#'
#' @param type The type of model to be used for batch effect evaluation or harmonization (eg: "lmer", "lm", "gam").
#' @param covariates Name of covariates supplied to `model`.
#' @param smooth Variable names that require a smooth function.
#' @param interaction Expression of interaction terms supplied to `model` (eg: "age,diagnosis").
#' @param smooth_int_type A vector that indicates the types of interaction in `gam` models. By default, smooth_int_type is set to be NULL, "linear" represents linear interaction terms.
#' "categorical-continuous", "factor-smooth" both represent categorical-continuous interactions ("factor-smooth" includes categorical variable as part of the smooth),
#' "tensor" represents interactions with different scales, and "smooth-smooth" represents interaction between smoothed variables.
#'
#' @return `interaction_gen` returns a list containing the following components:
#' \item{interaction}{A vector of interaction terms to be included}
#' \item{covariates}{Modified covariates after expressing interaction terms}
#' \item{smooth}{Modified smooth terms after expressing interaction terms}
#'
#' @export
#'
#' @examples
#' interaction_gen(type = "lm", covariates = c("AGE", "SEX", "DIAGNOSIS"),
#' interaction = "AGE,DIAGNOSIS")
#'
#' interaction_gen(type = "gam", covariates = c("AGE", "SEX", "DIAGNOSIS"),
#' smooth = "AGE", smooth_int_type = "linear", interaction = "AGE,DIAGNOSIS")
#'



interaction_gen <- function(type = "lm", covariates = NULL, smooth = NULL, interaction = NULL, smooth_int_type = NULL){
  if(!is.null(interaction)){
    if(type == "gam"){
      if (length(interaction) != length(smooth_int_type)) {
        stop("The lengths of 'interaction' and 'smooth_int_type' must match.")
      }
      covariates <- setdiff(covariates, smooth)
      inter_gen <- function(interaction, smooth, covariates, x){
        if(x == "linear"){
          interaction <- gsub(",", ":", interaction)
          smooth_rm <- NULL
          covariate_rm <- NULL
        }else if(x == "categorical-continuous"){
          element <- strsplit(interaction,",")[[1]]
          smooth_element <- element[which(element %in% smooth)]
          categorical_element <- setdiff(element, smooth_element)
          interaction <- paste0("s(", smooth_element, ", by = ", categorical_element, ")")
          smooth_rm <- smooth_element
          covariate_rm <- NULL
        }else if(x == "factor-smooth"){
          element <- strsplit(interaction,",")[[1]]
          interaction <- paste("s(", interaction, ", bs = 'fs')")
          smooth_element <- element[which(element %in% smooth)]
          categorical_element <- setdiff(element, smooth_element)
          smooth_rm <- smooth_element
          covariate_rm <- categorical_element
        }else if(x == "tensor"){
          interaction <- paste("ti(", interaction, ")")
          smooth_rm <- NULL
          covariate_rm <- NULL
        }else if(x == "smooth-smooth"){
          element <- strsplit(interaction,",")
          interaction <- paste("s(", interaction, ")")
          smooth_rm <- element[[1]]
          covariate_rm <- NULL
        }
        element_result <- list(interaction = interaction, smooth_rm = smooth_rm, covariate_rm = covariate_rm)
        return(element_result)
      }
      interaction_after <- lapply(1:length(interaction), function(i) inter_gen(interaction[i], smooth, covariates, x = smooth_int_type[i])$interaction) |> unlist()
      smooth_rm<- lapply(1:length(interaction), function(i) inter_gen(interaction[i], smooth, covariates, x = smooth_int_type[i])$smooth_rm) |> unlist()
      covariate_rm <- lapply(1:length(interaction), function(i) inter_gen(interaction[i], smooth, covariates, x = smooth_int_type[i])$covariate_rm) |> unlist()
      smooth_after <- smooth[which(!smooth %in% smooth_rm)]
      if(length(smooth_after)==0){smooth <- NULL}else{smooth <- smooth_after}
      if(length(covariates[which(!covariates %in% covariate_rm)]) == 0){covariates <- NULL}else{covariates <- covariates[which(!covariates %in% covariate_rm)]}
    }else{
      interaction_after <- gsub(",", ":", interaction)
      smooth <- smooth
      covariates <- covariates
    }
  }else{
    interaction_after <- NULL
    smooth <- smooth
    covariates <- setdiff(covariates, smooth)
  }
  int_result <- list("interaction" = interaction_after, "covariates" = covariates, "smooth" = smooth)
  return(int_result)
}

#' Export Batch Effect Diagnosis Results
#'
#' Save all the batch effect diagnosis results in a single Excel file or a Quarto report.
#'
#' @param path The path to save the result.
#' @param result A list derived from `visual_prep()` that contains datasets and statistical test results.
#' @param use_quarto A boolean variable indicating whether to generate a Quarto report.
#'
#' @return This function does not return a value. It saves the data to the specified file.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
#'
#' @export
#'
#'
#' @examples
#' # Initialize result to NULL for safety
#' result <- NULL
#'
#' # Check if the previous results file exists and load it, or run `visual_prep`
#' if (file.exists("./tests/testthat/previous-results/lm_result.rds")) {
#'   result <- readRDS("./tests/testthat/previous-results/lm_result.rds")
#' }
#'
#' # Use the result if it is available
#' if (!is.null(result)) {
#'   temp_dir <- tempdir()
#'   diag_save(temp_dir, result)
#'   message("Diagnostics saved to: ", temp_dir)
#' } else {
#'   message("Result is NULL. Please ensure the file exists and is accessible.")
#' }

diag_save <- function(path, result, use_quarto = TRUE){
  quarto_package <- requireNamespace("quarto", quietly = TRUE)
  if (use_quarto && !is.null(Sys.which("quarto")[[1]]) && quarto_package) {
    original_dir <- getwd()
    template_path <- system.file("quarto_templates/diagnosis_report.qmd", package = "ComBatFamQC")
    new_template_path <- file.path(path, basename(template_path))
    if (!file.exists(new_template_path)) {
      file.copy(template_path, new_template_path, overwrite = TRUE)
      message("Template moved to: ", new_template_path)
    }
    output_file <- file.path(path, "diagnosis_report.html")
    setwd(path)
    on.exit(setwd(original_dir), add = TRUE)

    quarto::quarto_render(
      input = "diagnosis_report.qmd",
      output_file = "diagnosis_report.html",
      execute_params = list(data = result)
    )
  }else{
    wb <- createWorkbook()
    header_style <- createStyle(textDecoration = "bold", fgFill = "#D3D3D3", halign = "center")
    addWorksheet(wb, "Batch Summary")
    writeData(wb, "Batch Summary", result$info$summary_df)
    addStyle(wb, sheet = "Batch Summary", style = header_style, rows = 1, cols = seq_len(ncol(result$info$summary_df)), gridExpand = TRUE)
    addWorksheet(wb, "PCA Summary")
    writeData(wb, "PCA Summary", result$pca_summary)
    addStyle(wb, sheet = "PCA Summary", style = header_style, rows = 1, cols = seq_len(ncol(result$pca_summary)), gridExpand = TRUE)
    addWorksheet(wb, "MDMR")
    writeData(wb, "MDMR", result$mdmr.summary)
    addStyle(wb, sheet = "MDMR", style = header_style, rows = 1, cols = seq_len(ncol(result$mdmr.summary)), gridExpand = TRUE)
    addWorksheet(wb, "ANOVA")
    writeData(wb, "ANOVA", result$anova_test_df)
    addStyle(wb, sheet = "ANOVA", style = header_style, rows = 1, cols = seq_len(ncol(result$anova_test_df)), gridExpand = TRUE)
    addWorksheet(wb, "Kruskal-Wallis")
    writeData(wb, "Kruskal-Wallis", result$kw_test_df)
    addStyle(wb, sheet = "Kruskal-Wallis", style = header_style, rows = 1, cols = seq_len(ncol(result$kw_test_df)), gridExpand = TRUE)
    addWorksheet(wb, "Kenward-Roger")
    writeData(wb, "Kenward-Roger", result$kr_test_df)
    addStyle(wb, sheet = "Kenward-Roger", style = header_style, rows = 1, cols = seq_len(ncol(result$kr_test_df)), gridExpand = TRUE)
    addWorksheet(wb, "Levene's Test")
    writeData(wb, "Levene's Test", result$lv_test_df)
    addStyle(wb, sheet = "Levene's Test", style = header_style, rows = 1, cols = seq_len(ncol(result$lv_test_df)), gridExpand = TRUE)
    addWorksheet(wb, "Bartlett's Test")
    writeData(wb, "Bartlett's Test", result$bl_test_df)
    addStyle(wb, sheet = "Bartlett's Test", style = header_style, rows = 1, cols = seq_len(ncol(result$bl_test_df)), gridExpand = TRUE)
    addWorksheet(wb, "Fligner-Killeen")
    writeData(wb, "Fligner-Killeen", result$fk_test_df)
    addStyle(wb, sheet = "Fligner-Killeen", style = header_style, rows = 1, cols = seq_len(ncol(result$fk_test_df)), gridExpand = TRUE)
    saveWorkbook(wb, file = paste0(path, "/diagnosis.xlsx"), overwrite = TRUE)
  }
}


#' Export Brain ROI Age Trends
#'
#' Save all brain age trends into a single Excel file.
#'
#' @param path The path to save the excel file.
#' @param age_list A list containing all ROIs' true volumes, age trend estimates, and the fitted GAMLSS model.
#'
#' @return This function does not return a value. It saves the data to the specified file.
#'
#'
#' @export
#'
#'
#' @examples
#' # Initialize result to NULL for safety
#' age_list <- NULL
#'
#' # Check if the previous results file exists and load it
#' if (file.exists("./tests/testthat/previous-results/age_list.rds")) {
#'   age_list <- readRDS("./tests/testthat/previous-results/age_list.rds")
#' }
#'
#' # Use the result if it is available
#' if (!is.null(age_list)) {
#'   temp_dir <- tempdir()
#'   age_save(temp_dir, age_list)
#'   message("Age trend table saved to: ", temp_dir)
#' } else {
#'   message("Age list is NULL. Please ensure the file exists and is accessible.")
#' }
#'




age_save <- function(path, age_list){
  wb <- createWorkbook()
  header_style <- createStyle(textDecoration = "bold", fgFill = "#D3D3D3", halign = "center")
  female_df <- lapply(seq_len(length(age_list)), function(i){
    feature_name <- names(age_list)[i]
    roi_df <- age_list[[i]]$predicted_df_sex %>% filter(.data[["sex"]] == "F") %>% pivot_wider(names_from = .data[["type"]], values_from = .data[["prediction"]]) %>% mutate(roi = feature_name)
    return(roi_df)
  }) %>% bind_rows()

  male_df <- lapply(seq_len(length(age_list)), function(i){
    feature_name <- names(age_list)[i]
    roi_df <- age_list[[i]]$predicted_df_sex %>% filter(.data[["sex"]] == "M") %>% pivot_wider(names_from = .data[["type"]], values_from = .data[["prediction"]]) %>% mutate(roi = feature_name)
    return(roi_df)
  }) %>% bind_rows()

  addWorksheet(wb, "Female")
  writeData(wb, "Female", female_df)
  addStyle(wb, sheet = "Female", style = header_style, rows = 1, cols = seq_len(ncol(female_df)), gridExpand = TRUE)
  addWorksheet(wb, "Male")
  writeData(wb, "Male", male_df)
  addStyle(wb, sheet = "Male", style = header_style, rows = 1, cols = seq_len(ncol(male_df)), gridExpand = TRUE)

  saveWorkbook(wb, file = paste0(path, "/age_trend.xlsx"), overwrite = TRUE)
}

#' Compute Quantile Functions for a Predictor in a GAMLSS Model
#'
#' This function computes quantile functions for a specified predictor in a GAMLSS model, allowing
#' the evaluation of response quantiles (e.g., 25th, 50th, 75th percentiles) as a function of the predictor.
#' It is a modified version of the `getQuantile` function from the **GAMLSS** package, with improvements to
#' explicitly require the dataset as an argument, avoiding reliance on global or external variables.
#'
#' @param obj A fitted GAMLSS model object.
#' @param term A character string specifying the name of the predictor variable for which quantiles are computed.
#' @param quantile A numeric vector of probabilities (e.g., \code{c(0.25, 0.5, 0.75)}) at which to compute the quantiles.
#' @param data A data frame containing the dataset used in the model. This must include the predictor specified in \code{term}.
#' @param n.points An integer specifying the number of points at which to evaluate the quantile functions (default: 100).
#' @param fixed.at A named list specifying fixed values for other predictors in the dataset. If not provided,
#'                 categorical predictors are set to their most frequent levels, and numeric predictors to their median values.
#'
#' @return
#' A list of spline functions, one for each quantile specified in \code{quantile}. Each spline function can
#' be evaluated at specific predictor values to obtain the corresponding quantile.
#'
#' @details
#' This function generates a temporary dataset by varying the specified predictor (\code{term}) over a sequence of
#' values while holding all other predictors constant (using values specified in \code{fixed.at}, or derived defaults).
#' It then computes the distribution parameters for the GAMLSS model and calculates the quantiles using the appropriate
#' quantile function for the distribution family.
#'
#' @importFrom gamlss.dist qNO
#' @importFrom stats splinefun
#'
#' @export
#'
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE)) {
#'   library(gamlss)
#'   set.seed(123)
#'   sub_df <- data.frame(
#'     age = seq(1, 20, length.out = 100),
#'     height = 50 + 2.5 * seq(1, 20, length.out = 100) + rnorm(100, 0, 5)
#'   )
#'
#'   mdl <- gamlss(height ~ pb(age), data = sub_df, family = NO())
#'
#'   quantile_function <- getQuantileRefactored(
#'     obj = mdl,
#'     term = "age",
#'     quantile = c(0.25, 0.5, 0.75),
#'     data = sub_df
#'   )
#'  }else{
#'  message("The 'gamlss' package is not installed. Please install it to run this example.")
#'  }


getQuantileRefactored <- function(obj, term, quantile, data, n.points = 100, fixed.at = list()) {
  if (is.null(obj) || !inherits(obj, "gamlss"))
    stop("Supply a valid GAMLSS model in obj")
  if (is.null(term))
    stop("The model term is not set")
  if (is.null(data))
    stop("You must provide the dataset explicitly in the 'data' argument")

  # Ensure the term exists in the dataset
  if (!(term %in% names(data)))
    stop(paste("The specified term", term, "is not in the dataset"))

  # Generate new data points for the term
  xvar <- seq(min(data[[term]], na.rm = TRUE), max(data[[term]], na.rm = TRUE), length.out = n.points)

  # Create a temporary dataset for predictions
  dat.temp <- data[rep(1, n.points), , drop = FALSE]
  dat.temp[[term]] <- xvar  # Replace the term column with the generated sequence

  # Fill other variables with fixed or derived values
  for (col in setdiff(names(data), term)) {
    if (col %in% names(fixed.at)) {
      dat.temp[[col]] <- fixed.at[[col]]
    } else if (is.factor(data[[col]])) {
      dat.temp[[col]] <- levels(data[[col]])[which.max(table(data[[col]]))]
    } else {
      dat.temp[[col]] <- median(data[[col]], na.rm = TRUE)
    }
  }

  # Predict parameters for the new data
  pp <- predictAll(obj, newdata = dat.temp, data = data, output = "matrix")

  # Quantile function for the family
  qfun <- paste0("q", obj$family[1])
  quantile_values <- sapply(quantile, function(q) {
    do.call(qfun, list(p = q, mu = pp[, "mu"], sigma = pp[, "sigma"]))
  })

  # Return as a list of quantile functions
  lapply(seq_along(quantile), function(i) {
    splinefun(xvar, quantile_values[, i])
  })
}

