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
#' @import tidyverse
#' @importFrom tidyr pivot_longer
#' @importFrom  stats complete.cases
#'
#' @export


data_prep = function(stage = "harmonization", result = NULL, features = NULL, batch = NULL, covariates = NULL, df = NULL, type = "lm", random = NULL, smooth = NULL, interaction = NULL, smooth_int_type = NULL, predict = FALSE, object = NULL){
  if(stage == "harmonization"){
    message("Starting data preparation for the batch effect diagnostic and harmonization stage...")
    ## When the result from the visual preparation stage is provided
    if(!is.null(result)){
      message("Taking the result from the visual preparation stage as input...")
      df = result$info$df
      char_var = result$info$char_var
      batch = result$info$batch
      features = result$info$features
      cov_shiny = result$info$cov_shiny
      obs_n = nrow(df)
      df = df[complete.cases(df[c(features, batch, cov_shiny, random)]),]
      obs_new = nrow(df)
      if((obs_n - obs_new) != 0){
        print(paste0(obs_n - obs_new, " observation(s) are dropped due to missing values."))
      }else{
        print("No observation is dropped due to missing values.")
      }
      df[[batch]] = as.factor(df[[batch]])
      df[char_var] =  lapply(df[char_var], as.factor)
      summary_df = result$info$summary_df
    }else{
      ## When the result from the visual preparation is not provided
      if(!predict){
        message("The result from the visual prepration stage is not provided! The required parameters should be specified...")
        }else{
        if(is.null(object)) stop("Please provide the saved ComBat model!")
        batch = object$batch.name
        eb = object$eb
        model_type = class(object$ComBat.model$fits[[1]])[1]
        features = colnames(object$ComBat.model$estimates$stand.mean)
        form_str = as.character(formula(object$ComBat.model$fits[[1]]))[3]
        if(model_type == "lm"){
          covariates = strsplit(form_str, "\\+")[[1]][which(!grepl("batch|:", strsplit(form_str, "\\+")[[1]]))]
          covariates = sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          random = NULL
          type = "lm"
        }else if(model_type == "lmerMod"){
          covariates = strsplit(form_str, "\\+")[[1]][which(!grepl("batch|:|\\(1", strsplit(form_str, "\\+")[[1]]))]
          covariates = sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          random = strsplit(form_str, "\\+")[[1]][which(grepl("\\(1", strsplit(form_str, "\\+")[[1]]))]
          random = sapply(random, function(x) gsub(" ", "", gsub("\\)", "", strsplit(x, "\\|")[[1]][2])), USE.NAMES = FALSE)
          type = "lmer"
        }else if(model_type == "gam"){
          covariates = strsplit(form_str, "\\+")[[1]][which(!grepl("batch|:|s\\(", strsplit(form_str, "\\+")[[1]]))]
          covariates = sapply(covariates, function(x) gsub(" ", "", x), USE.NAMES = FALSE)
          smooth_term = strsplit(form_str, "\\+")[[1]][which(grepl("s\\(", strsplit(form_str, "\\+")[[1]]))]
          smooth_term = sapply(smooth_term, function(x) gsub("\\) ", "", gsub(" s\\(", "", x)), USE.NAMES = FALSE)
          covariates = c(covariates, smooth_term)
          random = NULL
          type = "gam"
        }
      }
      obs_n = nrow(df)
      df = df[complete.cases(df[c(features, batch, covariates, random)]),]
      obs_new = nrow(df)
      if((obs_n - obs_new) != 0){
        print(paste0(obs_n - obs_new, " observation(s) are dropped due to missing values."))
      }else{
        print("No observation is dropped due to missing values.")
      }
      df[[batch]] = as.factor(df[[batch]])
      char_var = covariates[sapply(df[covariates], function(col) is.character(col) || is.factor(col))]
      enco_var = covariates[sapply(df[covariates], function(col) length(unique(col)) == 2 && all(unique(col) %in% c(0,1)))]
      df[char_var] =  lapply(df[char_var], as.factor)
      df[enco_var] =  lapply(df[enco_var], as.factor)
      cov_shiny = covariates
      char_var = c(char_var, enco_var)

      # Summary
      summary_df = df %>% group_by(eval(parse(text = batch))) %>% summarize(count = n(), percentage = 100*count/nrow(df))
      colnames(summary_df) = c(batch, "count", "percentage (%)")
      summary_df = summary_df %>% mutate(remove = case_when(count < 3 ~ "removed",
                                                            .default = "keeped"))
      batch_rm = summary_df %>% filter(remove == "removed") %>% pull(eval(parse(text = batch))) %>% droplevels()
      if(length(batch_rm) > 0){
        print(paste0("Batch levels that contain less than 3 observations are dropped: ", length(batch_rm), " level(s) are dropped, corresponding to ", df %>% filter(eval(parse(text = batch)) %in% batch_rm) %>% nrow(), " observations."))
      }else{print("Batch levels that contain less than 3 observations are dropped: no batch level is dropped.")}
      df = df %>% filter(!eval(parse(text = batch)) %in% batch_rm)
      df[[batch]] = df[[batch]] %>% droplevels()
    }

    if(!is.null(random)){
      for (r in random){
        df[[r]] = as.factor(df[[r]])
      }
    }

    ## drop univariate features
    features_orig = df[features]
    n_orig = length(colnames(features_orig))
    features_new = features_orig[, apply(features_orig, 2, function(col) { length(unique(col)) > 1 })]
    n_new = length(colnames(features_new))
    dropped_col = NULL
    if (n_orig > n_new){
      dropped_col = setdiff(colnames(features_orig), colnames(features_new))
      print(paste0(n_orig - n_new, " univariate feature column(s) are dropped: ", dropped_col))
    }
    features = colnames(features_new)

    int_result = interaction_gen(type = type, covariates = cov_shiny, interaction = interaction, smooth = smooth, smooth_int_type = smooth_int_type)
    interaction_orig = interaction
    smooth_orig = smooth
    covariates = int_result$covariates
    interaction = int_result$interaction
    smooth = int_result$smooth

    return(list("batch" = batch, "features" = features, "type" = type, "covariates" = covariates, "interaction" = interaction, "random" = random, "smooth" = smooth, "df" = df, "cov_shiny" = cov_shiny, "char_var" = char_var, "smooth_int_type" = smooth_int_type, "interaction_orig" = interaction_orig, "smooth_orig" = smooth_orig, "summary_df" = summary_df))
  }else{
    message("Starting data preparation for the post-harmonization stage...")
    obs_n = nrow(df)
    df = df[complete.cases(df[c(features, covariates, random)]),]
    obs_new = nrow(df)
    if((obs_n - obs_new) != 0){
      print(paste0(obs_n - obs_new, " observation(s) are dropped due to missing values."))
    }else{
      print("No observation is dropped due to missing values.")
    }
    char_var = covariates[sapply(df[covariates], function(col) is.character(col) || is.factor(col))]
    enco_var = covariates[sapply(df[covariates], function(col) length(unique(col)) == 2 && all(unique(col) %in% c(0,1)))]
    df[char_var] =  lapply(df[char_var], as.factor)
    df[enco_var] =  lapply(df[enco_var], as.factor)
    cov_shiny = covariates
    char_var = c(char_var, enco_var)

    if(!is.null(random)){
      for (r in random){
        df[[r]] = as.factor(df[[r]])
      }
    }

    ## drop univariate features
    features_orig = df[features]
    n_orig = length(colnames(features_orig))
    features_new = features_orig[, apply(features_orig, 2, function(col) { length(unique(col)) > 1 })]
    n_new = length(colnames(features_new))
    dropped_col = NULL
    if (n_orig > n_new){
      dropped_col = setdiff(colnames(features_orig), colnames(features_new))
      print(paste0(n_orig - n_new, " univariate feature column(s) are dropped: ", dropped_col))
    }

    features = colnames(features_new)

    ## generate interactions
    int_result = interaction_gen(type = type, covariates = cov_shiny, interaction = interaction, smooth = smooth, smooth_int_type = smooth_int_type)
    interaction_orig = interaction
    smooth_orig = smooth
    covariates = int_result$covariates
    interaction = int_result$interaction
    smooth = int_result$smooth
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
#' @import tidyverse
#' @importFrom methods hasArg
#' @importFrom invgamma rinvgamma
#' @importFrom stats rnorm
#'
#' @export


eb_check = function(data, bat, covar = NULL, model = lm, formula = NULL, robust.LS = FALSE, ref.batch = NULL, ...){
  data = as.matrix(data)
  n = nrow(data)
  p = ncol(data)

  if (p == 1) {
    warning("EB step skipped for univariate data.")
  }

  if (!is.null(ref.batch)) {
    if (!(ref.batch %in% levels(bat))) {
      stop("Reference batch must be in the batch levels")
    }
    ref = bat == ref.batch
    nref = sum(ref)
  }

  bat = droplevels(bat)
  batch = model.matrix(~ -1 + bat)
  batches = lapply(levels(bat), function(x) which(bat == x))
  n_batches = sapply(batches, length)

  if (robust.LS) {
    loc = median
    scl = .biweight_midvar
  } else {
    loc = mean
    scl = var
  }

  if (is.null(covar)) {
    mod = data.frame(I(batch))
  } else {
    mod = data.frame(covar, I(batch))
  }

  if (is.null(covar) | is.null(formula)) {
    formula = y ~ 1
  }

  # case when using nlme::lme
  if (hasArg("fixed")) {
    fits = apply(data, 2, function(y) {
      dat = data.frame(y = y, mod)

      addargs = list(...)
      addargs$fixed = NULL
      bat_formula = update(formula, ~ . + batch + -1)
      do.call(model, c(list(fixed = bat_formula, data = dat), addargs))
    })
  } else {
    fits = apply(data, 2, function(y) {
      dat = data.frame(y = y, mod)
      bat_formula = update(formula, ~ . + batch + -1)
      do.call(model, list(formula = bat_formula, data = dat, ...))
    })
  }

  #### Standardize the data ####
  # Model matrix for obtaining pooled mean
  pmod = mod
  pmod$batch[] = matrix(n_batches/n, n, nlevels(bat), byrow = TRUE)

  # Reference batch
  if (!is.null(ref.batch)) {
    pmod$batch[] = 0
    pmod$batch[,which(levels(bat) == ref.batch)] = 1
  }

  stand_mean = sapply(fits, predict, newdata = pmod, type = "response")
  resid_mean = sapply(fits, predict, newdata = mod, type = "response")

  if (!is.null(ref.batch)) {
    var_pooled = apply((data - resid_mean)[ref, , drop = FALSE], 2, scl) *
      (nref - 1)/nref
  } else {
    var_pooled = apply(data - resid_mean, 2, scl) * (n - 1)/n
  }

  if (hasArg("sigma.formula")) {
    sd_mat = sapply(fits, predict, newdata = pmod, what = "sigma",
                    type = "response")
  } else {
    sd_mat = sapply(sqrt(var_pooled), rep, n)
  }

  data_stand = (data-stand_mean)/sd_mat

  #### Obtain location and scale adjustments ####
  gamma_hat = Reduce(rbind, by(data_stand, bat, function(x) apply(x, 2, loc)))
  delta_hat = Reduce(rbind, by(data_stand, bat, function(x) apply(x, 2, scl)))

  rownames(gamma_hat) = rownames(delta_hat) = levels(bat)

  # Empirical Bayes adjustments

  eb_df = lapply(1:nlevels(bat), function(i){
    n_b = n_batches[i]

    # method of moments estimates
    g_bar = mean(gamma_hat[i,])
    g_var = var(gamma_hat[i,])

    d_bar = mean(delta_hat[i,])
    d_var = var(delta_hat[i,])

    d_a = (2 * d_var + d_bar^2)/d_var
    d_b = (d_bar * d_var + d_bar^3)/d_var

    # generate prior distribution
    g_prior = rnorm(length(gamma_hat[i,]), g_bar, g_var)
    d_prior = rinvgamma(length(gamma_hat[i,]), d_a, d_b)
    eb_df = data.frame(cbind("batch" = rep(rownames(gamma_hat)[i], length(gamma_hat[i,])), "features" = names(gamma_hat[i,]), "gamma_hat" = gamma_hat[i,], "gamma_prior" = g_prior, "delta_hat" = delta_hat[i,], "delta_prior" = d_prior))
    return(eb_df)
  }) %>% bind_rows() %>% pivot_longer(cols = c(3:6), names_to = "type", values_to = "eb_values") %>% mutate(eb_values = as.numeric(eb_values),
                                                                                                            batch = as.factor(batch))
  return(eb_df)
}

.biweight_midvar = function(data, center=NULL, norm.unbiased = TRUE) {
  if (is.null(center)) {
    center = median(data)
  }

  mad = median(abs(data - center))
  d = data - center
  c = ifelse(norm.unbiased, 9/qnorm(0.75), 9)
  u = d/(c*mad)

  n = length(data)
  indic = abs(u) < 1

  num = sum(indic * d^2 * (1 - u^2)^4)
  dem = sum(indic * (1 - u^2) * (1 - 5*u^2))^2

  n * num/dem
}


#' Model generations
#'
#' Generate appropriate regression models based on the model type and formula
#'
#' @param y Dependent variable in the model.
#' @param type A model function name that is used or to be used in the ComBatFamily Package (eg: "lmer", "lm", "gam").
#' @param batch Name of batch variable (often equivalent to site or scanner).
#' @param covariates Name of covariates supplied to `model`.
#' @param interaction Expression of interaction terms supplied to `model` (eg: "age*diagnosis").
#' @param random Variable name of a random effect in linear mixed effect model.
#' @param smooth Variable name that requires a smooth function.
#' @param df Dataset to be harmonized.
#'
#' @return A regression model object to be used for batch effect diagnostics and the post-harmonization stage.
#'
#' @export

model_gen = function(y, type, batch, covariates, interaction = NULL, random = NULL, smooth = NULL, df){
  if(!is.null(batch)){
    if(type == "lmer"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model = lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", batch, " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)
        }else{
          model = lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch, " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
      }else{model = lmer(as.formula(paste0(y, " ~ ", batch, " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
    }else if(type == "lm"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model = lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", batch)), data = df)
        }else{
          model = lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch)), data = df)}
      }else{
        model = lm(as.formula(paste0(y, " ~ ", batch)), data = df)
      }
    }else if(type == "gam"){
      if(is.null(interaction)){
        model = gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", batch)), data = df)
      }else{
        if(length(smooth) > 0){
          model = gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch)), data = df)
        }else{
          model = gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", batch)), data = df)
        }
      }
    }
  }else{
    if(type == "lmer"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model = lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)
        }else{
          model = lmer(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "), " + ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
      }else{model = lmer(as.formula(paste0(y, " ~ ", paste("(1 |", random, ")", collapse = " + "))), data = df)}
    }else if(type == "lm"){
      if(!is.null(covariates)){
        if(is.null(interaction)){
          model = lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "))), data = df)
        }else{
          model = lm(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "))), data = df)}
      }else{
        model = lm(as.formula(paste0(y, " ~ 1")), data = df)
      }
    }else if(type == "gam"){
      if(is.null(interaction)){
        model = gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "))), data = df)
      }else{
        if(length(smooth) > 0){
          model = gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", paste(interaction, collapse = " + "))), data = df)
        }else{
          model = gam(as.formula(paste0(y, " ~ ", paste(covariates, collapse = " + "), " + ", paste(interaction, collapse = " + "))), data = df)
        }
      }
    }
  }
  return(model)
}

#' ComBatFamily model formula generations
#'
#' Generate appropriate formula for ComBatFamily models
#'
#' @param x A model function name that is used or to be used in the ComBatFamily Package (eg: "lmer", "lm", "gam").
#' @param c Data frame or matrix of covariates supplied to `model`
#' @param i Expression of interaction terms supplied to `model`, using comma to separate terms. (eg: "age,diagnosis").
#' @param random Variable name of a random effect in linear mixed effect model.
#' @param smooth Variable name that requires a smooth function.
#'
#' @return A string of formula
#'
#' @export
form_gen = function(x, c, i, random, smooth){
  if (x == "lm"){
    if(!is.null(c)){
      if (is.null(i)){form = paste0("y ~", paste(colnames(c), collapse = "+"))}else{
        form = paste0("y ~", paste(colnames(c), collapse = "+"),  " + ", paste(i, collapse = " + "))
      }
    }else if(length(c) == 0){
      form = NULL}else{form = NULL}
  }else if (x == "lmer"){
    if(!is.null(c)){
      if (is.null(i)){form = paste0("y ~", paste(colnames(c), collapse = " + "),  " + ", paste("(1 |", random, ")", collapse = " + "))}else{
        form = paste0("y ~", paste(colnames(c), collapse = " + "),  " + ", paste(i, collapse = " + "), " + ", paste("(1 |", random, ")", collapse = " + "))
      }
    }else if(length(c) == 0){form = paste0("y ~", paste("(1 |", random, ")", collapse = " + "))}else{form = paste0("y ~", paste("(1 |", random, ")", collapse = " + "))}
  }else if(x == "gam"){
    if (is.null(i)){form = paste0("y ~ ", paste(colnames(c), collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "))}else{
      if(length(smooth) > 0){
        form = paste0("y ~ ", paste(colnames(c), collapse = " + "), " + ", paste("s(", smooth, ")", collapse = " + "), " + ", paste(i, collapse = " + "))
      }else{
        form = paste0("y ~ ", paste(colnames(c), collapse = " + "), " + ", paste(i, collapse = " + "))
      }
    }
  }
  return(form)
}


#' Interaction term generation
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
#' \item{covariates}{Modefied covariates after expressing interaction terms}
#' \item{smooth}{Modefied smooth terms after expressing interaction terms}
#'
#' @export
interaction_gen = function(type = "lm", covariates, smooth = NULL, interaction = NULL, smooth_int_type = NULL){
  if(!is.null(interaction)){
    if(type == "gam"){
      covariates = setdiff(covariates, smooth)
      inter_gen = function(interaction, smooth, covariates, x){
        if(x == "linear"){
          interaction = gsub(",", ":", interaction)
          smooth_rm = NULL
          covariate_rm = NULL
        }else if(x == "categorical-continuous"){
          element = strsplit(interaction,",")[[1]]
          smooth_element = element[which(element %in% smooth)]
          categorical_element = setdiff(element, smooth_element)
          interaction = paste0("s(", smooth_element, ", by = ", categorical_element, ")")
          smooth_rm = smooth_element
          covariate_rm = NULL
        }else if(x == "factor-smooth"){
          element = strsplit(interaction,",")[[1]]
          interaction = paste("s(", interaction, ", bs = 'fs')")
          smooth_element = element[which(element %in% smooth)]
          categorical_element = setdiff(element, smooth_element)
          smooth_rm = smooth_element
          covariate_rm = categorical_element
        }else if(x == "tensor"){
          interaction = paste("ti(", interaction, ")")
          smooth_rm = NULL
          covariate_rm = NULL
        }else if(x == "smooth-smooth"){
          element = strsplit(interaction,",")
          interaction = paste("s(", interaction, ")")
          smooth_rm = element[[1]]
          covariate_rm = NULL
        }
        element_result = list(interaction = interaction, smooth_rm = smooth_rm, covariate_rm = covariate_rm)
        return(element_result)
      }
      interaction_after = lapply(1:length(interaction), function(i) inter_gen(interaction[i], smooth, covariates, x = smooth_int_type[i])$interaction) |> unlist()
      smooth_rm= lapply(1:length(interaction), function(i) inter_gen(interaction[i], smooth, covariates, x = smooth_int_type[i])$smooth_rm) |> unlist()
      covariate_rm = lapply(1:length(interaction), function(i) inter_gen(interaction[i], smooth, covariates, x = smooth_int_type[i])$covariate_rm) |> unlist()
      smooth_after = smooth[which(!smooth %in% smooth_rm)]
      if(length(smooth_after)==0){smooth = NULL}else{smooth = smooth_after}
      if(length(covariates[which(!covariates %in% covariate_rm)]) == 0){covariates = NULL}else{covariates = covariates[which(!covariates %in% covariate_rm)]}
    }else{
      interaction_after = gsub(",", ":", interaction)
      smooth = smooth
      covariates = covariates
    }
  }else{
    interaction_after = NULL
    smooth = smooth
    covariates = setdiff(covariates, smooth)
  }
  int_result = list("interaction" = interaction_after, "covariates" = covariates, "smooth" = smooth)
  return(int_result)
}

