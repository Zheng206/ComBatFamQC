#' Post Harmonization Residual Generation
#'
#' Extract residuals after harmonization.
#'
#' @param type A model function name that is to be used (eg: `"lmer"`, `"lm"`, `"gam"`).
#' @param features The names of the features from which to extract residuals.
#' @param covariates Name of covariates supplied to `model`.
#' @param interaction Expression of interaction terms supplied to `model` (eg: `"age,diagnosis"`).
#' @param random Variable name of a random effect in linear mixed effect model.
#' @param smooth Variable name that requires a smooth function.
#' @param smooth_int_type Indicates the type of interaction in `gam` models. By default, `smooth_int_type` is set to be `"linear"`, representing linear interaction terms.
#' `"categorical-continuous"`, `"factor-smooth"` both represent categorical-continuous interactions (`"factor-smooth"` includes categorical variable as part of the smooth),
#' `"tensor"` represents interactions with different scales, and `"smooth-smooth"` represents interaction between smoothed variables.
#' @param df Harmonized dataset to extract residuals from.
#' @param rm variables to remove effects from.
#' @param model A boolean variable indicating whether an existing model is to be used.
#' @param model_path path to the existing model.
#' @param cores number of cores used for parallel computing.
#'
#' @return `residual_gen` returns a list containing the following components:
#' \item{model}{a list of regression models for all rois}
#' \item{residual}{Residual dataframe}
#'
#'
#' @export
#'
#'@examples
#'features <- colnames(adni)[43:53]
#'residual_gen(type = "lm", features = features,
#'covariates = c("AGE", "SEX", "DIAGNOSIS"), df = adni, rm = c("AGE", "SEX"), cores = 1)


residual_gen <- function(type = "lm", features = NULL, covariates = NULL, interaction = NULL, random = NULL, smooth = NULL, smooth_int_type = NULL, df, rm = NULL, model = FALSE, model_path = NULL, cores = detectCores()){

  if(!model){
    ## Characterize/factorize categorical variables
    if(is.null(features)) stop("Please identify the features required to generate residuals!")
    info <- data_prep(stage = "residual", features = features, covariates = covariates, df = df, type = type, random = random, smooth = smooth, interaction = interaction, smooth_int_type = smooth_int_type)
    df <- info$df
    features <- info$features
    covariates <- info$covariates
    interaction <- info$interaction
    smooth <- info$smooth
    used_col <- features
    other_col <- setdiff(colnames(df), used_col)
    other_info <- df[other_col]

    models <- mclapply(features, function(y){
      model <- model_gen(y = y, type = type, batch = NULL, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
      return(model)
    }, mc.cores = cores)
  }else{
    if(is.null(model_path)) stop("Please provide the path to the saved model!")
    models <- readRDS(model_path)
    info <- data_prep(stage = "residual", features = features, covariates = covariates, df = df, type = type, random = random, smooth = smooth, interaction = interaction, smooth_int_type = smooth_int_type, predict = TRUE, object = models)
    df <- info$df
    features <- info$features
    covariates <- info$covariates
    interaction <- info$interaction
    smooth <- info$smooth
    used_col <- features
    other_col <- setdiff(colnames(df), used_col)
    other_info <- df[other_col]
  }

  if(!is.null(rm)){
    if(type!="lmer"){
      residuals <- mclapply(1:length(features), function(i){
        model_coef <- coef(models[[i]])
        rm_names <- c()
        for (x in rm){
          sub_name <- names(model_coef)[which(grepl(x, names(model_coef)))]
          rm_names <- c(rm_names, sub_name)
        }
        rm_coef <- model_coef[names(model_coef) %in% rm_names]
        predict_y <- model.matrix(models[[i]])[, which(grepl(paste0(gsub("([.()])", "\\\\\\1", names(rm_coef)), collapse = "|"), colnames(model.matrix(models[[i]]))))] %*% t(t(unname(rm_coef)))
        residual_y <- df[[features[i]]] - predict_y
        residual_y <- data.frame(residual_y)
      }, mc.cores = cores) %>% bind_cols()
    }else{
      df[[random]] <- as.factor(df[[random]])
      residuals <- mclapply(1:length(features), function(i){
        model_coef <- coef(models[[i]])[[1]]
        rm_names <- c()
        for (x in rm){
          sub_name <- names(model_coef)[which(grepl(x, names(model_coef)))]
          rm_names <- c(rm_names, sub_name)
        }
        rm_coef <- model_coef[names(model_coef) %in% rm_names] %>% distinct()
        predict_y <- model.matrix(models[[i]])[, which(grepl(paste0(names(rm_coef), collapse = "|"), colnames(model.matrix(models[[i]]))))] %*% t(rm_coef)
        residual_y <- df[[features[i]]] - predict_y
        residual_y <- data.frame(residual_y)
      }, mc.cores = cores) %>% bind_cols()
    }
    colnames(residuals) <- features
    residuals <- cbind(other_info, residuals)
    residuals <- residuals[colnames(df)]
  }else{residuals <- df}
  result <- list("model" = models, "residual"= residuals)
  return(result)
}

