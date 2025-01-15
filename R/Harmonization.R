#' ComBatFamily Harmonization
#'
#' Conduct harmonization using four types of methods: 1) Original ComBat, 2) Longitudinal ComBat, 3) ComBat-GAM, and 4) CovBat.
#'
#' @param eb_check A boolean variable indicating whether the user wants to run the EB assumption test before harmonization.
#' @param result A list derived from `visual_prep()` that contains dataset and batch effect diagnostic information for Shiny visualization. Can be skipped if `features`, `batch`, `covariates` and `df` are provided.
#' @param features The name of the features to be harmonized. This can be skipped if `result` is provided.
#' @param batch The name of the batch variable. Can be skipped if `result` is provided.
#' @param covariates The names of covariates supplied to `model`. This can be be skipped if `result` is provided.
#' @param df Dataset to be harmonized. This can be be skipped if `result` is provided.
#' @param type The name of a regression model to be used: `"lmer"`, `"lm"`, `"gam"`.
#' @param random The variable name of a random effect in linear mixed effect model.
#' @param smooth The name of the covariates that require a smooth function.
#' @param interaction Expression of interaction terms supplied to `model` (eg: `"age,diagnosis"`).
#' @param smooth_int_type A vector that indicates the types of interaction in `gam` models. By default, `smooth_int_type` is set to be NULL, `"linear"` represents linear interaction terms.
#' `"categorical-continuous"`, `"factor-smooth"` both represent categorical-continuous interactions (`"factor-smooth"` includes categorical variable as part of the smooth),
#' `"tensor"` represents interactions with different scales, and "smooth-smooth" represents interaction between smoothed variables.
#' @param family The type of combat family to use, `comfam` or `covfam`.
#' @param eb If \code{TRUE}, uses ComBat model with empirical Bayes for mean and variance harmonization
#' @param ref.batch The name of the reference batch.
#' @param predict A boolean variable indicating whether to run ComBat from scratch or apply existing model to new dataset (currently only work for original ComBat and ComBat-GAM).
#' @param object Existing ComBat model.
#' @param reference Dataset to be considered as the reference group.
#' @param out_ref_include A boolean variable indicating whether the reference data should be included in the harmonized data output.
#' @param ... Additional arguments to `comfam` or `covfam` models.
#'
#' @return If the `eb_check` is set to be FALSE, then `combat_harm` returns a list containing the following components:
#' \item{com_family}{ComBat family to be considered: comfam, covfam}
#' \item{harmonized_df}{Harmonized dataset}
#' \item{combat.object}{Saved ComBat model and relevant information, such as the batch variable name and whether the EB method is used}
#' If `eb_check`  is set to be TRUE, then `combat_harm` will return a dataframe with the EB assumption test result.
#'
#' @importFrom stats formula
#'
#' @export
#'
#' @examples
#' combat_harm(features = colnames(adni)[43:53], batch = "manufac",
#' covariates = c("AGE", "SEX", "DIAGNOSIS"), df = head(adni,100), type = "lm")
#'

combat_harm <- function(eb_check = FALSE, result = NULL, features = NULL, batch = NULL, covariates = NULL, df = NULL, type = "lm", random = NULL, smooth = NULL, interaction = NULL, smooth_int_type = NULL, family = "comfam", eb = TRUE, ref.batch = NULL, predict = FALSE, object = NULL, reference = NULL, out_ref_include = TRUE, ...){
  info <- data_prep(stage = "harmonization", result = result, features = features, batch = batch, covariates = covariates, df = df, type = type, random = random, smooth = smooth, interaction = interaction, smooth_int_type = smooth_int_type, predict = predict, object = object)
  df <- info$df
  batch <- info$batch
  features <- info$features
  covariates <- info$covariates
  interaction <- info$interaction
  smooth <- info$smooth
  cov_shiny <- info$cov_shiny
  char_var <- info$char_var

  # Empirical Estimates
  if (is.null(covariates)){
    if(type == "lmer"){
      form_c <- NULL
      combat_c <- df[random]
    }else{
      form_c <- NULL
      combat_c <- NULL
    }
  }else{
    if(type == "lmer"){
      form_c <- df[covariates]
      combat_c <- cbind(df[cov_shiny], df[random])
    }else{
      form_c <- df[covariates]
      combat_c <- df[cov_shiny]
    }
  }
  if(!eb_check){
    if (is.null(reference)){
      if (!predict){
        message("Starting first-time harmonization...")
        form <- form_gen(x = type, c = form_c, i = interaction, random = random, smooth = smooth)
        if(family == "comfam"){
          ComBat_run <- comfam(data = df[features],
                               bat = df[[batch]],
                               covar = combat_c,
                               model = eval(parse(text = type)),
                               formula = as.formula(form),
                               ref.batch = ref.batch,
                               eb = eb,
                               ...)
        }else{
          ComBat_run <- covfam(data = df[features],
                               bat = df[[batch]] ,
                               covar = combat_c,
                               model = eval(parse(text = type)),
                               formula = as.formula(form),
                               ref.batch = ref.batch,
                               eb = eb,
                               ...)
        }
      }else{
        message("Starting out-of-sample harmonization using the saved ComBat Model...")
        ComBat_run <- predict(object = object$ComBat.model, newdata = df[features], newbat = df[[batch]], newcovar = combat_c, eb = object$eb, ...)
      }
    }else{
      message("Starting out-of-sample harmonization using the reference dataset...")
      reference[[batch]] <- as.factor(reference[[batch]])
      reference[char_var] <-  lapply(reference[char_var], as.factor)
      if(!is.null(random)){
        for (r in random){
          reference[[r]] <- as.factor(reference[[r]])
        }
      }
      ## check if reference data is included in the new data
      other_info <- setdiff(colnames(reference), features)
      n_ref <- df %>% semi_join(reference[other_info]) %>% nrow()
      if(n_ref == nrow(reference)){
        message("The reference data is included in the new unharmonized dataset")
        untouched <- reference
        untouched_included <- reference %>% semi_join(df[other_info])
        new_data <- df %>% anti_join(reference[other_info])
      }else if(n_ref < nrow(reference) & n_ref > 0){
        message("The reference data is partially included in the new unharmonized dataset")
        untouched <- reference
        untouched_included <- reference %>% semi_join(df[other_info])
        new_data <- df %>% anti_join(reference[other_info])
      }else if(n_ref == 0){
        message("The reference data is separated from the new unharmonized dataset")
        untouched <- reference
        untouched_included <- NULL
        new_data <- df
      }

      reference[[batch]] <- "reference"
      df_c <- rbind(reference, new_data)
      df_c[[batch]] <- as.factor(df_c[[batch]])
      if (is.null(covariates)){
        form_c <- NULL
        combat_c <- NULL
      }else{
        if(type == "lmer"){
          form_c <- df_c[covariates]
          combat_c <- cbind(df_c[cov_shiny], df_c[random])
        }else{
          form_c <- df_c[covariates]
          combat_c <- df_c[cov_shiny]
        }
      }
      form <- form_gen(x = type, c = form_c, i = interaction, random = random, smooth = smooth)
      if(family == "comfam"){
        ComBat_run <- comfam(data = df_c[features],
                             bat = df_c[[batch]],
                             covar = combat_c,
                             model = eval(parse(text = type)),
                             formula = as.formula(form),
                             ref.batch = "reference",
                             eb = eb,
                             ...)
      }else{
        ComBat_run <- covfam(data = df_c[features],
                             bat = df_c[[batch]] ,
                             covar = combat_c,
                             model = eval(parse(text = type)),
                             formula = as.formula(form),
                             ref.batch = "reference",
                             eb = eb,
                             ...)
      }
    }

    # Result
    used_col <- c(features, cov_shiny, batch)
    other_col <- setdiff(colnames(df), used_col)
    other_info <- df[other_col]

    if (is.null(reference)){
      if(family == "covfam"){
        com_family <- "covfam"
        comf_df <- ComBat_run$dat.covbat
        comf_df <- cbind(other_info, df[batch], df[cov_shiny], comf_df)
      }else{
        com_family <- "comfam"
        comf_df <- ComBat_run$dat.combat
        comf_df <- cbind(other_info, df[batch], df[cov_shiny], comf_df)
      }
    }else{
      if(family == "covfam"){
        com_family <- "covfam"
        comf_df <- ComBat_run$dat.covbat[(nrow(reference)+1):nrow(df_c),]
        comf_df <- cbind(new_data[other_col], new_data[batch], new_data[cov_shiny], comf_df)
        comf_df <- comf_df[colnames(df)]
        comf_df <- rbind(untouched_included, comf_df)
        if(out_ref_include){comf_df <- rbind(untouched, comf_df) %>% distinct()}
      }else{
        com_family <- "comfam"
        comf_df <- ComBat_run$dat.combat[(nrow(reference)+1):nrow(df_c),]
        comf_df <- cbind(new_data[other_col], new_data[batch], new_data[cov_shiny], comf_df)
        comf_df <- comf_df[colnames(df)]
        comf_df <- rbind(untouched_included, comf_df)
        if(out_ref_include){comf_df <- rbind(untouched, comf_df) %>% distinct()}
      }
    }
    comf_df <- comf_df[colnames(df)]
    combat_result <-  list("com_family" = com_family, "harmonized_df" = comf_df, "combat.object" = list("ComBat.model" = ComBat_run, "batch.name" = batch, "eb" = eb))
    return(combat_result)
  }else{
    message("Starting Empirical Bayes assumption check...")
    form <- form_gen(x = type, c = form_c, i = interaction, random = random, smooth = smooth)
    eb_df <- eb_check(data = df[features],
             bat = df[[batch]],
             covar = combat_c,
             model = eval(parse(text = type)),
             formula = as.formula(form),
             ref.batch = ref.batch,
             ...)
    return(eb_df)
  }
}


#' ComBat Family Harmonization
#'
#' Implementation of the ComBat Family of harmonization methods allowing for
#' flexible covariate modeling and alternative estimators for site effect
#' adjustment. Support for modeling of both location and scale via GAMLSS and
#' longitudinal harmonization via mixed effects models.
#'
#' @param data \emph{n x p} data frame or matrix of observations where
#'   \emph{p} is the number of features and \emph{n} is the number of subjects.
#' @param bat Factor indicating batch (often equivalent to site or scanner)
#' @param covar Data frame or matrix of covariates supplied to `model`
#' @param model Model function. ComBat Family supports any models that take
#'   arguments `formula` and `data`, but are limited to models fitting with
#'   identity link (e.g. `family = gaussian(link = "identity")`). This includes
#'   \link[stats]{lm}, \link[mgcv]{gam}, \link[gamlss]{gamlss},
#'   \link[quantreg]{rq}, \link[lme4]{lmer}, and more
#' @param formula Formula for `model` starting with `y ~` where `y` represents
#'   each feature
#' @param eb If \code{TRUE}, uses ComBat model with empirical Bayes for mean
#'   and variance harmonization
#' @param robust.LS If \code{TRUE}, uses robust location and scale estimators
#'   for error variance and site effect parameters. Currently uses median and
#'   biweight midvariance
#' @param ref.batch Reference batch, must take value in `levels(bat)`
#' @param ... Additional arguments to `model`
#'
#' @return `comfam` returns a list containing the following components:
#' \item{dat.combat}{Harmonized data as a matrix with same dimensions as `data`}
#' \item{batch.info}{Batch information, including reference batch if specified}
#' \item{fits}{List of model fits from regression step, outputs of `model` for each feature}
#' \item{estimates}{List of estimates from standardization and batch effect correction}
#'
#' @importFrom methods hasArg
#' @export
#'
#' @seealso
#'
#' \link[ComBatFamQC]{predict.comfam} for applying ComBat parameters for
#' harmonization of new observations
#'
#' @examples
#' comfam(iris[,1:2], iris$Species)
#' comfam(iris[,1:2], iris$Species, iris[3:4], lm, y ~ Petal.Length + Petal.Width)
comfam <- function(data, bat, covar = NULL, model = lm, formula = NULL,
                   eb = TRUE, robust.LS = FALSE, ref.batch = NULL, ...) {
  if (hasArg("family")) {
    if (list(...)$family$family[1] != "NO") {
      warning("Families other than Gaussian are supported but experimental, output dataset will not necessarily be in the original space.")

      warning("EB step will still assume Gaussian errors.")
    }
  }

  if(is.null(formula) && !(is.null(covar))) {
    warning("Covariates included but not controlled for, use the formula argument to control for covariates")
  }

  if(!(is.null(formula)) && is.null(covar)) {
    warning("Formula specified but covariates not included, covariate effects may not be preserved")
  }

  # Data details and formatting
  data <- as.matrix(data)
  n <- nrow(data)
  p <- ncol(data)

  if ((p == 1) & eb) {
    warning("EB step skipped for univariate data.")
    eb <- FALSE
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

  # Specify robust location/scale estimators
  if (robust.LS) {
    loc <- median
    scl <- .biweight_midvar
  } else {
    loc <- mean
    scl <- var
  }

  #### Fit specified models ####
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

      # include batch in formula to target pooled mean/variance
      bat_formula <- update(formula, ~ . + batch + -1)
      do.call(model, c(list(fixed = bat_formula, data = dat), addargs))
    })
  } else {
    fits <- apply(data, 2, function(y) {
      dat <- data.frame(y = y, mod)

      # include batch in formula to target pooled mean/variance
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
  if (eb) {
    gamma_star <- NULL
    delta_star <- NULL

    for (i in 1:nlevels(bat)) {
      n_b <- n_batches[i]

      # method of moments estimates
      g_bar <- mean(gamma_hat[i,])
      g_var <- var(gamma_hat[i,])

      d_bar <- mean(delta_hat[i,])
      d_var <- var(delta_hat[i,])

      d_a <- (2 * d_var + d_bar^2)/d_var
      d_b <- (d_bar * d_var + d_bar^3)/d_var

      # adjust within batch
      bdat <- data_stand[batches[[i]],]
      g_orig <- gamma_hat[i,]
      g_old  <- gamma_hat[i,]
      d_old  <- delta_hat[i,]

      change_old <- 1
      change <- 1
      count  <- 0
      while(change > 10e-5){
        g_new <- (n_b*g_var*g_orig + d_old*g_bar)/(n_b*g_var + d_old)

        if (robust.LS) {
          sum2 <- (n_b-1) * sapply(1:p, function(v) {
            .biweight_midvar(bdat[,v], g_new[v])})
        } else {
          sum2   <- colSums(sweep(bdat, 2, g_new)^2)
        }

        d_new <- (sum2/2 + d_b)/(n_b/2 + d_a - 1)

        change <- max(abs(g_new - g_old)/g_old, abs(d_new - d_old)/d_old)

        if (count > 30) {
          if (change > change_old) {
            warning("Empirical Bayes step failed to converge after 30 iterations,
    	            using estimate before change between iterations increases.")
            break
          }
        }

        g_old <- g_new
        d_old <- d_new

        change_old <- change
        count <- count+1
      }

      gamma_star <- rbind(gamma_star, g_new)
      delta_star <- rbind(delta_star, d_new)
    }

    rownames(gamma_star) <- rownames(gamma_hat)
    rownames(delta_star) <- rownames(delta_hat)
  } else {
    gamma_star <- gamma_hat
    delta_star <- delta_hat
  }

  #### Harmonize the data ####
  # Remove batch effects
  data_nb <- data_stand
  for (i in 1:nlevels(bat)) {
    data_nb[batches[[i]],] <- sweep(data_nb[batches[[i]],, drop = FALSE], 2,
                                    gamma_star[i,], "-")
    data_nb[batches[[i]],] <- sweep(data_nb[batches[[i]],, drop = FALSE], 2,
                                    sqrt(delta_star[i,]), "/")
  }

  # Reintroduce covariate effects
  data_combat <- data_nb*sd_mat + stand_mean

  if (!is.null(ref.batch)) {
    data_combat[ref,] <- data[ref,]
  }

  estimates <-  list(
    stand.mean = stand_mean,
    stand.sd = sd_mat,
    var.pooled = var_pooled,
    gamma.hat = gamma_hat,
    delta.hat = delta_hat,
    gamma.star = gamma_star,
    delta.star = delta_star
  )

  batch_info <- list(
    batch = bat,
    batch.mod = pmod$batch,
    ref.batch = ref.batch
  )

  out <- list(dat.combat = data_combat, batch.info = batch_info,
              fits = fits, estimates = estimates)
  class(out) <- "comfam"
  out
}

#' Apply Harmonization to New Data
#'
#' Using parameters estimated via `comfam`, apply harmonization on new data.
#' `predict.comfam` will estimate new batch adjustments if new batches are
#' specified. For batches with existing estimates, the estimates from `object`
#' are used. Harmonization targets are the same as `object` (e.g. `ref.batch`
#' from `object` if specified).
#'
#' **Note:** The function currently does not support models of class \code{lmer}
#' (e.g., from \link[lme4]{lmer}).
#'
#' @param object Object of class `comfam`, typically output of the harmonization
#'   function in this package.
#' @param newdata \emph{n x p} data frame or matrix of new observations where
#'   \emph{p} is the number of features and \emph{n} is the number of subjects.
#'   The features must match the original `data` used in `object`
#' @param newbat Factor indicating new batch (often equivalent to site or scanner)
#' @param newcovar Data frame or matrix of new covariates supplied to `model`.
#'   Must contain all variables specified in the original `formula` used in
#'   `object`.
#' @param eb If \code{TRUE}, uses ComBat model with empirical Bayes for new batches
#' @param robust.LS If \code{TRUE}, uses robust location and scale estimators
#'   for new batch effect estimates Currently uses median and biweight
#'   midvariance
#' @param ... Additional arguments to `predict` for the class of `model` (e.g.
#'   `predict.lm` for ComBat)
#'
#' @return `predict.comfam` returns a list containing the following components:
#' \item{dat.combat}{New harmonized data as a matrix with same dimensions as `newdata`}
#' \item{batch.info}{New batch information, including reference batch if specified}
#' \item{fits}{List of model fits from regression step, forwarded from `object`}
#' \item{estimates}{List of estimates from standardization and batch effect correction, including new batches if relevant}
#'
#' @export
#'
#' @examples
#' com_out <- comfam(iris[1:75,1:2], iris$Species[1:75])
#'
#' # out-of-sample with new batch
#' out_pred <- predict(com_out, iris[76:150,1:2], iris$Species[76:150])
#'
#' # in-sample
#' in_pred <- predict(com_out, iris[1:25,1:2], iris$Species[1:25])
#' max(in_pred$dat.combat - com_out$dat.combat[1:25,])

predict.comfam <- function(object, newdata, newbat, newcovar = NULL,
                           robust.LS = FALSE, eb = TRUE, ...) {

  data <- as.matrix(newdata)
  n <- nrow(data)
  p <- ncol(data)

  if ((p == 1) & eb) {
    warning("EB step skipped for univariate data.")
    eb <- FALSE
  }

  # Specify robust location/scale estimators
  if (robust.LS) {
    loc <- median
    scl <- .biweight_midvar
  } else {
    loc <- mean
    scl <- var
  }

  bat <- object$batch.info$batch
  batch_mod <- object$batch.info$batch.mod
  stand_mean <- object$estimates$stand.mean
  var_pooled <- object$estimates$var.pooled
  gamma_hat <- object$estimates$gamma.hat
  delta_hat <- object$estimates$delta.hat
  gamma_star <- object$estimates$gamma.star
  delta_star <- object$estimates$delta.star
  fits <- object$fits

  #### Match new batches to old batches ####
  known <- rownames(gamma_hat)
  bat <- droplevels(bat)
  newbat <- droplevels(newbat)
  bat_levels <- union(known, levels(newbat))
  newbat_app <- factor(newbat, bat_levels)
  batches <- lapply(levels(newbat_app), function(x) which(newbat_app == x))

  # new batches to estimate/adjust
  newbat_est <- which(bat_levels %in% setdiff(levels(newbat), known))
  newbat_adj <- which(bat_levels %in% levels(newbat))

  #### Standardize the data ####
  # resize batch_mod
  batch <- matrix(batch_mod[1,], n, nlevels(bat), byrow = TRUE)

  if (is.null(newcovar)) {
    pmod <- data.frame(I(batch))
  } else {
    pmod <- data.frame(newcovar, I(batch))
  }

  stand_mean <- sapply(fits, predict, newdata = pmod, type = "response", ...)
  if (hasArg("sigma.formula")) {
    sd_mat <- sapply(fits, predict, newdata = pmod, what = "sigma",
                     type = "response", ...)
  } else {
    sd_mat <- sapply(sqrt(var_pooled), rep, n)
  }

  data_stand <- (data - stand_mean)/sd_mat

  #### Obtain location and scale adjustments ####
  # get naive estimates for new batches
  for (i in newbat_est) {
    gamma_hat <- rbind(gamma_hat,
                       apply(data_stand[batches[[i]],, drop = FALSE], 2, loc))
    delta_hat <- rbind(delta_hat,
                       apply(data_stand[batches[[i]],, drop = FALSE], 2, scl))
  }

  rownames(gamma_hat) <- rownames(delta_hat) <- bat_levels

  # Empirical Bayes adjustments for new batches
  if (eb) {
    for (i in newbat_est) {
      n_b <- length(batches[[i]])

      # method of moments estimates
      g_bar <- mean(gamma_hat[i,])
      g_var <- var(gamma_hat[i,])
      d_bar <- mean(delta_hat[i,])
      d_var <- var(delta_hat[i,])
      d_a <- (2 * d_var + d_bar^2)/d_var
      d_b <- (d_bar * d_var + d_bar^3)/d_var

      # adjust within batch
      bdat <- data_stand[batches[[i]],]
      g_orig <- gamma_hat[i,]
      g_old  <- gamma_hat[i,]
      d_old  <- delta_hat[i,]

      change_old <- 1
      change <- 1
      count  <- 0
      while(change > 10e-5){
        g_new <- (n_b*g_var*g_orig + d_old*g_bar)/(n_b*g_var + d_old)

        if (robust.LS) {
          sum2 <- (n_b-1) * sapply(1:p, function(v) {
            .biweight_midvar(bdat[,v], g_new[v])})
        } else {
          sum2   <- colSums(sweep(bdat, 2, g_new)^2)
        }

        d_new <- (sum2/2 + d_b)/(n_b/2 + d_a - 1)

        change <- max(abs(g_new - g_old)/g_old, abs(d_new - d_old)/d_old)

        if (count > 30) {
          if (change > change_old) {
            warning("Empirical Bayes step failed to converge after 30 iterations,
    	            using estimate before change between iterations increases.")
            break
          }
        }

        g_old <- g_new
        d_old <- d_new

        change_old <- change
        count <- count+1
      }

      gamma_star <- rbind(gamma_star, g_new)
      delta_star <- rbind(delta_star, d_new)
    }

    rownames(gamma_star) <- rownames(gamma_hat)
    rownames(delta_star) <- rownames(delta_hat)
  } else {
    gamma_star <- gamma_hat
    delta_star <- delta_hat
  }

  #### Harmonize the data ####
  # Remove batch effects
  data_nb <- data_stand
  for (i in newbat_adj) {
    data_nb[batches[[i]],] <- sweep(data_nb[batches[[i]],, drop = FALSE], 2,
                                    gamma_star[i,], "-")
    data_nb[batches[[i]],] <- sweep(data_nb[batches[[i]],, drop = FALSE], 2,
                                    sqrt(delta_star[i,]), "/")
  }

  # Reintroduce covariate effects
  data_combat <- data_nb*sd_mat + stand_mean

  estimates <-  list(
    stand.mean = stand_mean,
    stand.sd = sd_mat,
    var.pooled = var_pooled,
    gamma.hat = gamma_hat,
    delta.hat = delta_hat,
    gamma.star = gamma_star,
    delta.star = delta_star
  )

  batch_info <- object$batch.info

  out <- list(dat.combat = data_combat, batch.info = batch_info,
              fits = fits, estimates = estimates)
  class(out) <- "comfam"
  out
}


#' CovBat Family Harmonization
#'
#' Implementation of the CovBat Family of harmonization methods allowing for
#' removal of multivariate batch effects, flexible covariate modeling and
#' alternative estimators for site effect adjustment. Support for modeling of
#' both location and scale via GAMLSS. Additional support for modeling of
#' covariate effects in score location and scale.
#'
#' @param data \emph{n x p} data frame or matrix of observations where
#'   \emph{p} is the number of features and \emph{n} is the number of subjects.
#' @param bat Factor indicating batch (often equivalent to site or scanner)
#' @param covar Data frame or matrix of covariates supplied to `model`
#' @param model Model function. ComBat Family supports any models that take
#'   arguments `formula` and `data`, but are limited to models fitting with
#'   identity link (e.g. `family = gaussian(link = "identity")`). This includes
#'   \link[stats]{lm}, \link[mgcv]{gam}, \link[gamlss]{gamlss},
#'   \link[quantreg]{rq}, \link[lme4]{lmer}, and more
#' @param formula Formula for `model` starting with `y ~` where `y` represents
#'   each feature
#' @param score.model Model for scores, defaults to NULL for fitting basic
#'   location and scale model without covariates on the scores
#' @param score.args List of arguments for score model
#' @param eb If \code{TRUE}, uses ComBat model with empirical Bayes for mean
#'   and variance harmonization.
#' @param robust.LS If \code{TRUE}, uses robust location and scale estimators
#'   for error variance and site effect parameters. Uses median and
#'   biweight midvariance
#' @param ref.batch Reference batch, must take value in `levels(bat)`
#' @param percent.var Numeric. The number of harmonized principal component
#'    scores is selected to explain this proportion of the variance
#' @param n.pc Optional numeric. If specified, this number of principal
#'    component scores is harmonized. Overrides \code{percent.var}
#' @param std.var If \code{TRUE}, scales variances to be equal to 1 before PCA.
#' @param ... Additional arguments to `model`
#'
#' @return `covfam` returns a list containing the following components:
#' \item{dat.covbat}{Harmonized data as a matrix with same dimensions as `data`}
#' \item{batch.info}{Batch information, including reference batch if specified}
#' \item{combat.out}{List output of `comfam` from the ComBat step}
#' \item{pc.output}{Output of `prcomp` from PCA step}
#' \item{n.pc}{Numeric, number of PCs harmonized}
#' \item{scores.com}{List output of `comfam` from the CovBat step}
#'
#' @export
#'
#' @examples
#' covfam(iris[,1:2], iris$Species)
#' covfam(iris[,1:2], iris$Species, iris[3:4], lm, y ~ Petal.Length + Petal.Width)
covfam <- function(data, bat, covar = NULL, model = lm, formula = NULL,
                   score.model = NULL, score.args = NULL, eb = TRUE,
                   robust.LS = FALSE, ref.batch = NULL, percent.var = 0.95,
                   n.pc = NULL, std.var = TRUE, ...)
{
  data <- as.matrix(data)
  n <- nrow(data)
  p <- ncol(data)

  #### Remove mean/variance effects ####
  com_out <- comfam(data, bat, covar, model, formula, eb, robust.LS, ref.batch,
                    ...)
  com_res <- com_out$dat.combat - com_out$estimates$stand.mean

  #### Adjust for multivariate batch effects via PCA ####
  d_pc <- prcomp(com_res, center = TRUE, scale. = std.var)

  # Only adjust PCs specified via percent.var or npc
  if (!is.null(n.pc)) {
    npc <- n.pc
  } else {
    npc <- which(cumsum(d_pc$sdev^2/sum(d_pc$sdev^2)) > percent.var)[1]
  }
  scores <- d_pc$x[,1:npc]

  # ComBat without covariates to remove site effect in score mean/variance
  # If score.model specified, fits that model instead
  if (is.null(score.model)) {
    scores_com <- comfam(scores, bat, eb = FALSE, ref.batch = ref.batch)
  } else {
    scores_com <- do.call(comfam, c(list(scores, bat, covar,
                                         model = score.model, eb = FALSE,
                                         ref.batch = ref.batch), score.args))
  }
  full_scores <- d_pc$x
  full_scores[,1:npc] <- scores_com$dat.combat

  #### Project scores back into observation space ####
  if (std.var) {
    data_covbat <- full_scores %*% t(d_pc$rotation) *
      matrix(d_pc$scale, n, p, byrow = TRUE) +
      matrix(d_pc$center, n, p, byrow = TRUE)
  } else {
    data_covbat <- full_scores %*% t(d_pc$rotation) +
      matrix(d_pc$center, n, p, byrow = TRUE)
  }

  # Reintroduce covariate effects
  data_covbat <- data_covbat + com_out$estimates$stand.mean

  batch_info <- list(
    batch = bat,
    levels = levels(bat)
  )
  batch_info$ref.batch <- ref.batch

  out <- list(dat.covbat = data_covbat, batch.info = batch_info,
              combat.out = com_out, pc.output = d_pc, n.pc = npc,
              scores.combat = scores_com)
  class(out) <- c("covfam")
  out
}


