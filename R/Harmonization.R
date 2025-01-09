#' ComBatFamily Harmonization
#'
#' Conduct harmonization using the techniques from the ComBatFamily package, which currently includes four types of harmonization methods: 1) Original ComBat, 2) Longitudinal ComBat, 3) ComBat-GAM, 4) CovBat.
#'
#' @param eb_check A boolean variable indicating whether the user wants to run the EB assumption test before harmonization.
#' @param result A list derived from `visual_prep()` that contains dataset and batch effect diagnostic information for Shiny visualization. Can be skipped if `features`, `batch`, `covariates` and `df` are provided.
#' @param features The name of the features to be harmonized. This can be skipped if `result` is provided.
#' @param batch The name of the batch variable. Can be skipped if `result` is provided.
#' @param covariates The names of covariates supplied to `model`. This can be be skipped if `result` is provided.
#' @param df Dataset to be harmonized. This can be be skipped if `result` is provided.
#' @param type The name of a regression model to be used in the ComBatFamily package: "lmer", "lm", "gam".
#' @param random The variable name of a random effect in linear mixed effect model.
#' @param smooth The name of the covariates that require a smooth function.
#' @param interaction Expression of interaction terms supplied to `model` (eg: "age,diagnosis").
#' @param smooth_int_type A vector that indicates the types of interaction in `gam` models. By default, smooth_int_type is set to be NULL, "linear" represents linear interaction terms.
#' "categorical-continuous", "factor-smooth" both represent categorical-continuous interactions ("factor-smooth" includes categorical variable as part of the smooth),
#' "tensor" represents interactions with different scales, and "smooth-smooth" represents interaction between smoothed variables.
#' @param family The type of combat family to use, comfam or covfam.
#' @param eb If \code{TRUE}, uses ComBat model with empirical Bayes for mean and variance harmonization
#' @param ref.batch The name of the reference batch.
#' @param predict A boolean variable indicating whether to run ComBat from scratch or apply existing model to new dataset (currently only work for original ComBat and ComBat-GAM).
#' @param object Existing ComBat model.
#' @param reference Dataset to be considered as the reference group.
#' @param out_ref_include A boolean variable indicating whether the reference data should be included in the harmonized data output.
#' @param ... Additional arguments to `comfam` or `covfam` models from the ComBatFamily package.
#'
#' @return If the `eb_check` is set to be FALSE, then `combat_harm` returns a list containing the following components:
#' \item{com_family}{ComBat family to be considered: comfam, covfam}
#' \item{harmonized_df}{Harmonized dataset}
#' \item{combat.object}{Saved ComBat model and relevant information, such as the batch variable name and whether the EB method is used}
#' If `eb_check`  is set to be TRUE, then `combat_harm` will return a dataframe with the EB assumption test result.
#'
#' @import ComBatFamily
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
          ComBat_run <- ComBatFamily::comfam(data = df[features],
                                            bat = df[[batch]],
                                            covar = combat_c,
                                            model = eval(parse(text = type)),
                                            formula = as.formula(form),
                                            ref.batch = ref.batch,
                                            eb = eb,
                                            ...)
        }else{
          ComBat_run <- ComBatFamily::covfam(data = df[features],
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
        ComBat_run <- ComBatFamily::comfam(data = df_c[features],
                                          bat = df_c[[batch]],
                                          covar = combat_c,
                                          model = eval(parse(text = type)),
                                          formula = as.formula(form),
                                          ref.batch = "reference",
                                          eb = eb,
                                          ...)
      }else{
        ComBat_run <- ComBatFamily::covfam(data = df_c[features],
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
