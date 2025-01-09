library(testthat)

test_that("Data preparation function works correctly", {
  features <- colnames(adni)[c(43:104)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  ## Harmonization Data Preparation
  ### Without result
  prep_result <- data_prep(stage = "harmonization", result = NULL, features = features,
            batch = batch, covariates = covariates, df = adni, type = "lm", random = NULL,
            smooth = NULL, interaction = interaction, smooth_int_type = NULL, predict = FALSE, object = NULL)
  expect_type(prep_result, "list")
  expect_equal(length(prep_result), 14)
  expect_equal(prep_result$batch, batch)
  expect_equal(prep_result$features, features)
  expect_equal(prep_result$type, "lm")
  expect_equal(prep_result$covariates, covariates)
  expect_equal(prep_result$interaction, "timedays:DIAGNOSIS")
  expect_equal(prep_result$random, NULL)
  expect_equal(prep_result$smooth, NULL)
  expect_equal(dim(prep_result$df), c(2515, 104))
  expect_equal(prep_result$cov_shiny, covariates)
  expect_equal(prep_result$char_var, c("SEX", "DIAGNOSIS"))
  expect_equal(prep_result$smooth_int_type, NULL)
  expect_equal(prep_result$interaction_orig, "timedays,DIAGNOSIS")
  expect_equal(prep_result$smooth_orig, NULL)
  expect_equal(dim(prep_result$summary_df), c(3, 4))

  prep_result_lmer <- data_prep(stage = "harmonization", result = NULL, features = features,
                           batch = batch, covariates = covariates, df = adni, type = "lmer", random = "subid",
                           smooth = NULL, interaction = interaction, smooth_int_type = NULL, predict = FALSE, object = NULL)
  expect_type(prep_result_lmer, "list")
  expect_equal(length(prep_result_lmer), 14)
  expect_equal(prep_result_lmer$batch, batch)
  expect_equal(prep_result_lmer$features, features)
  expect_equal(prep_result_lmer$type, "lmer")
  expect_equal(prep_result_lmer$covariates, covariates)
  expect_equal(prep_result_lmer$interaction, "timedays:DIAGNOSIS")
  expect_equal(prep_result_lmer$random, "subid")
  expect_equal(prep_result_lmer$smooth, NULL)
  expect_equal(dim(prep_result_lmer$df), c(2515, 104))
  expect_equal(prep_result_lmer$cov_shiny, covariates)
  expect_equal(prep_result_lmer$char_var, c("SEX", "DIAGNOSIS"))
  expect_equal(prep_result_lmer$smooth_int_type, NULL)
  expect_equal(prep_result_lmer$interaction_orig, "timedays,DIAGNOSIS")
  expect_equal(prep_result_lmer$smooth_orig, NULL)
  expect_equal(dim(prep_result_lmer$summary_df), c(3, 4))

  prep_result_gam <- data_prep(stage = "harmonization", result = NULL, features = features,
                                batch = batch, covariates = covariates, df = adni, type = "gam", random = NULL,
                                smooth = "AGE", interaction = interaction, smooth_int_type = "linear", predict = FALSE, object = NULL)
  expect_type(prep_result_gam, "list")
  expect_equal(length(prep_result_gam), 14)
  expect_equal(prep_result_gam$batch, batch)
  expect_equal(prep_result_gam$features, features)
  expect_equal(prep_result_gam$type, "gam")
  expect_equal(prep_result_gam$covariates, c("timedays", "SEX", "DIAGNOSIS"))
  expect_equal(prep_result_gam$interaction, "timedays:DIAGNOSIS")
  expect_equal(prep_result_gam$random, NULL)
  expect_equal(prep_result_gam$smooth, "AGE")
  expect_equal(dim(prep_result_gam$df), c(2515, 104))
  expect_equal(prep_result_gam$cov_shiny, covariates)
  expect_equal(prep_result_gam$char_var, c("SEX", "DIAGNOSIS"))
  expect_equal(prep_result_gam$smooth_int_type, "linear")
  expect_equal(prep_result_gam$interaction_orig, "timedays,DIAGNOSIS")
  expect_equal(prep_result_gam$smooth_orig, "AGE")
  expect_equal(dim(prep_result_gam$summary_df), c(3, 4))

  result <- readRDS("previous-results/lm_result.rds")

  ### With Result
  prep_result_w <- data_prep(stage = "harmonization", result = result, type = "lm", random = NULL,
                           smooth = NULL, interaction = interaction, smooth_int_type = NULL, predict = FALSE, object = NULL)
  expect_identical(prep_result, prep_result_w)

  ### With Existing ComBat Model
  saved_model <- readRDS("previous-results/saved_combat_lm_model.rds")
  prep_model_w <- data_prep(stage = "harmonization", df = adni, predict = TRUE, object = saved_model)
  expect_identical(prep_result$batch, prep_model_w$batch)
  expect_identical(prep_result$features, prep_model_w$features)
  expect_identical(prep_result$type, prep_model_w$type)
  expect_identical(prep_result$covariates, prep_model_w$covariates)

  saved_model_lmer <- readRDS("previous-results/combat_model_lmer.rds")
  saved_model_lmer <- saved_model_lmer$combat.object
  prep_model_w_lmer <- data_prep(stage = "harmonization", df = adni, predict = TRUE, object = saved_model_lmer)
  expect_identical(prep_result_lmer$batch, prep_model_w_lmer$batch)
  expect_identical(prep_result_lmer$features, prep_model_w_lmer$features)
  expect_identical(prep_result_lmer$type, prep_model_w_lmer$type)
  expect_identical(prep_result_lmer$covariates, prep_model_w_lmer$covariates)

  saved_model_gam <- readRDS("previous-results/combat_model_gam.rds")
  saved_model_gam <- saved_model_gam$combat.object
  prep_model_w_gam <- data_prep(stage = "harmonization", df = adni, predict = TRUE, object = saved_model_gam)
  expect_identical(prep_result_gam$batch, prep_model_w_gam$batch)
  expect_identical(prep_result_gam$features, prep_model_w_gam$features)
  expect_identical(prep_result_gam$type, prep_model_w_gam$type)
  expect_identical(c(prep_result_gam$covariates, prep_result_gam$smooth), prep_model_w_gam$covariates)

  ## Post-Harmonization Data Preparation
  prep_result_post <- data_prep(stage = "post-harmonization", result = NULL, features = features,
                           covariates = covariates, df = adni, type = "lm", random = NULL,
                           smooth = NULL, interaction = interaction, smooth_int_type = NULL, predict = FALSE, object = NULL)
  expect_type(prep_result_post, "list")
  expect_equal(length(prep_result_post), 12)
  expect_equal(prep_result_post$features, features)
  expect_equal(prep_result_post$type, "lm")
  expect_equal(prep_result_post$covariates, covariates)
  expect_equal(prep_result_post$interaction, "timedays:DIAGNOSIS")
  expect_equal(prep_result_post$random, NULL)
  expect_equal(prep_result_post$smooth, NULL)
  expect_equal(dim(prep_result_post$df), c(2515, 104))
  expect_equal(prep_result_post$cov_shiny, covariates)
  expect_equal(prep_result_post$char_var, c("SEX", "DIAGNOSIS"))
  expect_equal(prep_result_post$smooth_int_type, NULL)
  expect_equal(prep_result_post$interaction_orig, "timedays,DIAGNOSIS")
  expect_equal(prep_result_post$smooth_orig, NULL)

  prep_result_post_lmer <- data_prep(stage = "post-harmonization", result = NULL, features = features,
                                covariates = covariates, df = adni, type = "lmer", random = "subid",
                                smooth = NULL, interaction = interaction, smooth_int_type = NULL, predict = FALSE, object = NULL)

  prep_result_post_gam <- data_prep(stage = "post-harmonization", result = NULL, features = features,
                                     covariates = covariates, df = adni, type = "gam", random = NULL,
                                     smooth = "AGE", interaction = interaction, smooth_int_type = "linear", predict = FALSE, object = NULL)

  saved_model_post <- readRDS("previous-results/result_residual_model_lm.rds")
  prep_result_post_w_model <- data_prep(stage = "post-harmonization", df = adni, predict = TRUE, object = saved_model_post)
  expect_identical(prep_result_post$batch, prep_result_post_w_model$batch)
  expect_identical(prep_result_post$features, prep_result_post_w_model$features)
  expect_identical(prep_result_post$type, prep_result_post_w_model$type)
  expect_identical(prep_result_post$covariates, prep_result_post_w_model$covariates)

  saved_model_post_lmer <- readRDS("previous-results/result_residual_model_lmer.rds")
  prep_result_post_w_model_lmer <- data_prep(stage = "post-harmonization", df = adni, predict = TRUE, object = saved_model_post_lmer)
  expect_identical(prep_result_post_lmer$batch, prep_result_post_w_model_lmer$batch)
  expect_identical(prep_result_post_lmer$features, prep_result_post_w_model_lmer$features)
  expect_identical(prep_result_post_lmer$type, prep_result_post_w_model_lmer$type)
  expect_identical(prep_result_post_lmer$covariates, prep_result_post_w_model_lmer$covariates)

  saved_model_post_gam <- readRDS("previous-results/result_residual_model_gam.rds")
  prep_result_post_w_model_gam <- data_prep(stage = "post-harmonization", df = adni, predict = TRUE, object = saved_model_post_gam)
  expect_identical(prep_result_post_gam$batch, prep_result_post_w_model_gam$batch)
  expect_identical(prep_result_post_gam$features, prep_result_post_w_model_gam$features)
  expect_identical(prep_result_post_gam$type, prep_result_post_w_model_gam$type)
  expect_identical(c(prep_result_post_gam$covariates, prep_result_post_gam$smooth), prep_result_post_w_model_gam$covariates)
})

test_that("EB Assumption Check function works correctly", {
  features <- colnames(adni)[c(43:104)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  eb_result <- eb_check(data = adni[,features], bat = as.factor(adni$manufac),
  covar = adni[, covariates], model = lm, formula = y ~ AGE + SEX + timedays + DIAGNOSIS)
  expect_type(eb_result, "list")
  expect_equal(dim(eb_result), c(744, 4))
  expect_equal(levels(eb_result$batch), c("GE", "Philips", "Siemens"))
  expect_equal(unique(eb_result$features), features)
  expect_equal(unique(eb_result$type), c("gamma_hat", "gamma_prior", "delta_hat", "delta_prior"))
})

test_that("Biweight Midvariance Calculation works correctly", {
  data <- c(1, 2, 3, 4, 100)
  biweight_var <- .biweight_midvar(data)
  expect_equal(biweight_var, 1.941142, tolerance = 1e-5)
})


test_that("Model Generation function works correctly", {
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  model_form <- model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "lm",
  batch = "manufac", covariates = covariates, interaction = "timedays:DIAGNOSIS", df = adni)
  lm_model <- lm(thickness.left.caudal.anterior.cingulate ~ manufac + timedays + AGE + SEX + DIAGNOSIS + timedays:DIAGNOSIS, adni)
  expect_equal(model_form$fitted.values, lm_model$fitted.values, tolerance = 1e-5)

  model_form_wo_batch <- model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "lm",
                          batch = NULL, covariates = covariates, interaction = "timedays:DIAGNOSIS", df = adni)
  lm_model_wo_batch <- lm(thickness.left.caudal.anterior.cingulate ~ timedays + AGE + SEX + DIAGNOSIS + timedays:DIAGNOSIS, adni)
  expect_equal(model_form_wo_batch$fitted.values, lm_model_wo_batch$fitted.values, tolerance = 1e-5)

  model_form_lmer <- model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "lmer",
                          batch = "manufac", covariates = covariates, interaction = "timedays:DIAGNOSIS", random = "subid", df = adni)
  lmer_model <- lmer(thickness.left.caudal.anterior.cingulate ~ manufac + timedays + AGE + SEX + DIAGNOSIS + timedays:DIAGNOSIS + (1|subid), adni)
  expect_equal(fitted(lmer_model), fitted(model_form_lmer), tolerance = 1e-5)

  model_form_lmer_wo_batch <- model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "lmer",
                               batch = NULL, covariates = covariates, interaction = "timedays:DIAGNOSIS", random = "subid", df = adni)
  lmer_model_wo_batch <- lmer(thickness.left.caudal.anterior.cingulate ~ timedays + AGE + SEX + DIAGNOSIS + timedays:DIAGNOSIS + (1|subid), adni)
  expect_equal(fitted(lmer_model_wo_batch), fitted(model_form_lmer_wo_batch), tolerance = 1e-5)

  model_form_gam <- model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "gam",
                          batch = "manufac", covariates = covariates, interaction = "timedays:DIAGNOSIS", smooth = "AGE", df = adni)
  gam_model <- gam(thickness.left.caudal.anterior.cingulate ~ manufac + timedays + s(AGE) + SEX + DIAGNOSIS + timedays:DIAGNOSIS, data = adni)
  expect_equal(model_form_gam$fitted.values, gam_model$fitted.values, tolerance = 1e-5)

  model_form_gam_wo_batch <- model_gen(y = "thickness.left.caudal.anterior.cingulate", type = "gam",
                              batch = NULL, covariates = covariates, interaction = "timedays:DIAGNOSIS", smooth = "AGE", df = adni)
  gam_model_wo_batch <- gam(thickness.left.caudal.anterior.cingulate ~ timedays + s(AGE) + SEX + DIAGNOSIS + timedays:DIAGNOSIS, data = adni)
  expect_equal(model_form_gam_wo_batch$fitted.values, gam_model_wo_batch$fitted.values, tolerance = 1e-5)
})

test_that("ComBatFamily Model Formula Generation function works correctly", {
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  covariates <- adni[, covariates]
  combat_form <- form_gen(x = "lm", c = covariates)
  expect_type(combat_form, "character")
  expect_equal(combat_form, "y ~timedays+AGE+SEX+DIAGNOSIS")

  combat_form_lmer <- form_gen(x = "lmer", c = covariates, random = "subid")
  expect_type(combat_form_lmer, "character")
  expect_equal(combat_form_lmer, "y ~timedays + AGE + SEX + DIAGNOSIS + (1 | subid )")

  combat_form_gam<- form_gen(x = "gam", c = covariates, smooth = "AGE")
  expect_type(combat_form_gam, "character")
  expect_equal(combat_form_gam, "y ~ timedays + AGE + SEX + DIAGNOSIS + s( AGE )")
})

test_that("Interaction Term Generation function works correctly", {
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction_result_linear <- interaction_gen(type = "gam", covariates = covariates,
                  smooth = "AGE", smooth_int_type = "linear", interaction = "timedays,DIAGNOSIS")
  expect_type(interaction_result_linear, "list")
  expect_equal(interaction_result_linear$interaction, "timedays:DIAGNOSIS")
  expect_equal(interaction_result_linear$covariates, c("timedays", "SEX", "DIAGNOSIS"))
  expect_equal(interaction_result_linear$smooth, c("AGE"))

  interaction_result_cc <- interaction_gen(type = "gam", covariates = covariates,
                                               smooth = "AGE", smooth_int_type = "categorical-continuous", interaction = "AGE,DIAGNOSIS")
  expect_equal(interaction_result_cc$interaction, "s(AGE, by = DIAGNOSIS)")
  expect_equal(interaction_result_cc$covariates, c("timedays", "SEX", "DIAGNOSIS"))
  expect_equal(interaction_result_cc$smooth, NULL)

  interaction_result_fs <- interaction_gen(type = "gam", covariates = covariates,
                                           smooth = "AGE", smooth_int_type = "factor-smooth", interaction = "AGE,DIAGNOSIS")
  expect_equal(interaction_result_fs$interaction, "s( AGE,DIAGNOSIS , bs = 'fs')")
  expect_equal(interaction_result_fs$covariates, c("timedays", "SEX"))
  expect_equal(interaction_result_fs$smooth, NULL)

  interaction_result_t <- interaction_gen(type = "gam", covariates = covariates,
                                           smooth = "AGE", smooth_int_type = "tensor", interaction = "AGE,DIAGNOSIS")
  expect_equal(interaction_result_t$interaction, "ti( AGE,DIAGNOSIS )")
  expect_equal(interaction_result_t$covariates, c("timedays", "SEX", "DIAGNOSIS"))
  expect_equal(interaction_result_t$smooth, "AGE")

  interaction_result_ss <- interaction_gen(type = "gam", covariates = covariates,
                                          smooth = "AGE", smooth_int_type = "smooth-smooth", interaction = "AGE,DIAGNOSIS")
  expect_equal(interaction_result_ss$interaction, "s( AGE,DIAGNOSIS )")
  expect_equal(interaction_result_ss$covariates, c("timedays", "SEX", "DIAGNOSIS"))
  expect_equal(interaction_result_ss$smooth, NULL)
})

test_that("Exporting diagnosis result works correctly", {
  result <- readRDS("previous-results/lm_result.rds")
  temp_dir <- tempdir()
  diag_save(temp_dir, result)
  output_path <- file.path(temp_dir, "diagnosis.xlsx")
  expect_true(file.exists(output_path))
})


test_that("Exporting age trend table works correctly", {
  age_list <- readRDS("previous-results/age_list.rds")
  temp_dir <- tempdir()
  age_save(temp_dir, age_list)
  output_path <- file.path(temp_dir, "age_trend.xlsx")
  expect_true(file.exists(output_path))
})





