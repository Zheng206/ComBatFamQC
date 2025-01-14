test_that("Visualization preparation function works correctly", {
  ### Linear Diagnosis
  features <- colnames(adni)[c(43:46)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  result_orig <- visual_prep(type = "lm", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = NULL, df = adni, cores = 1)
  saved_result_lm <- readRDS("previous-results/lm_result.rds")
  expect_equal(result_orig$residual_add_df, saved_result_lm$residual_add_df, tolerance = 1e-8)
  expect_equal(result_orig$residual_ml_df, saved_result_lm$residual_ml_df, tolerance = 1e-8)
  expect_equal(result_orig$mdmr.summary, saved_result_lm$mdmr.summary, tolerance = 1e-8)
  expect_equal(result_orig$anova_test_df, saved_result_lm$anova_test_df, tolerance = 1e-8)
  expect_equal(result_orig$lv_test_df, saved_result_lm$lv_test_df, tolerance = 1e-8)

  result_lmer <- visual_prep(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni, cores = 1)
  expect_type(result_lmer, "list")


  result_gam <- visual_prep(type = "gam", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = "AGE", smooth_int_type = "linear", random = NULL, df = adni, cores = 1)
  expect_type(result_gam, "list")
})
