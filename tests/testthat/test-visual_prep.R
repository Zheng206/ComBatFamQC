test_that("Visualization preparation function works correctly", {
  ### Linear Diagnosis
  features <- colnames(adni)[c(43:104)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  result_orig <- visual_prep(type = "lm", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = NULL, df = adni, cores = 2)
  saved_result_lm <- readRDS("previous-results/lm_result.rds")
  expect_identical(result_orig$residual_add_df, saved_result_lm$residual_add_df)
  expect_identical(result_orig$residual_ml_df, saved_result_lm$residual_ml_df)
  expect_identical(result_orig$mdmr.summary, saved_result_lm$mdmr.summary)
  expect_identical(result_orig$anova_test_df, saved_result_lm$anova_test_df)
  expect_identical(result_orig$lv_test_df, saved_result_lm$lv_test_df)

  result_lmer <- visual_prep(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni, cores = 2)
  saved_result_lmer <- readRDS("previous-results/lmer_result.rds")
  expect_identical(result_lmer$residual_add_df, saved_result_lmer$residual_add_df)
  expect_identical(result_lmer$residual_ml_df, saved_result_lmer$residual_ml_df)
  expect_identical(result_lmer$mdmr.summary, saved_result_lmer$mdmr.summary)
  expect_identical(result_lmer$anova_test_df, saved_result_lmer$anova_test_df)
  expect_identical(result_lmer$lv_test_df, saved_result_lmer$lv_test_df)

  result_gam <- visual_prep(type = "gam", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = "AGE", smooth_int_type = "linear", random = NULL, df = adni, cores = 2)
  saved_result_gam <- readRDS("previous-results/gam_result.rds")
  expect_identical(result_gam$residual_add_df, saved_result_gam$residual_add_df)
  expect_identical(result_gam$residual_ml_df, saved_result_gam$residual_ml_df)
  expect_identical(result_gam$mdmr.summary, saved_result_gam$mdmr.summary)
  expect_identical(result_gam$anova_test_df, saved_result_gam$anova_test_df)
  expect_identical(result_gam$lv_test_df, saved_result_gam$lv_test_df)
})
