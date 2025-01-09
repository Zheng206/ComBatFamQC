test_that("Residual generation works correctly", {
  ## Fit New Model
  features <- colnames(adni)[c(43:104)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  combat_model_lm <- readRDS("previous-results/combat_model_lm.rds")
  harmonized_df <- combat_model_lm$harmonized_df
  result_residual <- residual_gen(type = "lm", features = features, covariates = covariates, interaction = interaction, smooth = NULL, df = harmonized_df, rm = c("timedays", "DIAGNOSIS"), cores = 1)
  saved_result_residual <- readRDS("previous-results/result_residual_lm.rds")
  expect_identical(result_residual$residual, saved_result_residual$residual)

  combat_model_lmer <- readRDS("previous-results/combat_model_lmer.rds")
  result_residual_lmer <- residual_gen(type = "lmer", features = features, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = combat_model_lmer$harmonized_df, rm = c("timedays", "DIAGNOSIS"), cores = 1)
  saved_result_residual_lmer <- readRDS("previous-results/result_residual_lmer.rds")
  expect_identical(result_residual_lmer$residual, saved_result_residual_lmer$residual)

  combat_model_gam <- readRDS("previous-results/combat_model_gam.rds")
  result_residual_gam <- residual_gen(type = "gam", features = features, covariates = covariates, interaction = interaction, smooth = "AGE", smooth_int_type = "linear", df = combat_model_gam$harmonized_df, rm = c("timedays", "DIAGNOSIS"), cores = 1)
  saved_result_residual_gam <- readRDS("previous-results/result_residual_gam.rds")
  expect_identical(result_residual_gam$residual, saved_result_residual_gam$residual)

  ## Use Saved Model
  result_residual_saved_model <- residual_gen(model = TRUE, model_path = "previous-results/result_residual_model_lm.rds", df = harmonized_df, rm = c("timedays", "DIAGNOSIS", "SEX"), cores = 1)
  result_residual_new_model <- residual_gen(type = "lm", features = features, covariates = covariates, interaction = interaction, smooth = NULL, df = harmonized_df, rm = c("timedays", "DIAGNOSIS", "SEX"), cores = 1)
  expect_identical(result_residual_saved_model$residual, result_residual_new_model$residual)
})
