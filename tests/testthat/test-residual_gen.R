test_that("Residual generation works correctly", {
  ## Fit New Model
  features <- colnames(adni)[c(43:46)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  combat_model_lm <- combat_harm(type = "lm", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = NULL, df = adni)
  harmonized_df <- combat_model_lm$harmonized_df
  result_residual <- residual_gen(type = "lm", features = features, covariates = covariates, interaction = interaction, smooth = NULL, df = harmonized_df, rm = c("timedays", "DIAGNOSIS"), cores = 1)
  expect_type(result_residual, "list")

  result_residual_lmer <- residual_gen(type = "lmer", features = features, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = harmonized_df, rm = c("timedays", "DIAGNOSIS"), cores = 1)
  expect_type(result_residual_lmer, "list")
  prep_result_post_w_model_lmer <- data_prep(stage = "post-harmonization", df = adni, predict = TRUE, object = result_residual_lmer$model)
  expect_type(prep_result_post_w_model_lmer, "list")

  result_residual_gam <- residual_gen(type = "gam", features = features, covariates = covariates, interaction = interaction, smooth = "AGE", smooth_int_type = "linear", df = harmonized_df, rm = c("timedays", "DIAGNOSIS"), cores = 1)
  expect_type(result_residual_gam, "list")
  prep_result_post_w_model_gam <- data_prep(stage = "post-harmonization", df = adni, predict = TRUE, object = result_residual_gam$model)
  expect_type(prep_result_post_w_model_gam, "list")


  ## Use Saved Model
  result_residual_saved_model <- residual_gen(model = TRUE, model_path = "previous-results/result_residual_model_lm.rds", df = harmonized_df, rm = c("timedays", "DIAGNOSIS", "SEX"), cores = 1)
  result_residual_new_model <- residual_gen(type = "lm", features = features, covariates = covariates, interaction = interaction, smooth = NULL, df = harmonized_df, rm = c("timedays", "DIAGNOSIS", "SEX"), cores = 1)
  expect_equal(result_residual_saved_model$residual, result_residual_new_model$residual, tolerance = 1e-8)
})
