test_that("Harmonization functions work correctly", {
  features <- colnames(adni)[c(43:104)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  ## First-time Harmonization
  ### EB Check
  eb_result <- combat_harm(eb_check = TRUE, type = "lm", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = NULL, df = adni)
  expect_equal(dim(eb_result), c(744, 4))
  ### Original ComBat
  combat_model_lm <- combat_harm(type = "lm", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = NULL, df = adni)
  saved_combat_model_lm <- readRDS("previous-results/combat_model_lm.rds")
  expect_identical(combat_model_lm$harmonized_df, saved_combat_model_lm$harmonized_df)

  ### Longitudinal ComBat
  combat_model_lmer <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni)
  saved_combat_model_lmer <- readRDS("previous-results/combat_model_lmer.rds")
  expect_identical(combat_model_lmer$harmonized_df, saved_combat_model_lmer$harmonized_df)

  ### ComBat-GAM
  combat_model_gam <- combat_harm(type = "gam", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = "AGE", smooth_int_type = "linear", df = adni)
  saved_combat_model_gam <- readRDS("previous-results/combat_model_gam.rds")
  expect_identical(combat_model_gam$harmonized_df, saved_combat_model_gam$harmonized_df)

  ### CovBat
  covbat_model <- combat_harm(type = "gam", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth_int_type = "linear", smooth = "AGE", df = adni, family = "covfam")
  saved_covbat_model <- readRDS("previous-results/covbat_model.rds")
  expect_identical(covbat_model$harmonized_df, saved_covbat_model$harmonized_df)

  ## Out-of-Sample Harmonization
  ### Existing Model
  saved_model <- readRDS("previous-results/saved_combat_gam_model.rds")
  predict_model <- combat_harm(df = adni %>% head(1000), predict = TRUE, object = saved_model)
  saved_predict_model <- readRDS("previous-results/predict_model.rds")
  expect_identical(predict_model$harmonized_df, saved_predict_model$harmonized_df)

  ### Reference Model
  #### harmonize reference data
  reference_site <- adni %>% group_by(site) %>% summarize(count = n()) %>% arrange(desc(count)) %>% pull(site) %>% head(30)
  reference_df <- adni %>% filter(site %in% reference_site)
  features = colnames(reference_df)[c(43:104)]
  covariates = c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction = c("timedays,DIAGNOSIS")
  batch = "site"
  ref_model <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = reference_df)
  ref_model_cov <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = reference_df, family = "covfam")
  #### harmonize new data to the reference data
  reference_model <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni, reference = ref_model$harmonized_df)
  reference_model_cov <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni, reference = ref_model_cov$harmonized_df, family = "covfam")
  saved_reference_model <- readRDS("previous-results/reference_model.rds")
  saved_reference_model_cov <- readRDS("previous-results/reference_model_cov.rds")
  expect_identical(reference_model$harmonized_df, saved_reference_model$harmonized_df)
  expect_identical(reference_model_cov$harmonized_df, saved_reference_model_cov$harmonized_df)
})
