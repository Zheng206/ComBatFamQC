test_that("Harmonization functions work correctly", {
  features <- colnames(adni)[c(43:46)]
  covariates <- c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction <- c("timedays,DIAGNOSIS")
  batch <- "manufac"
  ## First-time Harmonization
  ### EB Check
  eb_result <- combat_harm(eb_check = TRUE, type = "lm", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = NULL, df = adni)
  expect_equal(dim(eb_result), c(48, 4))

  ### Original ComBat
  combat_model_lm <- combat_harm(type = "lm", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = NULL, df = adni)
  harm_lm <- readRDS("previous-results/harm_lm.rds")
  expect_equal(combat_model_lm$harmonized_df[features], harm_lm[features], tolerance = 1e-8)
  prep_model_w <- data_prep(stage = "harmonization", df = adni, predict = TRUE, object = combat_model_lm$combat.object)
  expect_type(prep_model_w, "list")

  ### Longitudinal ComBat
  combat_model_lmer <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni)
  expect_type(combat_model_lmer, "list")
  prep_model_w_lmer <- data_prep(stage = "harmonization", df = adni, predict = TRUE, object = combat_model_lmer$combat.object)
  expect_type(prep_model_w_lmer, "list")

  ### ComBat-GAM
  combat_model_gam <- combat_harm(type = "gam", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = "AGE", smooth_int_type = "linear", df = adni)
  expect_type(combat_model_gam, "list")
  prep_model_w_gam <- data_prep(stage = "harmonization", df = adni, predict = TRUE, object = combat_model_gam$combat.object)
  expect_type(prep_model_w_gam, "list")

  ### CovBat
  covbat_model <- combat_harm(type = "gam", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth_int_type = "linear", smooth = "AGE", df = adni, family = "covfam")
  expect_type(covbat_model, "list")

  ## Out-of-Sample Harmonization
  ### Existing Model
  saved_model <- combat_model_gam$combat.object
  predict_model <- combat_harm(df = adni %>% head(1000), predict = TRUE, object = saved_model)
  expect_type(predict_model, "list")

  ### Reference Model
  #### harmonize reference data
  reference_site <- adni %>% group_by(site) %>% summarize(count = n()) %>% arrange(desc(count)) %>% pull(site) %>% head(30)
  reference_df <- adni %>% filter(site %in% reference_site)
  features = colnames(reference_df)[c(43:46)]
  covariates = c("timedays", "AGE", "SEX", "DIAGNOSIS")
  interaction = c("timedays,DIAGNOSIS")
  batch = "site"
  ref_model <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = reference_df)
  ref_model_cov <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = reference_df, family = "covfam")
  #### harmonize new data to the reference data
  reference_model <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni, reference = ref_model$harmonized_df, eb = FALSE)
  reference_model_cov <- combat_harm(type = "lmer", features = features, batch = batch, covariates = covariates, interaction = interaction, smooth = NULL, random = "subid", df = adni, reference = ref_model_cov$harmonized_df, family = "covfam", eb = FALSE)
  expect_type(reference_model, "list")
  expect_type(reference_model, "list")
})


test_that("Prediction functions work correctly", {
  com_out <- comfam(iris[1:75,1:2], iris$Species[1:75])
  out_pred <- predict(com_out, iris[76:150,1:2], iris$Species[76:150])
  expect_type(out_pred, "list")
})
