library(shinytest2)
test_that("Launch Shiny App without error", {
  result <- readRDS("previous-results/lm_result.rds")
  # Launch the Shiny app
  app <- AppDriver$new(comfam_shiny(result), name = "comfam_shiny", variant = platform_variant())

  # Check if the app launched without errors
  expect_true(!is.null(app))

  # 1. Test initial UI loading
  app$set_inputs(tabselected = "1", wait_ = FALSE)
  app$expect_screenshot(name = "tab1_initial")
  expect_true(!is.null(app$get_value(output = "data_usage_explain")))

  # Test if data overview loads
  app$set_inputs(data_view = "Exploratory Analysis")
  app$set_inputs(single_feature = "thickness.left.caudal.anterior.cingulate", single_cov = "AGE")
  expect_equal(app$get_value(input = "data_view"), "Exploratory Analysis")
  expect_equal(app$get_value(input = "num_var_control_batch"), "No")
  expect_true(!is.null(app$get_value(output = "batch_vi")))
  expect_true(!is.null(app$get_value(output = "cov_vi")))
  app$expect_screenshot(name = "data_overview_exploration")

  # 3. Test Summary Tab
  app$set_inputs(tabselected = "2", type = "Plot", timeout_ = 8000, wait_ = TRUE)
  expect_true(!is.null(app$get_value(output = "output")))
  app$set_inputs(type = "Table")
  expect_true(!is.null(app$get_value(output = "table")))
  app$expect_screenshot(name = "summary_tab")

  # 4. Test Residual Plot tab
  app$set_inputs(tabselected = "3", resid_all = "Yes", resid_color = "No", timeout_ = 8000, wait_ = TRUE)
  expect_true(!is.null(app$get_value(output = "res_add")))
  expect_true(!is.null(app$get_value(output = "res_ml")))
  app$expect_screenshot(name = "residual_plot")

  # 5. Test PCA selection and plotting in Global Batch Effect Diagnosis
  app$set_inputs(tabselected = "4", PC1 = "PC1", PC2 = "PC2", pca_all = "Yes", pca_label = "No", timeout_ = 8000, wait_ = TRUE)
  expect_true(!is.null(app$get_value(output = "pca")))
  app$expect_screenshot(name = "global_batch_effect_pca")

  # 6. Test Individual Batch Effect Diagnosis
  app$set_inputs(tabselected = "5", test_batch = "ANOVA", test_variance = "Fligner-Killeen", timeout_ = 8000, wait_ = TRUE)
  expect_true(!is.null(app$get_value(output = "test_batch_ui")))
  expect_true(!is.null(app$get_value(output = "test_variance_ui")))
  app$expect_screenshot(name = "individual_batch_effect")

  # 7. Test Harmonization tab
  app$set_inputs(tabselected = "6", com_type = "comfam", com_model = "gam", eb_control = "Yes", ref_bat_select = "Batch1", timeout_ = 8000, wait_ = TRUE)
  #expect_true(!is.null(app$get_value(output = "eb_location")))
  app$expect_screenshot(name = "harmonization_tab")


  # Stop the app
  app$stop()
})
