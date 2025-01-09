library(shinytest2)
test_that("Launch Shiny App without error", {
  age_list <- readRDS("previous-results/age_list.rds")

  # Launch the Shiny app
  app <- AppDriver$new(age_shiny(age_list = age_list, features = names(age_list), quantile_type = c(paste0("quantile_", 100*0.25), "median", paste0("quantile_", 100*0.75))), name = "age_shiny", variant = platform_variant())

  # Check if the app launched without errors
  expect_true(!is.null(app))

  # 1. Verify UI loads correctly
  expect_true(!is.null(app$get_value(input = "features")))
  expect_true(!is.null(app$get_value(input = "quantile")))
  app$expect_screenshot(name = "ui_initial")

  # 2. Test input interactions
  app$set_inputs(features = "Volume_1", quantile = "quantile_75", sex = "Female", timeout_ = 8000, wait_ = TRUE)
  expect_equal(app$get_value(input = "features"), "Volume_1")
  expect_equal(app$get_value(input = "quantile"), "quantile_75")
  app$expect_screenshot(name = "plot_female")

  app$set_inputs(sex = "Male")
  app$expect_screenshot(name = "plot_male")

  app$set_inputs(sex = "Female vs. Male (Only for visualization)")
  app$expect_screenshot(name = "plot_female_vs_male")

  # 3. Test age table output
  expect_true(!is.null(app$get_value(output = "agetable")))
  app$expect_screenshot(name = "table_output")

  # 4. Test exporting functionality
  temp_folder <- tempfile()
  dir.create(temp_folder)
  app$set_inputs(age_save_path = temp_folder)
  app$click("Export")
  #Sys.sleep(10)  # Wait for export to complete
  #temp_file <- file.path(temp_folder, "age_trend.xlsx")
  #expect_true(file.exists(temp_file))
  app$expect_screenshot(name = "export_success")

  temp_gamlss_path <- tempfile(fileext = ".rds")
  app$set_inputs(gamlss_save_path = temp_gamlss_path)
  app$click("gamlss_model")


  # Stop the apps
  app$stop()


})


test_that("Age dataframe generated correctly", {
  features <- colnames(age_df)[c(6:16)]
  age <- "age"
  sex <- "sex"
  icv <- "ICV_baseline"
  age_df[[sex]] <- as.factor(age_df[[sex]])


  age_sub_df <- age_df[,c(features[1], age, sex, icv)] %>% na.omit()
  colnames(age_sub_df) <- c("y", "age", "sex", "icv")

  age_sub <- age_list_gen(sub_df = age_sub_df,  lq = 0.25, hq = 0.75)

  saved_age_list <- readRDS("previous-results/age_list.rds")
  expect_identical(age_sub$predicted_df_sex, saved_age_list[[1]]$predicted_df_sex)
})
