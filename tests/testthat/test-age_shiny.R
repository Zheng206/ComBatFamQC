library(shinytest2)
test_that("Launch Shiny App without error", {
  age_list <- readRDS("previous-results/age_list.rds")

  # Launch the Shiny app
  app <- AppDriver$new(age_shiny(age_list = age_list, features = names(age_list), quantile_type = c(paste0("quantile_", 100*0.25), "median", paste0("quantile_", 100*0.75))), name = "age_shiny", variant = platform_variant(), load_timeout = 30000,
                       shiny_args = list(port = 8080))

  # Check if the app launched without errors
  expect_true(!is.null(app))

  # 1. Verify UI loads correctly
  app$set_window_size(width = 1619, height = 992)
  expect_true(!is.null(app$get_value(input = "features")))
  expect_true(!is.null(app$get_value(input = "quantile")))

  # 2. Test input interactions
  app$set_inputs(features = "Volume_1", quantile = "quantile_75", sex = "Female", timeout_ = 8000, wait_ = TRUE)
  expect_equal(app$get_value(input = "features"), "Volume_1")
  expect_equal(app$get_value(input = "quantile"), "quantile_75")
  Sys.sleep(1.8)
  app$expect_screenshot()

  app$set_inputs(sex = "Male")
  Sys.sleep(1.8)
  app$expect_screenshot()

  app$set_inputs(sex = "Female vs. Male (Only for visualization)")
  Sys.sleep(1.8)
  app$expect_screenshot()

  app$set_inputs(quantile = "customization")
  app$set_inputs(quantile_selection = 0.75)
  app$wait_for_value(input = "quantile_selection", ignore = NULL)
  app$expect_screenshot()
  app$set_inputs(quantile_selection = 0.1)
  app$wait_for_value(input = "quantile_selection", ignore = NULL)
  app$wait_for_value(output = "agetable", timeout = 10)
  app$expect_screenshot()

  app$set_inputs(sex = "Male")
  app$wait_for_value(output = "agetable", timeout = 10)
  app$expect_screenshot()

  app$set_inputs(sex = "Female vs. Male (Only for visualization)")
  app$wait_for_value(output = "agetable", timeout = 10)
  app$expect_screenshot()

  # 3. Test age table output
  expect_true(!is.null(app$get_value(output = "agetable")))

  # 4. Test exporting functionality
  temp_folder <- tempfile()
  dir.create(temp_folder)
  app$set_inputs(age_save_path = temp_folder)
  app$click("Export")
  Sys.sleep(1.8)
  app$expect_screenshot()

  temp_gamlss_path <- tempfile(fileext = ".rds")
  app$set_inputs(gamlss_save_path = temp_gamlss_path)
  app$click("gamlss_model")

  # Stop the apps
  app$stop()
})

test_that("Age dataframe generated correctly", {
  features <- colnames(age_df)[c(6:8)]
  age <- "age"
  sex <- "sex"
  icv <- "ICV_baseline"
  age_df[[sex]] <- as.factor(age_df[[sex]])


  age_sub_df <- age_df[,c(features[1], age, sex, icv)] %>% na.omit()
  colnames(age_sub_df) <- c("y", "age", "sex", "icv")

  age_sub <- age_list_gen(sub_df = age_sub_df,  lq = 0.25, hq = 0.75)

  saved_age_list <- readRDS("previous-results/age_list.rds")
  expect_equal(age_sub$predicted_df_sex, saved_age_list[[1]]$predicted_df_sex, tolerance = 1e-8)

  age_sub_1 <- age_list_gen(sub_df = age_sub_df,  lq = 0.25, hq = 0.75, mu = "linear", sigma = "linear", tau = "smooth", nu = "smooth")
  expect_type(age_sub_1, "list")
})

test_that("Age trend quantile generated correctly", {
  age_list <- readRDS("previous-results/age_list.rds")
  age_result <- customize_percentile(age_list, "Volume_2", 0.3, "F")
  expect_type(age_result, "list")
})

test_that("Age trend plot generated correctly", {
  age_list <- readRDS("previous-results/age_list.rds")
  plotly_package <- requireNamespace("plotly", quietly = TRUE)
  if(plotly_package){
    base_plot <- age_trend_plot(age_list, f = "Volume_1", s = "none", q = "median", use_plotly = TRUE)
    expect_true(inherits(base_plot, "plotly"))
    age_plot_F <- age_trend_plot(age_list, f = "Volume_1", s = "F", q = "quantile_75", use_plotly = TRUE)
    expect_true(inherits(age_plot_F, "plotly"))
    age_plot_M <- age_trend_plot(age_list, f = "Volume_1", s = "M", q = "quantile_75", use_plotly = TRUE)
    expect_true(inherits(age_plot_M, "plotly"))
    age_plot_F_M <- age_trend_plot(age_list, f = "Volume_1", s = "F vs M", q = "quantile_75", use_plotly = TRUE)
    expect_true(inherits(age_plot_F_M, "plotly"))
    cus_list <- cus_result_gen(age_list, customized_q = 0.3, f = "Volume_1")
    age_plot_F_M_customize <- age_trend_plot(age_list, f = "Volume_1", s = "F vs M", q = "customization", cus_list = cus_list, use_plotly = TRUE)
    expect_true(inherits(age_plot_F_M_customize, "plotly"))
    age_plot_M_customize <- age_trend_plot(age_list, f = "Volume_1", s = "M", q = "customization", cus_list = cus_list, use_plotly = TRUE)
    expect_true(inherits(age_plot_M_customize, "plotly"))
    age_plot_F_customize <- age_trend_plot(age_list, f = "Volume_1", s = "F", q = "customization", cus_list = cus_list, use_plotly = TRUE)
    expect_true(inherits(age_plot_F_customize, "plotly"))
  }

  base_plot_gg <- age_trend_plot(age_list, f = "Volume_1", s = "none", q = "median", use_plotly = FALSE)
  expect_true(inherits(base_plot_gg, "ggplot"))
  age_plot_F_gg <- age_trend_plot(age_list, f = "Volume_1", s = "F", q = "quantile_75", use_plotly = FALSE)
  expect_true(inherits(age_plot_F_gg, "ggplot"))
  age_plot_M_gg <- age_trend_plot(age_list, f = "Volume_1", s = "M", q = "quantile_75", use_plotly = FALSE)
  expect_true(inherits(age_plot_M_gg, "ggplot"))
  age_plot_F_M_gg <- age_trend_plot(age_list, f = "Volume_1", s = "F vs M", q = "quantile_75", use_plotly = FALSE)
  expect_true(inherits(age_plot_F_M_gg, "ggplot"))
  cus_list <- cus_result_gen(age_list, customized_q = 0.3, f = "Volume_1")
  age_plot_F_M_customize_gg <- age_trend_plot(age_list, f = "Volume_1", s = "F vs M", q = "customization", cus_list = cus_list, use_plotly = FALSE)
  expect_true(inherits(age_plot_F_M_customize_gg, "ggplot"))
  age_plot_F_customize_gg <- age_trend_plot(age_list, f = "Volume_1", s = "F", q = "customization", cus_list = cus_list, use_plotly = FALSE)
  expect_true(inherits(age_plot_F_customize_gg, "ggplot"))
  age_plot_M_customize_gg <- age_trend_plot(age_list, f = "Volume_1", s = "M", q = "customization", cus_list = cus_list, use_plotly = FALSE)
  expect_true(inherits(age_plot_M_customize_gg, "ggplot"))
})


test_that("Age trend table generated correctly", {
  age_list <- readRDS("previous-results/age_list.rds")
  age_table_F_M <- age_table_gen(result = age_list$Volume_1, q = "median", s = "F vs M")
  expect_true("datatables" %in% class(age_table_F_M))
  expect_true(inherits(age_table_F_M, "htmlwidget"))
  age_table_F <- age_table_gen(result = age_list$Volume_1, q = "median", s = "F")
  expect_true("datatables" %in% class(age_table_F))
  expect_true(inherits(age_table_F, "htmlwidget"))
  age_table_M <- age_table_gen(result = age_list$Volume_1, q = "median", s = "M")
  expect_true("datatables" %in% class(age_table_M))
  expect_true(inherits(age_table_M, "htmlwidget"))
  cus_list <- cus_result_gen(age_list, customized_q = 0.3, f = "Volume_1")
  age_table_M_cus <- age_table_gen(result = cus_list$cus_result, q = "median", s = "M")
  expect_true("datatables" %in% class(age_table_M_cus))
  age_table_F_cus <- age_table_gen(result = cus_list$cus_result, q = "median", s = "F")
  expect_true("datatables" %in% class(age_table_F_cus))
  age_table_F_M_cus <- age_table_gen(result = cus_list$cus_result, q = "median", s = "F vs M")
  expect_true("datatables" %in% class(age_table_F_M_cus))
})


