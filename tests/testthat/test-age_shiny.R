test_that("Shiny app server initializes and processes inputs correctly", {
  skip_if_not_installed("systemfonts")
  age_list <- readRDS(testthat::test_path("previous-results/age_list.rds"))
  server <- age_shiny(age_list = age_list, features = names(age_list), quantile_type = c("quantile_25", "median", "quantile_75"))
  testServer(server, {
    session$setInputs(features = "Volume_1", quantile = "quantile_75", sex = "Female")
    expect_equal(input$features, "Volume_1")
    expect_equal(input$quantile, "quantile_75")
    expect_true(!is.null(output$agetable))
    session$setInputs(sex = "Male")
    expect_equal(input$sex, "Male")
    session$setInputs(sex = "Female vs. Male (Only for visualization)")
    expect_equal(input$sex, "Female vs. Male (Only for visualization)")
  })
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

  saved_age_list <- readRDS(testthat::test_path("previous-results/age_list.rds"))
  expect_equal(age_sub$predicted_df_sex, saved_age_list[[1]]$predicted_df_sex, tolerance = 1e-8)

  age_sub_1 <- age_list_gen(sub_df = age_sub_df,  lq = 0.25, hq = 0.75, mu = "linear", sigma = "linear", tau = "smooth", nu = "smooth")
  expect_type(age_sub_1, "list")
})

test_that("Age trend quantile generated correctly", {
  age_list <- readRDS(testthat::test_path("previous-results/age_list.rds"))
  age_result <- customize_percentile(age_list, "Volume_2", 0.3, "F")
  expect_type(age_result, "list")
})

test_that("Age trend plot generated correctly", {
  age_list <- readRDS(testthat::test_path("previous-results/age_list.rds"))
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
  age_list <- readRDS(testthat::test_path("previous-results/age_list.rds"))
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


