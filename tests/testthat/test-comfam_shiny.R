test_that("comfam_shiny server logic works correctly", {
  skip_if_not_installed("systemfonts")
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  server <- comfam_shiny(result)
  testServer(server, {
    # Test 1: Ensure inputs update correctly
    session$setInputs(data_view = "Exploratory Analysis")
    expect_equal(input$data_view, "Exploratory Analysis")

    # Test 2: Ensure feature selection works
    session$setInputs(single_feature = "thickness.left.caudal.middle.frontal")
    expect_equal(input$single_feature, "thickness.left.caudal.middle.frontal")

    # Test 3: Ensure output tables and plots exist
    session$setInputs(num_var_control_batch = "No")
    expect_equal(input$num_var_control_batch, "No")

    # Test 4: Changing categorical variables
    session$setInputs(single_cov = "AGE")
    session$setInputs(num_var_control_batch = "Yes")
    expect_true(!is.null(output$batch_sep_control))

    session$setInputs(single_cov = "DIAGNOSIS")
    expect_equal(input$single_cov, "DIAGNOSIS")
    session$setInputs(char_var_control = "boxplot with points")
    expect_equal(input$char_var_control, "boxplot with points")

    # Test 5: Ensure batch selection works
    session$setInputs(overview_batch_select = c("Philips", "Siemens"))
    expect_true("Philips" %in% input$overview_batch_select)
    expect_true("Siemens" %in% input$overview_batch_select)

    # Test 6: Test summary statistics
    session$setInputs(tabselected = "2")
    session$setInputs(type = "Plot")
    expect_equal(input$type, "Plot")
    session$setInputs(type = "Table")
    expect_equal(input$type, "Table")

    # Test 7: Residual plots
    session$setInputs(tabselected = "3")
    session$setInputs(resid_color = "Yes", resid_all = "No", resid_label = "Yes")
    expect_equal(input$resid_color, "Yes")
    expect_equal(input$resid_all, "No")
    expect_equal(input$resid_label, "Yes")

    # Test 8: Global Diagnosis (PCA plots)
    session$setInputs(tabselected = "4")
    session$setInputs(PC1 = "PC1", PC2 = "PC2", pca_label = "Yes", pca_all = "No")
    expect_true(!is.null(output$pca))

    # Test 9: Feature Level Diagnosis
    session$setInputs(tabselected = "5")
    session$setInputs(test_variance = "Bartlett's Test", test_batch = "ANOVA")
    expect_equal(input$test_variance, "Bartlett's Test")
    expect_equal(input$test_batch, "ANOVA")
    session$setInputs(test_variance = "Levene's Test", test_batch = "Kruskal-Wallis")
    expect_equal(input$test_variance, "Levene's Test")
    expect_equal(input$test_batch, "Kruskal-Wallis")

    # Test 10: Harmonization
    session$setInputs(tabselected = "6")
    session$setInputs(com_type = "covfam", com_model = "gam")
    expect_equal(input$com_type, "covfam")
    expect_equal(input$com_model, "gam")
  })
})

test_that("Batch effect diagnostics plot generated correctly", {
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  features <- result$info$features
  batch_density_plot <- combat_plot_gen(result, features[1], plot_name = "batch_density")
  batch_density_plot_se <- combat_plot_gen(result, features[1], batch_control = "Yes", batch_level = c("GE", "Philips", "Siemens"), plot_name = "batch_density")
  expect_true(inherits(batch_density_plot, "ggplot"))
  expect_true(inherits(batch_density_plot_se, "ggplot"))

  cov_feature_plot_num <- combat_plot_gen(result, features[1], batch_control = "No", plot_name = "cov_feature", c = "AGE")
  cov_feature_plot_num_se <- combat_plot_gen(result, features[1], batch_control = "Yes", batch_level = c("GE", "Philips", "Siemens"), plot_name = "cov_feature", c = "AGE")
  cov_feature_plot_char_1 <- combat_plot_gen(result, features[1], batch_control = "No", plot_name = "cov_feature", c = "SEX")
  cov_feature_plot_char_2 <- combat_plot_gen(result, features[1], batch_control = "No", plot_name = "cov_feature", c = "SEX", char_plot_type = "boxplot with points")
  cov_feature_plot_char_3 <- combat_plot_gen(result, features[1], batch_control = "No", plot_name = "cov_feature", c = "SEX", char_plot_type = "density plot")
  cov_feature_plot_char_1_se <- combat_plot_gen(result, features[1], batch_control = "Yes", batch_level = c("GE", "Philips", "Siemens"), plot_name = "cov_feature", c = "SEX")
  cov_feature_plot_char_2_se <- combat_plot_gen(result, features[1], batch_control = "Yes", batch_level = c("GE", "Philips", "Siemens"), plot_name = "cov_feature", c = "SEX", char_plot_type = "boxplot with points")
  expect_true(inherits(cov_feature_plot_num, "ggplot"))
  expect_true(inherits(cov_feature_plot_num_se, "ggplot"))
  expect_true(inherits(cov_feature_plot_char_1, "ggplot"))
  expect_true(inherits(cov_feature_plot_char_2, "ggplot"))
  expect_true(inherits(cov_feature_plot_char_3, "ggplot"))
  expect_true(inherits(cov_feature_plot_char_1_se, "ggplot"))
  expect_true(inherits(cov_feature_plot_char_1_se, "ggplot"))

  batch_summary_plot <- combat_plot_gen(result, plot_name = "batch_summary")
  batch_summary_plot_text <- combat_plot_gen(result, plot_name = "batch_summary", text_status = "Yes")
  expect_true(inherits(batch_summary_plot, "ggplot"))
  expect_true(inherits(batch_summary_plot_text, "ggplot"))

  cov_distribution_plot_num <- combat_plot_gen(result, plot_name = "cov_distribution", c = "AGE")
  cov_distribution_plot_char <- combat_plot_gen(result, plot_name = "cov_distribution", c = "SEX")
  cov_distribution_plot_char_text <- combat_plot_gen(result, plot_name = "cov_distribution", c = "SEX", text_status = "Yes")
  expect_true(inherits(cov_distribution_plot_num, "ggplot"))
  expect_true(inherits(cov_distribution_plot_char, "ggplot"))
  expect_true(inherits(cov_distribution_plot_char_text, "ggplot"))

  resid_add_plot <- combat_plot_gen(result, f = features[1], batch_control = "No", plot_name = "resid_add", color = "No", label = "No", angle = 0)
  resid_add_plot_color <- combat_plot_gen(result, f = features[1], batch_control = "No", plot_name = "resid_add", color = "Yes", label = "No", angle = 0)
  resid_add_plot_label <- combat_plot_gen(result, f = features[1], batch_control = "No", plot_name = "resid_add", color = "Yes", label = "Yes", angle = 0)
  resid_add_plot_label_se <- combat_plot_gen(result, f = features[1], batch_control = "Yes", batch_level = c("Philips", "Siemens"), plot_name = "resid_add", color = "Yes", label = "Yes", angle = 0)
  expect_true(inherits(resid_add_plot, "ggplot"))
  expect_true(inherits(resid_add_plot_color, "ggplot"))
  expect_true(inherits(resid_add_plot_label, "ggplot"))
  expect_true(inherits(resid_add_plot_label_se, "ggplot"))

  resid_mul_plot <- combat_plot_gen(result, f = features[1], batch_control = "No", plot_name = "resid_mul", color = "No", label = "No", angle = 0)
  resid_mul_plot_color <- combat_plot_gen(result, f = features[1], batch_control = "No", plot_name = "resid_mul", color = "Yes", label = "No", angle = 0)
  resid_mul_plot_label <- combat_plot_gen(result, f = features[1], batch_control = "No", plot_name = "resid_mul", color = "Yes", label = "Yes", angle = 0)
  resid_mul_plot_label_se <- combat_plot_gen(result, f = features[1], batch_control = "Yes", batch_level = c("Philips", "Siemens"), plot_name = "resid_mul", color = "Yes", label = "Yes", angle = 0)
  expect_true(inherits(resid_mul_plot, "ggplot"))
  expect_true(inherits(resid_mul_plot_color, "ggplot"))
  expect_true(inherits(resid_mul_plot_label, "ggplot"))
  expect_true(inherits(resid_mul_plot_label_se, "ggplot"))

  pca_plot <- combat_plot_gen(result, batch_control = "No", plot_name = "pca", PC1 = "PC1", PC2 = "PC2")
  pca_plot_label <- combat_plot_gen(result, batch_control = "No", plot_name = "pca", PC1 = "PC1", PC2 = "PC2", label = "Yes")
  pca_plot_label_se <- combat_plot_gen(result, batch_control = "Yes", batch_level = c("Philips", "Siemens"), plot_name = "pca", PC1 = "PC1", PC2 = "PC2", label = "Yes")
  expect_true(inherits(pca_plot, "ggplot"))
  expect_true(inherits(pca_plot_label, "ggplot"))
  expect_true(inherits(pca_plot_label_se, "ggplot"))

  tsne_plot <- combat_plot_gen(result, batch_control = "No", plot_name = "tsne")
  tsne_plot_label <- combat_plot_gen(result, batch_control = "No", plot_name = "tsne", label = "Yes")
  tsne_plot_label_se <- combat_plot_gen(result, batch_control = "Yes", batch_level = c("Philips", "Siemens"), plot_name = "tsne", label = "Yes")
  expect_true(inherits(tsne_plot, "ggplot"))
  expect_true(inherits(tsne_plot_label, "ggplot"))
  expect_true(inherits(tsne_plot_label_se, "ggplot"))

  eb_df <- combat_harm(eb_check = TRUE, result, type = "lm", interaction = "timedays:DIAGNOSIS")
  eb_location_plot <- combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "No", plot_name = "eb_location")
  eb_location_plot_non_eb <- combat_plot_gen(result, eb = FALSE, eb_df = eb_df, batch_control = "No", plot_name = "eb_location")
  eb_location_plot_se <- combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "Yes", batch_level = "GE", plot_name = "eb_location")
  expect_true(inherits(eb_location_plot, "ggplot"))
  expect_true(inherits(eb_location_plot_non_eb, "ggplot"))
  expect_true(inherits(eb_location_plot_se, "ggplot"))

  eb_scale_plot <- combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "No", plot_name = "eb_scale")
  eb_scale_plot_non_eb <- combat_plot_gen(result, eb = FALSE, eb_df = eb_df, batch_control = "No", plot_name = "eb_scale")
  eb_scale_plot_se <- combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "Yes", batch_level = "GE", plot_name = "eb_scale")
  expect_true(inherits(eb_scale_plot, "ggplot"))
  expect_true(inherits(eb_scale_plot_non_eb, "ggplot"))
  expect_true(inherits(eb_scale_plot_se, "ggplot"))
})


test_that("Batch effect diagnostic table generated correctly", {
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  features <- result$info$features
  data_overview_table <- combat_table_gen(result, table_name = "data_overview")
  expect_true("datatables" %in% class(data_overview_table))
  expect_true(inherits(data_overview_table, "htmlwidget"))
  exploratory_table <- combat_table_gen(result, table_name = "exploratory_analysis", f = features[1])
  expect_true("datatables" %in% class(exploratory_table))
  expect_true(inherits(exploratory_table, "htmlwidget"))
  summary_table <- combat_table_gen(result, table_name = "summary_df")
  expect_true("datatables" %in% class(summary_table))
  expect_true(inherits(summary_table, "htmlwidget"))
  covariate_table_num <- combat_table_gen(result, table_name = "cov_table", c = "AGE")
  expect_true("datatables" %in% class(covariate_table_num))
  expect_true(inherits(covariate_table_num, "htmlwidget"))
  covariate_table_char <- combat_table_gen(result, table_name = "cov_table", c = "SEX")
  expect_true("datatables" %in% class(covariate_table_char))
  expect_true(inherits(covariate_table_char, "htmlwidget"))
  pca_table <- combat_table_gen(result, table_name = "pc_variance", PC1 = "PC1", PC2 = "PC2")
  expect_true("datatables" %in% class(pca_table))
  expect_true(inherits(pca_table, "htmlwidget"))
  mdmr_table <- combat_table_gen(result, table_name = "mdmr")
  expect_true("datatables" %in% class(mdmr_table))
  expect_true(inherits(mdmr_table, "htmlwidget"))
  kr_table <- combat_table_gen(result, table_name = "kenward_roger")
  expect_true("datatables" %in% class(kr_table))
  expect_true(inherits(kr_table, "htmlwidget"))
  anova_table <- combat_table_gen(result, table_name = "anova")
  expect_true("datatables" %in% class(anova_table))
  expect_true(inherits(anova_table, "htmlwidget"))
  kw_table <- combat_table_gen(result, table_name = "kruskal_wallis")
  expect_true("datatables" %in% class(kw_table))
  expect_true(inherits(kw_table, "htmlwidget"))
  fk_table <- combat_table_gen(result, table_name = "fligner_killeen")
  expect_true("datatables" %in% class(fk_table))
  expect_true(inherits(fk_table, "htmlwidget"))
  levenes_table <- combat_table_gen(result, table_name = "levenes")
  expect_true("datatables" %in% class(levenes_table))
  expect_true(inherits(levenes_table, "htmlwidget"))
  bartletts_table <- combat_table_gen(result, table_name = "bartletts")
  expect_true("datatables" %in% class(bartletts_table))
  expect_true(inherits(bartletts_table, "htmlwidget"))
})
