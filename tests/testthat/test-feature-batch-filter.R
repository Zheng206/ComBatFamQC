feature_batch_filter_fixture <- function() {
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  result$info$df <- utils::head(result$info$df, 180)
  tests <- data.frame(feature = letters[1:6],
    p.value = c("<0.001", "0.050", "0.050", "0.050", "0.200", NA),
    sig = c("***", "*", NA, NA, NA, NA),
    p.value.raw = c(.0001, .0496, .05, .0504, .2, NA))
  for (key in c("kr_test_df", "anova_test_df", "kw_test_df", "fk_test_df", "lv_test_df", "bl_test_df"))
    result[[key]] <- tests
  result
}

test_that("feature-level filters use full-precision Bonferroni results for every test", {
  result <- feature_batch_filter_fixture()
  result$info$type <- "lmer"
  original <- result
  for (test in c("kenward_roger", "anova", "kruskal_wallis", "fligner_killeen", "levenes", "bartletts")) {
    full <- combat_table_gen(result, test)
    filtered <- combat_table_gen(result, test, significant_only = TRUE)
    expect_equal(full$x$data$feature, letters[1:6])
    expect_equal(filtered$x$data$feature, c("a", "b"))
    expect_equal(filtered$x$data$p.value, c("<0.001", "0.050"))
    expect_equal(combat_table_gen(result, test, significant_only = FALSE)$x$data, full$x$data)
  }
  expect_identical(result, original)

  # Older saved results retain significance symbols derived before rounding.
  result$anova_test_df$p.value.raw <- NULL
  expect_equal(combat_table_gen(result, "anova", significant_only = TRUE)$x$data$feature, c("a", "b"))

  # No significant rows still produces the usual three-column table.
  result$bl_test_df$p.value.raw <- rep(.5, 6)
  empty <- combat_table_gen(result, "bartletts", significant_only = TRUE)
  expect_equal(nrow(empty$x$data), 0L)
  expect_true(all(c("feature", "p.value", "sig") %in% names(empty$x$data)))
  result$info$type <- "lm"
  result$kr_test_df <- data.frame()
  expect_s3_class(combat_table_gen(result, "kenward_roger", significant_only = TRUE), "datatables")
})

test_that("Shiny feature-level filters are independent and preserve overall summaries", {
  result <- feature_batch_filter_fixture()
  original <- result
  run_table <- combat_table_gen
  displayed <- list()
  local_mocked_bindings(combat_table_gen = function(result, table_name, ..., significant_only = FALSE) {
    table <- run_table(result, table_name, ..., significant_only = significant_only)
    displayed[[table_name]] <<- table$x$data$feature
    table
  })
  testServer(comfam_shiny(result), {
    session$setInputs(test_batch = "ANOVA", test_variance = "Fligner-Killeen",
      batch_significant_only = FALSE, variance_significant_only = FALSE)
    invisible(output$test_batch_table)
    invisible(output$test_variance)
    expect_equal(displayed$anova, letters[1:6])
    expect_equal(displayed$fligner_killeen, letters[1:6])
    batch_summary <- output$sig_pct_batch
    variance_summary <- output$sig_pct_variance

    session$setInputs(batch_significant_only = TRUE)
    invisible(output$test_batch_table)
    expect_equal(displayed$anova, c("a", "b"))
    expect_equal(displayed$fligner_killeen, letters[1:6])
    expect_identical(output$sig_pct_batch, batch_summary)
    session$setInputs(test_batch = "Kruskal-Wallis")
    invisible(output$test_batch_table)
    expect_equal(displayed$kruskal_wallis, c("a", "b"))

    session$setInputs(variance_significant_only = TRUE)
    for (test in c("Fligner-Killeen", "Levene's Test", "Bartlett's Test")) {
      session$setInputs(test_variance = test)
      invisible(output$test_variance)
    }
    expect_equal(displayed$fligner_killeen, c("a", "b"))
    expect_equal(displayed$levenes, c("a", "b"))
    expect_equal(displayed$bartletts, c("a", "b"))
    session$setInputs(test_variance = "Fligner-Killeen")
    expect_identical(output$sig_pct_variance, variance_summary)

    session$setInputs(batch_significant_only = FALSE, variance_significant_only = FALSE)
    invisible(output$test_batch_table)
    invisible(output$test_variance)
    expect_equal(displayed$kruskal_wallis, letters[1:6])
    expect_equal(displayed$fligner_killeen, letters[1:6])
  })
  expect_identical(result, original)
})
