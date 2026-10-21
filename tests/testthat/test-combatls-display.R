test_that("screening presentation combines effects with their matching joint tests", {
  check <- list(features = c("a", "b"),
    summary = data.frame(covariate = c("AGE", "SEX"), tested = c(2L, 1L),
      significant = c(1L, 0L), total_features = 2L, significant_pct_all = c(50, 0)),
    effect_summary = data.frame(covariate = c("AGE", "SEX", "SEX"),
      contrast = c("per 1 original unit", "M vs F", "Other vs F"),
      estimated_features = c(2L, 1L, 1L), sd_ratio_median = c(1.23456, 1.5, .75)),
    screening = data.frame(feature = rep(c("a", "b"), 2),
      covariate = rep(c("AGE", "SEX"), each = 2), statistic = c(10.12345, .1, 2, NA),
      df = c(1, 1, 2, NA), p.value = c(1e-8, .5, .1, NA),
      q.value = c(2e-8, .5, .2, NA), method = "Likelihood-ratio test",
      status = c("ok", "ok", "ok", "failed")),
    effects = data.frame(feature = c("a", "b", "a", "a", "b", "b"),
      covariate = c("AGE", "AGE", rep("SEX", 4)),
      contrast = c(rep("per 1 original unit", 2), rep(c("M vs F", "Other vs F"), 2)),
      sd_ratio = c(1.2, 1.26912, 1.5, .75, NA, NA),
      sd_change_pct = c(20, 26.912, 50, -25, NA, NA), status = "ok"))
  original <- check
  tables <- .combatls_screening_display(check)
  expect_identical(check, original)
  expect_identical(tables$summary[["Features tested"]], c(2L, 1L))
  expect_equal(tables$summary[["Significant features"]], c("1 / 2", "0 / 2"))
  expect_equal(tables$summary[["Percentage of features"]], c("50.000%", "0.000%"))
  expect_equal(tables$summary[["Median SD ratio"]], c("1.235", "\u2014"))
  expect_match(tables$summary[["SD ratio contrast"]][2], "Multiple contrasts")
  age <- tables$details[tables$details[["Biological covariate"]] == "AGE", ]
  expect_equal(age[["BH-adjusted p-value"]], c("<0.001", "0.500"))
  expect_equal(age[["SD ratio"]], c("1.200", "1.269"))
  expect_equal(age$sig, c("***", ""))
  expect_equal(.combatls_display_sig(c(0, .0009, .001, .0099, .01, .0499, .05, NA)),
               c("***", "***", "**", "**", "*", "*", "", ""))
  sex_a <- subset(tables$details, Feature == "a" & `Biological covariate` == "SEX")
  expect_equal(nrow(sex_a), 2L)
  expect_true(all(sex_a[["BH-adjusted p-value"]] == "0.200"))
  expect_equal(.combatls_display_p(c(0, .0009, .001, .05, NA)),
               c("<0.001", "<0.001", "0.001", "0.050", "NA"))
  expect_false(any(c("Test statistic", "Degrees of freedom", "Test method", "Test status", "Effect status") %in% names(tables$details)))
  expect_equal(tables$details$.fdr_sort, sort(tables$details$.fdr_sort))
  expect_equal(tables$details$.significant, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(nrow(.combatls_filter_associations(tables$details, "SEX")), 4L)
  expect_equal(nrow(.combatls_filter_associations(tables$details, significant_only = TRUE)), 1L)
  expect_equal(nrow(.combatls_filter_associations(tables$details, "SEX", TRUE)), 0L)
  expect_identical(.combatls_filter_associations(tables$details, ""), tables$details)
  widget <- .combatls_associations_table(tables$details)
  expect_equal(widget$x$options$order, list(list(9L, "asc")))
  expect_true(any(vapply(widget$x$options$columnDefs, function(def)
    identical(def$visible, FALSE) && all(c(5L, 8L, 9L) %in% def$targets), logical(1))))
  expect_match(as.character(widget$x$options$rowCallback), "lightyellow", fixed = TRUE)
  expect_match(as.character(widget$x$options$rowCallback), '"red"', fixed = TRUE)
  expect_match(as.character(widget$x$options$rowCallback), '"inherit"', fixed = TRUE)
  # Distinct very small adjusted p-values sort numerically despite identical labels.
  check$screening$q.value <- c(9e-5, 1e-8, .05, NA)
  precise <- .combatls_screening_display(check)$details
  expect_equal(precise$Feature[1:2], c("b", "a"))
  expect_equal(precise[["BH-adjusted p-value"]][1:2], rep("<0.001", 2))
  expect_false(any(precise$.significant[precise$.fdr_sort == .05]))
  expect_equal(precise$.fdr_sort[nrow(precise)], 2)
  check$effect_summary <- check$effect_summary[1L, ]
  check$effect_summary$estimated_features <- 0L
  expect_true(all(.combatls_screening_display(check)$summary[["Median SD ratio"]] == "\u2014"))
  expect_equal(.combatls_screening_display(list(summary = NULL)),
               list(summary = data.frame(), details = data.frame()))
})

test_that("screening heatmap keeps feature data with bold labels and a compact legend", {
  check <- structure(list(screening = data.frame(feature = letters[1:5], covariate = "AGE",
    q.value = c(.009, .01, .05, .8, NA))), class = "combatls_check")
  plot <- combatls_check_plot(check, "screening")
  expect_equal(plot$data$feature, letters[1:5])
  expect_length(plot$layers, 1L)
  expect_s3_class(plot$layers[[1L]]$geom, "GeomTile")
  expect_equal(plot$theme$axis.text.x$face, "bold")
  expect_equal(plot$theme$axis.text.y$face, "bold")
  expect_equal(plot$theme$axis.title$face, "bold")
  expect_equal(plot$theme$legend.title$face, "bold")
  expect_equal(plot$theme$legend.text$face, "bold")
  expect_match(as.character(plot$data$evidence[1]), "Strong evidence", fixed = TRUE)
  expect_match(as.character(plot$data$evidence[2]), "Evidence (", fixed = TRUE)
  expect_match(as.character(plot$data$evidence[3]), "Not significant", fixed = TRUE)
  expect_true(is.na(plot$data$evidence[5]))
  expect_equal(plot$labels$fill, "BH-adjusted p-value")
  expect_null(plot$labels$title)
  expect_null(plot$labels$caption)
  expect_silent(ggplot2::ggplot_build(plot))
})
