biological_fixture <- function() {
  set.seed(129)
  subject <- rep(seq_len(40), each = 3)
  data <- data.frame(AGE = rep(runif(40, 25, 75), each = 3) + rep(0:2, 40),
    SEX = factor(rep(rep(c("Female", "Male"), 20), each = 3)),
    batch = factor(rep(rep(c("A", "B", "C"), length.out = 40), each = 3)),
    subject = factor(subject))
  data$roi_a <- .2 * data$AGE + 2 * (data$SEX == "Male") +
    .03 * data$AGE * (data$SEX == "Male") + as.numeric(data$batch) +
    rep(rnorm(40, sd = 2), each = 3) + rnorm(120)
  data$roi_b <- -.15 * data$AGE + 3 * sin(data$AGE / 10) - (data$SEX == "Male") +
    rep(rnorm(40), each = 3) + rnorm(120)
  attr(data$roi_a, "units") <- "mm"
  attr(data$AGE, "units") <- "years"
  data
}

biological_model <- function(type, data = biological_fixture(), extra = NULL) {
  model_gen("roi_a", type, batch = "batch", covariates = c("AGE", "SEX", extra),
    interaction = "AGE:SEX", random = if (type == "lmer") "subject" else NULL,
    smooth = if (type == "gam") "AGE" else NULL, df = data)
}

test_that("biological lm estimates, contrasts, units and conditional intervals match the fitted model", {
  data <- biological_fixture()
  fit <- biological_model("lm", data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"), "mm", c(AGE = "years"))
  expect_equal(table$coefficient, c("AGE", "SEXMale", "AGE:SEXMale"))
  expect_equal(table$estimate, unname(stats::coef(fit)[table$coefficient]))
  expect_equal(table$std_error, unname(sqrt(diag(stats::vcov(fit)))[table$coefficient]))
  expect_equal(table$conf_low, unname(stats::confint(fit)[table$coefficient, 1]))
  expect_equal(table$conf_high, unname(stats::confint(fit)[table$coefficient, 2]))
  expect_match(table$contrast_or_unit[1], "years", fixed = TRUE)
  expect_match(table$contrast_or_unit[2], "Male vs Female", fixed = TRUE)
  expect_match(table$contrast_or_unit[2], "AGE = 0", fixed = TRUE)
  expect_match(table$contrast_or_unit[3], "Conditional interaction", fixed = TRUE)
  expect_true(all(table$n_observations == nrow(data)))
  expect_true(all(table$feature_unit == "mm"))
  expect_true(all(grepl("SEX = Female", table$categorical_references, fixed = TRUE)))
  expect_true(all(table$model_formula == paste(deparse(stats::formula(fit)), collapse = " ")))
  # Batch exclusion follows the model's terms, not a coefficient-name prefix.
  data$batch_score <- rnorm(nrow(data))
  fit <- biological_model("lm", data, extra = "batch_score")
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX", "batch_score"))
  expect_true("batch_score" %in% table$term)
  expect_false(any(table$coefficient %in% c("(Intercept)", "batchB", "batchC")))
  fit <- stats::lm(roi_a ~ AGE * batch + SEX, data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  expect_equal(table$coefficient, c("AGE", "SEXMale"))
  expect_match(table$contrast_or_unit[1L], "conditional at batch = A", fixed = TRUE)
  data$AGE[1L] <- NA_real_
  fit <- biological_model("lm", data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  expect_true(all(table$n_observations == nrow(data) - 1L))
})

test_that("categorical coding records actual references and does not mislabel sum contrasts", {
  data <- biological_fixture()
  data$SEX <- stats::relevel(data$SEX, "Male")
  fit <- biological_model("lm", data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  expect_true(any(grepl("Female vs Male", table$contrast_or_unit, fixed = TRUE)))
  stats::contrasts(data$SEX) <- stats::contr.sum(2)
  fit <- biological_model("lm", data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  expect_true(any(grepl("SEX contrast", table$contrast_or_unit, fixed = TRUE)))
  expect_false(any(grepl("vs", table$contrast_or_unit, fixed = TRUE)))
  expect_true(all(grepl("non-treatment coding", table$categorical_references, fixed = TRUE)))
  expect_true(all(grepl("Male=1", gsub(" ", "", table$covariate_coding), fixed = TRUE)))
  levels(data$SEX) <- c("Group:A", "Group:B")
  stats::contrasts(data$SEX) <- stats::contr.treatment(2)
  fit <- biological_model("lm", data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  expect_true(any(grepl("Group:B vs Group:A", table$contrast_or_unit, fixed = TRUE)))
})

test_that("mixed models report population fixed effects without random-effect estimates", {
  fit <- biological_model("lmer")
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  expect_equal(table$estimate, unname(lme4::fixef(fit)[table$coefficient]))
  expect_equal(table$conf_low, table$estimate - stats::qnorm(.975) * table$std_error)
  expect_true(all(grepl("population fixed", table$interval_method, fixed = TRUE)))
  expect_false(any(grepl("subject|Intercept|batch", table$coefficient)))
})

test_that("GAM parametric effects exclude smooth basis coefficients and flag smooth terms", {
  data <- biological_fixture()
  fit <- model_gen("roi_b", "gam", "batch", covariates = "SEX", smooth = "AGE", df = data)
  table <- .biological_fixed_effects(fit, "roi_b", "batch", c("AGE", "SEX"))
  expect_equal(table$coefficient, c("SEXMale", "s(AGE)"))
  expect_equal(table$estimate[1L], unname(stats::coef(fit)["SEXMale"]))
  expect_equal(table$std_error[1L], unname(sqrt(diag(stats::vcov(fit)))["SEXMale"]))
  expect_equal(table$conf_low[1L], table$estimate[1L] - stats::qnorm(.975) * table$std_error[1L])
  expect_true(is.na(table$estimate[2L]))
  expect_match(table$estimate_status[2L], "Separate smooth-effect view", fixed = TRUE)
  expect_false(any(grepl("s(AGE).", table$coefficient, fixed = TRUE)))
  # Parametric interactions and factor-by smooths stay distinguishable.
  fit <- mgcv::gam(roi_a ~ SEX + AGE:SEX + s(AGE, by = SEX) + batch, data = data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  expect_true(any(table$effect_type == "Conditional interaction"))
  expect_equal(sum(table$effect_type == "Smooth effect (separate view required)"), 2L)
})

test_that("aliased coefficients and invalid intervals are explicitly unavailable", {
  data <- biological_fixture()
  data$duplicate <- data$AGE
  for (type in c("lm", "lmer", "gam")) {
    fit <- suppressMessages(biological_model(type, data, "duplicate"))
    table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX", "duplicate"))
    row <- table[table$coefficient == "duplicate", ]
    expect_equal(nrow(row), 1L)
    expect_true(is.na(row$estimate))
    expect_true(is.na(row$conf_low))
    expect_match(row$estimate_status, "Unavailable", fixed = TRUE)
  }
  fit <- stats::lm(y ~ x, data.frame(y = c(1, 4), x = c(1, 2)))
  table <- .biological_fixed_effects(fit, "y", "batch", "x")
  expect_equal(table$estimate, 3)
  expect_true(is.na(table$conf_low))
  expect_match(table$interval_status, "Unavailable", fixed = TRUE)
})

test_that("units are explicit and old saved results are exported without refitting", {
  data <- biological_fixture()
  expect_equal(.biological_units(data, c("roi_a", "roi_b")), c(roi_a = "mm", roi_b = "Not recorded"))
  expect_equal(.biological_units(data, "roi_b", c(roi_b = "mm^3")), c(roi_b = "mm^3"))
  expect_error(.biological_units(data, "roi_a", "mm"), "named character")
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  table <- .biological_report_table(result)
  expect_equal(table$feature, result$info$features)
  expect_true(all(is.na(table$estimate)))
  expect_true(all(grepl("rerun visual_prep", table$estimate_status, fixed = TRUE)))
})

test_that("the compact report table follows the plot selection", {
  fit <- biological_model("lm")
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  before <- table
  compact <- .biological_report_widget(table)
  expect_equal(names(compact$x$data), c("feature", "estimate", "std_error", "confidence_interval", "selection_group"))
  expect_equal(.biological_effect_labels(table), c("AGE", "SEX: Male vs Female", "AGE \u00d7 SEX: Male vs Female"))
  view <- as.character(.biological_coefficient_view(table))
  expect_false(grepl("Conditional interaction", view, fixed = TRUE))
  expect_false(grepl("Not recorded", view, fixed = TRUE))
  expect_equal(compact$x$data$estimate, table$estimate)
  expect_equal(compact$x$data$selection_group, match(.biological_group_labels(table), unique(.biological_group_labels(table))))
  expect_match(as.character(compact$x$options$initComplete), "combatqc-biological-term-change", fixed = TRUE)
  expect_identical(table, before)
  data <- biological_fixture()
  fit <- model_gen("roi_a", "gam", "batch", covariates = "SEX", smooth = "AGE", df = data)
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  view <- as.character(.biological_coefficient_view(table))
  expect_match(view, "This smooth requires a separate fitted-effect curve", fixed = TRUE)
  expect_equal(length(gregexpr("<option ", view, fixed = TRUE)[[1L]]), 2L)
  expect_equal(.biological_report_widget(table)$x$data$confidence_interval[2L], "Unavailable")
})

test_that("Excel uses short effect names and omits unknown-unit placeholders", {
  fit <- biological_model("lm")
  table <- .biological_fixed_effects(fit, "roi_a", "batch", c("AGE", "SEX"))
  before <- table
  exported <- .biological_excel_table(table)
  expect_equal(exported$`Biological term`, c("AGE", "SEX: Male vs Female", "AGE \u00d7 SEX: Male vs Female"))
  expect_false(any(grepl("Not recorded", unlist(exported), fixed = TRUE)))
  expect_false(any(grepl("Conditional interaction", exported$`Biological term`, fixed = TRUE)))
  expect_true(all(exported$`Feature unit` == ""))
  expect_equal(exported$Estimate, table$estimate)
  expect_equal(exported$`95% CI lower`, table$conf_low)
  expect_equal(exported$`Model coefficient`, table$coefficient)
  expect_equal(exported$`Model formula`, table$model_formula)
  expect_identical(table, before)
})

test_that("diagnostic fits, single-run HTML and Excel carry the same biological estimates for every model", {
  data <- biological_fixture()
  original_extract <- .biological_fixed_effects
  captured <- list()
  local_mocked_bindings(.biological_fixed_effects = function(model, feature, ...) {
    captured[[feature]] <<- model
    original_extract(model, feature, ...)
  })
  for (type in c("lm", "lmer", "gam")) {
    result <- visual_prep(type, features = c("roi_a", "roi_b"), batch = "batch",
      covariates = c("AGE", "SEX"), interaction = "AGE,SEX", df = data,
      random = if (type == "lmer") "subject" else NULL,
      smooth = if (type == "gam") "AGE" else NULL,
      smooth_int_type = if (type == "gam") "linear" else NULL,
      cores = 1, mdmr = FALSE, feature.units = c(roi_b = "mm^3"))
    table <- result$biological_fixed_effects
    expect_equal(unique(table$feature), c("roi_a", "roi_b"))
    expect_true(all(table$n_observations == nrow(data)))
    expect_true(all(table$feature_unit[table$feature == "roi_a"] == "mm"))
    expect_true(all(table$feature_unit[table$feature == "roi_b"] == "mm^3"))
    for (feature in c("roi_a", "roi_b")) {
      expect_equal(result$residual_ml_df[[feature]], unname(stats::resid(captured[[feature]])))
      estimated <- table[table$feature == feature & table$estimate_status == "Available", ]
      coefficients <- if (type == "lmer") lme4::fixef(captured[[feature]]) else stats::coef(captured[[feature]])
      expect_equal(estimated$estimate, unname(coefficients[estimated$coefficient]))
    }
    path <- tempfile(paste0("biological-", type))
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    # Export may never fit a model, even when regenerating an existing report.
    block_fit <- function(...) stop("Export must not refit the supplied diagnosis")
    with_mocked_bindings({
      diag_save(path, result, use_quarto = FALSE)
      workbook <- file.path(path, "diagnosis.xlsx")
      expect_equal(openxlsx::getSheetNames(workbook), c("Batch Summary", "PCA Summary", "MDMR", "ANOVA",
        "Kruskal-Wallis", "Kenward-Roger", "Levene's Test", "Bartlett's Test", "Fligner-Killeen", "Biological Fixed Effects"))
      exported <- openxlsx::read.xlsx(workbook, sheet = "Biological Fixed Effects", check.names = FALSE, sep.names = " ")
      expect_equal(names(exported), .biological_column_labels())
      names(exported) <- names(table)
      expect_equal(nrow(exported), nrow(table))
      expect_equal(exported$feature, table$feature)
      expect_equal(exported$term, .biological_effect_labels(table))
      expect_equal(exported$feature_unit, table$feature_unit)
      available <- table$estimate_status == "Available"
      expect_equal(as.numeric(exported$estimate[available]), table$estimate[available])
      expect_equal(as.numeric(exported$conf_low[available]), table$conf_low[available])
      expected_excel <- .biological_excel_table(table)
      expect_equal(exported$covariate_coding, expected_excel$`Covariate coding`)
      expect_equal(exported$model_formula, table$model_formula)
      expect_equal(openxlsx::read.xlsx(workbook, "ANOVA")$feature, result$anova_test_df$feature)
      if (type == "gam") expect_true(all(exported$estimate[!available] == "Unavailable"))
      if (requireNamespace("quarto", quietly = TRUE) && !is.null(quarto::quarto_path())) {
        writeLines("Stale template from a different run", file.path(path, "diagnosis_report.qmd"))
        diag_save(path, result, use_quarto = TRUE)
        html <- paste(readLines(file.path(path, "diagnosis_report.html"), warn = FALSE), collapse = "\n")
        expect_true(grepl("Biological fixed effects", html, fixed = TRUE))
        expect_true(grepl("biological-term-select", html, fixed = TRUE))
        expect_true(grepl('<select id="biological-term-select"', html, fixed = TRUE))
        expect_true(grepl("<svg", html, fixed = TRUE) || grepl("data:image/svg+xml", html, fixed = TRUE))
        expect_false(grepl("&lt;svg", html, fixed = TRUE))
        expect_true(grepl("95% CI", html, fixed = TRUE))
        expect_false(grepl("Model details and availability", html, fixed = TRUE))
        expect_false(grepl('id="biological-model-details"', html, fixed = TRUE))
        expect_true(grepl("combatqc-biological-term-change", html, fixed = TRUE))
        expect_true(grepl("roi_a", html, fixed = TRUE))
        expect_true(grepl("roi_b", html, fixed = TRUE))
        expect_false(grepl("Stale template from a different run", html, fixed = TRUE))
        if (type == "gam") {
          expect_true(grepl("This smooth requires a separate fitted-effect curve", html, fixed = TRUE))
          expect_true(grepl("smooth basis coefficients are not shown as slopes", html, fixed = TRUE))
        }
      }
    }, model_gen = block_fit)
  }
})
