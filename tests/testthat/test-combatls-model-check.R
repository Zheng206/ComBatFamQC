scale_check_data <- function() {
  set.seed(514)
  n <- 360
  data <- data.frame(AGE = rnorm(n), SEX = factor(rep(c("F", "M", "Other"), n / 3)),
                     site = factor(rep(c("A", "B", "C"), each = n / 3)))
  batch_mean <- c(A = -1, B = 0, C = 1)[data$site]
  batch_scale <- c(A = -.25, B = 0, C = .25)[data$site]
  data$a <- data$AGE + batch_mean + rnorm(n) * exp(.65 * data$AGE + .2 * as.numeric(data$SEX) + batch_scale)
  data$b <- .5 * data$AGE + batch_mean + rnorm(n) * exp(.3 * as.numeric(data$SEX) + batch_scale)
  data
}

test_that("screening explicitly models candidates independently of proposed scale", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  proposed <- ~ 1
  check <- combatls_model_check(df = data, features = c("a", "b"), batch = "site",
                                covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX,
                                sigma.formula = proposed)
  expect_s3_class(check, "combatls_check")
  expect_identical(check$sigma.formula, proposed)
  expect_setequal(all.vars(check$screen.formula), c("AGE", "SEX"))
  expect_false("harmonized_df" %in% names(check))
  expect_true(all(check$diagnostics$valid))
  expect_true(all(check$screening$status == "ok"))
  expect_true(check$screening$q.value[check$screening$feature == "a" & check$screening$covariate == "AGE"] < .01)
  for (feature in check$features) {
    full <- check$screen_fits[[feature]]
    expect_true(".combatls_batch" %in% all.vars(full$mu.formula))
    expect_true(".combatls_batch" %in% all.vars(full$sigma.formula))
    for (candidate in check$candidates) {
      reduced <- check$reduced_fits[[feature]][[candidate]]
      expect_false(candidate %in% all.vars(reduced$sigma.formula))
      expected <- gamlss::LR.test(reduced, full, print = FALSE)
      row <- check$screening[check$screening$feature == feature & check$screening$covariate == candidate, ]
      expect_equal(row$statistic, expected$chi)
      expect_equal(row$df, expected$df)
      if (expected$p.val == 0) expect_lt(row$p.value, .Machine$double.eps)
      else expect_equal(row$p.value, expected$p.val, tolerance = 1e-8)
    }
  }
  expect_true(all(check$screening$df[check$screening$covariate == "SEX"] == 2))
  for (candidate in check$candidates) {
    rows <- check$screening$covariate == candidate
    expect_equal(check$screening$q.value[rows], stats::p.adjust(check$screening$p.value[rows], "BH"))
  }
  expect_equal(nrow(check$parameters), nrow(check$data) * length(check$features) * 2)
  expect_equal(check$parameters$residual[check$parameters$model == "proposed" & check$parameters$feature == "a"],
                as.numeric(check$fits$a$residuals))
  expect_error(combatls_model_check(df = data, features = "a", batch = "site", covariates = c("AGE", "SEX"),
                                    formula = y ~ AGE, screen.formula = ~ 1), "include every candidate")
})

test_that("screening handles whole smooth terms and preserves formula scope", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  center <- function(x) x - .2
  check <- combatls_model_check(df = data, features = "a", batch = "site",
                                covariates = c("AGE", "SEX"), formula = y ~ center(AGE) + SEX,
                                screen.formula = ~ pb(AGE, df = 3) + SEX)
  expect_s3_class(check$screen_fits$a, "gamlss")
  expect_match(check$screening$method[1L], "Approximate LRT")
  expect_false("AGE" %in% all.vars(check$reduced_fits$a$AGE$sigma.formula))
  expect_true("SEX" %in% all.vars(check$reduced_fits$a$AGE$sigma.formula))
  expect_s3_class(combatls_check_plot(check, "screening"), "ggplot")
  local_mocked_bindings(.combatls_exploratory_fit = function(...) stop("must reuse smooth fits"))
  curves <- combatls_check_centiles(check, "a", "AGE", model = "screening", n = 5)
  expect_true(all(is.finite(curves$centile)))
})

test_that("failed feature comparisons stay in the BH correction family", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  bad_response <- data$b
  fit_function <- .combatls_exploratory_fit
  local_mocked_bindings(.combatls_exploratory_fit = function(mu, sigma, data, control) {
    if (identical(data$y, bad_response)) return(list(fit = NULL, warnings = character(), error = "simulated failure"))
    fit_function(mu, sigma, data, control)
  })
  check <- combatls_model_check(df = data, features = c("a", "b"), batch = "site",
                                covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX)
  expect_true(all(is.na(check$screening$q.value[check$screening$feature == "b"])))
  expect_equal(check$summary$significant_pct_all, 50 * check$summary$significant)
  expect_equal(check$summary$significant_pct_tested, 100 * check$summary$significant)
  expect_true(all(is.na(check$effects$sd_ratio[check$effects$feature == "b"])))
  expect_true(all(check$effect_summary$estimated_features == 1))
  for (candidate in check$candidates) {
    row <- check$screening[check$screening$feature == "a" & check$screening$covariate == candidate, ]
    expect_equal(row$q.value, min(1, 2 * row$p.value))
  }
  expect_true(all(grepl("simulated failure", check$diagnostics$message[check$diagnostics$feature == "b"])))
})

test_that("model-fit-only checks allow no screening candidates", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  check <- combatls_model_check(df = scale_check_data(), features = "a", batch = "site",
                                covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX,
                                candidates = character())
  expect_equal(nrow(check$screening), 0L)
  expect_null(check$suggested.formula)
  expect_s3_class(combatls_check_plot(check, "qq"), "ggplot")
})

test_that("failed fits remain visible and cannot produce significant results", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  fit_function <- .combatls_exploratory_fit
  local_mocked_bindings(.combatls_exploratory_fit = function(mu, sigma, data, control) {
    run <- fit_function(mu, sigma, data, control)
    run$fit$converged <- FALSE
    run$warnings <- "simulated nonconvergence"
    run
  })
  check <- combatls_model_check(df = data, features = "a", batch = "site",
                                covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX)
  expect_true(all(is.na(check$screening$p.value)))
  expect_true(all(is.na(check$screening$q.value)))
  expect_false(any(check$diagnostics$valid))
  expect_true(all(grepl("simulated nonconvergence", check$diagnostics$message)))
  expect_null(check$suggested.formula)
  expect_true(all(is.na(check$summary$significant_pct_tested)))
  expect_true(all(check$summary$significant_pct_all == 0))
  expect_true(all(is.na(check$effects$sd_ratio)))
  expect_error(combatls_check_centiles(check, "a", "AGE"), "converged")
})

test_that("centiles average batch effects on links and plots never refit GAMLSS", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  check <- combatls_model_check(df = data, features = "a", batch = "site",
                                covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX,
                                sigma.formula = ~ AGE + SEX)
  local_mocked_bindings(.combatls_exploratory_fit = function(...) stop("must reuse fits"))
  for (reference in list(NULL, "B")) {
    check$ref.batch <- reference
    curves <- combatls_check_centiles(check, "a", "AGE", group = "SEX", n = 5, probabilities = c(.05, .5, .95))
    grid <- curves[curves$probability == .5, ]
    mu <- log_sigma <- numeric(nrow(grid))
    weights <- prop.table(table(check$model_data$.combatls_batch))
    if (!is.null(reference)) weights[] <- as.numeric(names(weights) == reference)
    for (level in names(weights)) {
      grid$.combatls_batch <- factor(level, levels = names(weights))
      design <- stats::model.matrix(~ AGE + SEX + .combatls_batch, grid)
      mu <- mu + weights[[level]] * as.numeric(design %*% check$fits$a$mu.coefficients)
      log_sigma <- log_sigma + weights[[level]] * as.numeric(design %*% check$fits$a$sigma.coefficients)
    }
    expect_equal(curves$mu[curves$probability == .5], mu, tolerance = 1e-8)
    expect_equal(curves$sigma[curves$probability == .5], exp(log_sigma), tolerance = 1e-8)
    expect_equal(curves$centile, curves$mu + stats::qnorm(curves$probability) * curves$sigma)
  }
  for (type in c("screening", "qq", "residuals", "centiles")) {
    plot <- combatls_check_plot(check, type, feature = "a", x = "AGE", group = "SEX")
    expect_s3_class(plot, "ggplot")
    expect_no_error(ggplot2::ggplot_build(plot))
  }
})


test_that("screening percentages and SD ratios have explicit denominators and contrasts", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  # Non-treatment coding must still yield level-versus-first-level SD ratios.
  contrasts(data$SEX) <- stats::contr.sum(3)
  check <- combatls_model_check(df = data, features = c("a", "b"), batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX)
  expect_equal(check$summary$significant_pct_all, 100 * check$summary$significant / 2)
  expect_equal(check$summary$significant_pct_tested,
    100 * check$summary$significant / check$summary$tested)
  expect_match(check$suggestion.rule, "at least one feature")
  expect_match(check$suggestion.rule, "Not an optimal model")
  for (feature in check$features) {
    fit <- check$screen_fits[[feature]]
    effects <- check$effects[check$effects$feature == feature, ]
    expect_true(all(effects$status == "ok"))
    age <- effects[effects$covariate == "AGE", ]
    expect_equal(age$sd_ratio, unname(exp(fit$sigma.coefficients["AGE"])))
    expect_equal(age$sd_change_pct, 100 * (age$sd_ratio - 1))
    # Reconstruct fitted link difference at identical nuisance covariates.
    for (level in levels(data$SEX)[-1L]) {
      grid <- check$model_data[rep(1L, 2), ]
      grid$SEX <- factor(c(levels(data$SEX)[1L], level), levels = levels(data$SEX))
      contrasts(grid$SEX) <- contrasts(data$SEX)
      train <- check$model_data
      train$y <- data[[feature]]
      link <- stats::predict(fit, newdata = grid, data = train, what = "sigma", type = "link")
      row <- effects[effects$contrast == paste(level, "vs", levels(data$SEX)[1L]), ]
      expect_equal(row$sd_ratio, unname(exp(diff(link))), tolerance = 1e-6)
    }
  }
  age_ratios <- check$effects$sd_ratio[check$effects$covariate == "AGE"]
  expect_equal(check$effect_summary$sd_ratio_median[check$effect_summary$covariate == "AGE"], median(age_ratios))
  expect_true(all(check$effect_summary$estimated_features == 2))
})

test_that("varying scale effects are not presented as constant SD ratios", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  check <- combatls_model_check(df = scale_check_data(), features = "a", batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX,
    screen.formula = ~ pb(AGE) + SEX)
  expect_true(all(is.na(check$effects$sd_ratio[check$effects$covariate == "AGE"])))
  expect_match(check$effects$status[check$effects$covariate == "AGE"], "nonconstant")
  expect_true(all(is.finite(check$effects$sd_ratio[check$effects$covariate == "SEX"])))
  interaction_check <- combatls_model_check(df = scale_check_data(), features = "a", batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX,
    screen.formula = ~ AGE * SEX)
  expect_true(all(is.na(interaction_check$effects$sd_ratio)))
})

test_that("centile comparison overlays the same grid from cached models", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  check <- combatls_model_check(df = scale_check_data(), features = "a", batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX)
  local_mocked_bindings(.combatls_exploratory_fit = function(...) stop("Unexpected refit"))
  both <- combatls_check_centiles(check, "a", "AGE", group = "SEX", model = "both", n = 5)
  for (source in c("proposed", "screening")) {
    expected <- combatls_check_centiles(check, "a", "AGE", group = "SEX", model = source, n = 5)
    actual <- both[both$model == source, ]
    rownames(actual) <- rownames(expected) <- NULL
    expect_equal(actual, expected)
  }
  expect_equal(both$AGE[both$model == "proposed"], both$AGE[both$model == "screening"])
  expect_false(isTRUE(all.equal(both$sigma[both$model == "proposed"], both$sigma[both$model == "screening"])))
  plot <- combatls_check_plot(check, "centiles", feature = "a", x = "AGE", group = "SEX", model = "both")
  built <- ggplot2::ggplot_build(plot)$data
  expect_length(unique(built[[3L]]$colour), 2)
  expect_true(all(built[[3L]]$linetype == 1))
  expect_error(combatls_check_plot(check, "qq", model = "both"), "only for centile")
})


test_that("centile bands and exact quartile SD comparisons reuse saved fits", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  check <- combatls_model_check(df = data, features = "a", batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ AGE + SEX)
  local_mocked_bindings(.combatls_exploratory_fit = function(...) stop("Unexpected refit"))
  for (group in list(NULL, "SEX")) {
    comparison <- .combatls_centile_variability(check, "a", "AGE", group)
    for (i in seq_len(nrow(comparison))) {
      row <- comparison[i, ]
      observed <- if (is.null(group)) data$AGE else data$AGE[data$SEX == row$curve_group]
      quartiles <- as.numeric(stats::quantile(observed, c(.25, .75)))
      expect_equal(c(row$x_lower, row$x_upper), quartiles)
      if (row$model == "proposed") expect_equal(row$sd_change_pct, 0, tolerance = 1e-7)
      else expect_equal(row$sd_change_pct,
        100 * expm1(unname(check$screen_fits$a$sigma.coefficients["AGE"]) * diff(quartiles)), tolerance = 1e-7)
    }
    plot <- combatls_check_plot(check, "centiles", feature = "a", x = "AGE", group = group, model = "both")
    expect_s3_class(plot$layers[[1L]]$geom, "GeomRibbon")
    expect_s3_class(plot$layers[[2L]]$geom, "GeomRibbon")
    expect_s3_class(plot$layers[[3L]]$geom, "GeomLine")
    expect_equal(plot$data$lower90, plot$data$mu + stats::qnorm(.05) * plot$data$sigma)
    expect_equal(plot$data$upper90, plot$data$mu + stats::qnorm(.95) * plot$data$sigma)
    expect_equal(plot$data$lower50, plot$data$mu + stats::qnorm(.25) * plot$data$sigma)
    expect_equal(plot$data$upper50, plot$data$mu + stats::qnorm(.75) * plot$data$sigma)
    if (is.null(group)) expect_s3_class(plot$facet, "FacetNull")
    else expect_s3_class(plot$facet, "FacetWrap")
    points <- combatls_check_plot(check, "centiles", feature = "a", x = "AGE", group = group,
      model = "both", show.points = TRUE)
    expect_s3_class(points$layers[[4L]]$geom, "GeomPoint")
    expect_equal(points$layers[[4L]]$data$measurement, check$data$a)
    expect_equal(points$layers[[4L]]$data$horizontal, check$data$AGE)
    expect_match(points$labels$caption, "descriptive overlay only")
    expect_no_error(ggplot2::ggplot_build(points))
    display <- .combatls_variability_display(comparison, "AGE", group)
    expect_true(all(c("AGE (25th percentile)", "AGE (75th percentile)", "SD change (%)") %in% names(display)))
  }
  expect_error(combatls_check_centiles(check, "a", "AGE", x.quantiles = c(-.1, 1.1)), "x.quantiles")
  # The R prediction interface still supports arbitrary requested centiles.
  custom <- combatls_check_centiles(check, "a", "AGE", probabilities = .9, n = 3)
  expect_true(all(custom$probability == .9))
  expect_equal(nrow(custom), 3L)
})

test_that("centile feature ordering uses adjusted evidence and retains untested features", {
  check <- list(features = c("a", "b", "c", "d"),
    screening = data.frame(feature = c("a", "b", "c"), covariate = "AGE", q.value = c(.2, 1e-7, NA)))
  expect_equal(.combatls_centile_feature_order(check, "AGE"), c("b", "a", "c", "d"))
  expect_equal(.combatls_centile_feature_order(check, "other"), check$features)
})

test_that("model checks reuse matching smooth and reduced models per feature", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  run_fit <- .combatls_exploratory_fit
  fit_calls <- 0L
  local_mocked_bindings(.combatls_exploratory_fit = function(...) {
    fit_calls <<- fit_calls + 1L
    run_fit(...)
  })
  data <- scale_check_data()
  check <- combatls_model_check(df = data, features = c("a", "b"), batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ pb(AGE) + SEX,
    sigma.formula = ~ pb(AGE) + SEX, screen.formula = ~ pb(AGE) + SEX)
  expect_identical(fit_calls, 6L)
  expect_identical(check$fits, check$screen_fits)
  expect_equal(nrow(check$diagnostics), 8L)
  expect_true(all(check$diagnostics$valid))

  fit_calls <- 0L
  check <- combatls_model_check(df = data, features = c("a", "b"), batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ pb(AGE, df = 3) + SEX,
    candidates = "AGE", screen.formula = ~ AGE)
  expect_identical(fit_calls, 4L)
  for (feature in check$features) {
    expect_identical(check$fits[[feature]], check$reduced_fits[[feature]]$AGE)
    comparison <- gamlss::LR.test(check$fits[[feature]], check$screen_fits[[feature]], print = FALSE)
    row <- check$screening[check$screening$feature == feature, ]
    expect_equal(row$statistic, comparison$chi)
    expect_equal(row$df, comparison$df)
  }
})

test_that("model reuse respects caller-defined formula helpers", {
  make_formula <- function(amount) {
    shift <- function(x) x + amount
    ~ shift(AGE)
  }
  first <- make_formula(.1)
  second <- make_formula(.2)
  expect_false(.combatls_same_formula(first, second))
  expect_true(.combatls_same_formula(first, first))
  copied <- first
  environment(copied) <- new.env(parent = environment(first))
  expect_true(.combatls_same_formula(first, copied))
  expect_false(.combatls_same_formula(~ AGE, ~ log(AGE)))
})

test_that("batched smooth centiles match separate batch predictions", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  data <- scale_check_data()
  # Unequal batch weights and non-treatment contrasts exercise standardization.
  data <- data[-seq_len(30), ]
  contrasts(data$SEX) <- stats::contr.sum(3)
  check <- combatls_model_check(df = data, features = "b", batch = "site",
    covariates = c("AGE", "SEX"), formula = y ~ pb(AGE) + SEX,
    sigma.formula = ~ pb(AGE) + SEX, screen.formula = ~ pb(AGE) + SEX)
  train <- check$model_data
  train$y <- check$data$b
  for (reference in list(NULL, "B")) for (group in list(NULL, "SEX")) {
    check$ref.batch <- reference
    curves <- combatls_check_centiles(check, "b", "AGE", group, probabilities = .5,
      model = "both", n = 5)
    for (source in c("proposed", "screening")) {
      actual <- curves[curves$model == source, ]
      grid <- actual[names(check$model_data)[names(check$model_data) != ".combatls_batch"]]
      weights <- prop.table(table(train$.combatls_batch))
      if (!is.null(reference)) weights[] <- as.numeric(names(weights) == reference)
      mu <- log_sigma <- numeric(nrow(grid))
      fit <- if (source == "proposed") check$fits$b else check$screen_fits$b
      for (level in names(weights)[weights > 0]) {
        grid$.combatls_batch <- factor(level, levels = levels(train$.combatls_batch))
        mu <- mu + weights[[level]] * as.numeric(stats::predict(fit, newdata = grid,
          data = train, what = "mu", type = "link"))
        log_sigma <- log_sigma + weights[[level]] * as.numeric(stats::predict(fit, newdata = grid,
          data = train, what = "sigma", type = "link"))
      }
      expect_equal(actual$mu, mu, tolerance = 1e-8)
      expect_equal(actual$sigma, exp(log_sigma), tolerance = 1e-8)
    }
  }
})
