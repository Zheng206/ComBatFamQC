fit_display_fixture <- function() {
  data <- data.frame(AGE = rep(0:99, each = 4),
    SEX = factor(rep(c("F", "M"), 200), levels = c("M", "F", "unused")),
    feature = seq_len(400))
  fit <- list(converged = TRUE, G.deviance = 0, df.fit = 1,
    mu.coefficients = 0, sigma.coefficients = 0)
  rows <- rev(seq_len(nrow(data)))
  structure(list(data = data, features = "feature", covariates = c("AGE", "SEX"),
    model_data = droplevels(data[c("AGE", "SEX")]),
    fits = list(feature = fit), screen_fits = list(feature = fit),
    parameters = rbind(data.frame(feature = "feature", model = "proposed", row = rows,
      residual = rep(c(-1, 1), 200)), data.frame(feature = "feature", model = "screening",
      row = rows, residual = rep(c(-2, 2), 200)))), class = "combatls_check")
}

test_that("observation bins count each observation once, including boundaries and empty groups", {
  check <- fit_display_fixture()
  binned <- .combatls_observation_bins(check, "feature", "AGE", "SEX")
  expect_equal(sum(binned$bins$n), 400)
  expect_equal(as.numeric(tapply(binned$bins$n, binned$bins$curve_group, sum)), c(200L, 200L))
  expect_false(anyNA(binned$observations$bin))
  expect_equal(range(binned$observations$horizontal), binned$limits)
  expect_equal(length(unique(binned$bins$lower)), length(unique(binned$bins$bin)))
  ungrouped <- .combatls_observation_bins(check, "feature", "AGE")
  expect_equal(sum(ungrouped$bins$n), 400)
  expect_equal(ungrouped$bins$n, as.numeric(rowsum(binned$bins$n, binned$bins$bin)))
  separated <- check
  separated$data$SEX <- factor(ifelse(separated$data$AGE < 50, "F", "M"))
  empty <- .combatls_observation_bins(separated, "feature", "AGE", "SEX")
  expect_true(any(empty$bins$n == 0))
  expect_equal(sum(empty$bins$n), 400)

  check$data$AGE[] <- 5
  constant <- .combatls_observation_bins(check, "feature", "AGE", "SEX")
  expect_equal(nrow(constant$bins), 2L)
  expect_equal(constant$bins$n, c(200L, 200L))
  expect_true(all(is.finite(constant$limits)))
  check$data$feature[1] <- NA
  expect_equal(sum(.combatls_observation_bins(check, "feature", "AGE")$bins$n), 399)
  expect_error(.combatls_observation_bins(check, "feature", "SEX"), "numeric")
})

test_that("profile summaries match fixed numeric, modal, grouped and batch targets", {
  check <- fit_display_fixture()
  expect_equal(.combatls_profile_summary(check, "AGE"),
    "AGE varies \u00b7 SEX = M \u00b7 Batch target = pooled")
  expect_match(.combatls_profile_summary(check, "AGE", "SEX"),
    "SEX = M / F (separate curves)", fixed = TRUE)
  check$ref.batch <- "site A"
  check$covariates <- c(check$covariates, "height")
  check$model_data$height <- seq_len(400)
  summary <- .combatls_profile_summary(check, "AGE")
  expect_match(summary, "height = 200.5", fixed = TRUE)
  expect_match(summary, "Batch target = site A", fixed = TRUE)
})

test_that("count shading darkens with counts on a shared scale across groups", {
  check <- fit_display_fixture()
  check$data$SEX <- factor(ifelse(check$data$AGE < 50, "F", "M"))
  binned <- .combatls_observation_bins(check, "feature", "AGE", "SEX")
  strip <- .combatls_count_strip(binned)
  tiles <- ggplot2::ggplot_build(strip)$data[[1]]
  brightness <- colMeans(grDevices::col2rgb(tiles$fill))
  expect_gt(brightness[which.min(binned$bins$n)], brightness[which.max(binned$bins$n)])
  expect_equal(strip$theme$axis.text$face, "bold")
})

test_that("isolated eligible RMS dots are gray with legends and separate y-axis labels", {
  withr::local_pdf(NULL)
  check <- fit_display_fixture()
  binned <- .combatls_observation_bins(check, "feature", "AGE")
  retained <- c(1L, 2L, 3L, max(binned$bins$bin))
  rows <- binned$observations$row[binned$observations$bin %in% retained]
  check$parameters$residual[!check$parameters$row %in% rows] <- NA_real_
  third <- binned$observations$row[binned$observations$bin == 3L]
  check$parameters$residual[check$parameters$row %in% third[-seq_len(13L)]] <- NA_real_
  diagnostic <- .combatls_fit_view_data(check, "feature", binned)
  expect_true(all(diagnostic$bins$connected[diagnostic$bins$bin %in% c(1L, 2L)]))
  expect_false(any(diagnostic$bins$connected[diagnostic$bins$bin == max(retained)]))
  expect_false(any(diagnostic$bins$connected[diagnostic$bins$bin == 3L]))
  expect_equal(diagnostic$bins$n[diagnostic$bins$bin == 3L], c(13L, 13L))
  expect_true(all(is.finite(diagnostic$bins$rms[diagnostic$bins$bin == 3L])))
  expect_true(all(is.finite(diagnostic$bins$rms[diagnostic$bins$bin == max(retained)])))
  expect_true(all(is.na(diagnostic$bins$rms[!diagnostic$bins$bin %in% retained])))
  plot <- .combatls_fit_view_plot(diagnostic)
  colors <- plot$scales$get_scales("colour")
  expect_equal(unname(colors$map("Unconnected bins")), "#A8AAAE")
  expect_equal(colors$get_limits(), c("Connected bins", "Unconnected bins"))
  expect_equal(plot$theme$legend.text$face, "bold")
  grob <- ggplot2::ggplotGrob(plot)
  expect_true(any(grepl("^strip-l", grob$layout$name)))
})

test_that("residual RMS uses saved normalized residuals at their original observation rows", {
  check <- fit_display_fixture()
  binned <- .combatls_observation_bins(check, "feature", "AGE", "SEX")
  diagnostic <- .combatls_fit_view_data(check, "feature", binned)
  expect_equal(sum(diagnostic$bins$n), 800)
  expect_true(all(diagnostic$bins$rms[diagnostic$bins$model == "proposed"] == 1))
  expect_true(all(diagnostic$bins$rms[diagnostic$bins$model == "screening"] == 2))
  expect_equal(diagnostic$points$horizontal, check$data$AGE[diagnostic$points$row])
  expect_equal(as.character(diagnostic$points$curve_group), as.character(check$data$SEX[diagnostic$points$row]))

  check$parameters$residual[check$parameters$model == "proposed"] <-
    check$data$AGE[check$parameters$row[check$parameters$model == "proposed"]] / 10
  check$parameters$residual[1] <- NA_real_
  diagnostic <- .combatls_fit_view_data(check, "feature", binned)
  proposed <- diagnostic$points[diagnostic$points$model == "proposed", ]
  expect_equal(proposed$residual, proposed$horizontal / 10)
  expect_equal(sum(diagnostic$bins$n), 799)
})

test_that("sparse bins keep counts but never receive RMS points or bridging lines", {
  withr::local_pdf(NULL)
  check <- fit_display_fixture()
  binned <- .combatls_observation_bins(check, "feature", "AGE")
  middle <- ceiling(nrow(binned$bins) / 2)
  removed <- binned$observations$row[binned$observations$bin == middle]
  # Leave one usable residual in the middle bin for each model.
  check$parameters$residual[check$parameters$row %in% removed[-1L]] <- NA_real_
  diagnostic <- .combatls_fit_view_data(check, "feature", binned)
  sparse <- diagnostic$bins$bin == middle
  expect_equal(diagnostic$bins$n[sparse], c(1L, 1L))
  expect_true(all(is.na(diagnostic$bins$rms[sparse])))
  expect_false(any(diagnostic$segments$bin %in% c(middle - 1L, middle)))
  expect_true(all(is.finite(diagnostic$segments$rms)))

  check$fits$feature$converged <- FALSE
  diagnostic <- .combatls_fit_view_data(check, "feature", binned)
  expect_match(diagnostic$status$message[1], "Unavailable")
  expect_false(any(diagnostic$points$model == "proposed"))
  expect_true(any(diagnostic$points$model == "screening"))
  for (view in c("residuals", "qq"))
    expect_no_warning(ggplot2::ggplotGrob(.combatls_fit_view_plot(diagnostic, view)))
  singleton <- fit_display_fixture()
  singleton$parameters <- singleton$parameters[!duplicated(singleton$parameters$model), ]
  diagnostic <- .combatls_fit_view_data(singleton, "feature", binned)
  expect_true(all(is.na(diagnostic$bins$rms)))
  expect_no_warning(ggplot2::ggplotGrob(.combatls_fit_view_plot(diagnostic, "qq")))
  check$screen_fits$feature <- NULL
  check$parameters <- data.frame()
  diagnostic <- .combatls_fit_view_data(check, "feature", binned)
  for (view in c("residuals", "qq"))
    expect_no_warning(ggplot2::ggplotGrob(.combatls_fit_view_plot(diagnostic, view)))
})

test_that("count strips share centile panel columns and residual comparisons render", {
  withr::local_pdf(NULL)
  check <- fit_display_fixture()
  for (group in list(NULL, "SEX")) {
    binned <- .combatls_observation_bins(check, "feature", "AGE", group)
    observations <- binned$observations
    observations$curve_group <- as.character(observations$curve_group)
    plot <- ggplot2::ggplot(observations, ggplot2::aes(horizontal, observed, color = curve_group)) +
      ggplot2::geom_point() + ggplot2::theme_minimal()
    if (!is.null(group)) plot <- plot + ggplot2::facet_wrap(~curve_group)
    combined <- .combatls_centiles_with_counts(plot, binned)
    expect_s3_class(combined, "gTree")
    expect_equal(combined$children[[1]]$widths, combined$children[[2]]$widths)
    strip <- .combatls_count_strip(binned)
    expect_equal(strip$labels$title, "Observed sample size by AGE")
    expect_equal(strip$scales$get_scales("x")$limits, binned$limits)
    upper <- ggplot2::ggplot_build(plot + ggplot2::scale_x_continuous(limits = binned$limits))
    lower <- ggplot2::ggplot_build(strip)
    expect_equal(lapply(upper$layout$panel_params, function(panel) panel$x.range),
      lapply(lower$layout$panel_params, function(panel) panel$x.range))
    if (!is.null(group)) expect_equal(upper$layout$layout$curve_group, lower$layout$layout$curve_group)
    diagnostic <- .combatls_fit_view_data(check, "feature", binned)
    for (view in c("residuals", "qq"))
      expect_no_warning(ggplot2::ggplotGrob(.combatls_fit_view_plot(diagnostic, view)))
  }
})
