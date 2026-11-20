covariance_fixture <- function() {
  x <- seq(-1, 1, length.out = 30)
  basis <- qr.Q(qr(scale(cbind(x, sin(5 * x)), center = TRUE, scale = FALSE)))
  a <- data.frame(first = basis[, 1], second = .8 * basis[, 1] + .6 * basis[, 2])
  b <- data.frame(first = basis[, 1], second = -.8 * basis[, 1] + .6 * basis[, 2])
  data <- rbind(transform(a, site = "A"), transform(b, site = "B"))
  data$site <- factor(data$site)
  list(info = list(features = c("first", "second"), batch = "site"), residual_ml_df = data)
}

test_that("covariance diagnostics recover known correlation distances", {
  result <- covariance_fixture()
  original <- result
  check <- covariance_batch_check(result)
  expect_s3_class(check, "covariance_check")
  expect_equal(check$matrices$A[1, 2], .8)
  expect_equal(check$matrices$B[1, 2], -.8)
  expect_equal(unname(check$pooled), diag(2), tolerance = 1e-12)
  expect_equal(check$pairwise$frobenius, sqrt(2 * 1.6^2))
  expect_equal(check$pairwise$off_diagonal_rms, 1.6)
  expect_equal(check$pairwise$max_abs, 1.6)
  expect_equal(check$pairwise$spectral, 1.6)
  expect_equal(check$to_pooled$spectral, c(.8, .8))
  expect_equal(check$to_pooled$frobenius, rep(sqrt(2 * .8^2), 2))
  for (value in check$distance_matrices) {
    expect_equal(value, t(value))
    expect_equal(unname(diag(value)), c(0, 0))
  }
  expect_identical(result, original)
})

test_that("correlation isolates scale changes and pooling respects sample sizes", {
  result <- covariance_fixture()
  a <- result$residual_ml_df[1:30, ]
  b <- a
  b$site <- factor("B", levels = c("A", "B"))
  b$first <- 2 * b$first + 100
  b$second <- 5 * b$second - 20
  b <- b[1:20, ]
  result$residual_ml_df <- rbind(a, b)
  check <- covariance_batch_check(result, matrix = "covariance")
  s_a <- stats::cov(a[c("first", "second")])
  s_b <- stats::cov(b[c("first", "second")])
  pooled <- (29 * s_a + 19 * s_b) / 48
  divisor <- outer(sqrt(diag(pooled)), sqrt(diag(pooled)))
  expect_equal(check$matrices$A, s_a / divisor)
  expect_equal(check$matrices$B, s_b / divisor)
  expect_equal(check$pooled, pooled / divisor)
  expect_true(check$pairwise$frobenius > 0)
  difference <- check$matrices$A - check$matrices$B
  expect_equal(check$pairwise$spectral, base::norm(difference, type = "2"))
  expect_equal(check$to_pooled$spectral[1L], base::norm(check$matrices$A - check$pooled, type = "2"))
  # The most extreme eigenvalue can be negative: its magnitude defines the norm.
  expect_gt(abs(min(eigen(difference, symmetric = TRUE)$values)), max(eigen(difference, symmetric = TRUE)$values))
  correlations <- covariance_batch_check(result)
  expect_equal(correlations$pooled, (29 * stats::cor(a[1:2]) + 19 * stats::cor(b[1:2])) / 48)

  # Identical rows with different shifts/scales have identical correlations,
  # including their pooled reference; covariance differences remain visible.
  b <- a
  b$site <- factor("B", levels = c("A", "B"))
  b$first <- 2 * b$first + 100
  b$second <- 5 * b$second - 20
  result$residual_ml_df <- rbind(a, b)
  same <- covariance_batch_check(result)
  expect_equal(same$pairwise$frobenius, 0, tolerance = 1e-12)
  expect_equal(same$to_pooled$frobenius, c(0, 0), tolerance = 1e-12)
  expect_equal(same$pairwise$spectral, 0, tolerance = 1e-12)
  expect_true(covariance_batch_check(result, matrix = "covariance")$pairwise$frobenius > 0)
})

test_that("covariance diagnostics report common-case and feature exclusions", {
  result <- covariance_fixture()
  data <- result$residual_ml_df
  tiny <- data[1:2, ]
  tiny$site <- "C"
  missing <- data[1, ]
  missing$site <- NA
  data$first[1] <- NA
  data$second[31] <- Inf
  result$residual_ml_df <- rbind(data, tiny, missing)
  check <- covariance_batch_check(result)
  expect_equal(check$samples$rows_complete, c(29, 29, 2))
  expect_equal(check$samples$rows_excluded, c(1, 1, 0))
  expect_equal(check$samples$eligible, c(TRUE, TRUE, FALSE))
  expect_equal(check$missing_batch_rows, 1)
  expect_named(check$matrices, c("A", "B"))
  expect_equal(check$matrices$A, stats::cor(data[2:30, 1:2]))

  result <- covariance_fixture()
  result$info$features <- c("first", "second", "constant", "batch_constant")
  result$residual_ml_df$constant <- 0
  result$residual_ml_df$batch_constant <- c(rep(0, 30), seq_len(30))
  correlation <- covariance_batch_check(result)
  covariance <- covariance_batch_check(result, matrix = "covariance")
  expect_equal(correlation$excluded_features$feature, c("constant", "batch_constant"))
  expect_equal(correlation$features, c("first", "second"))
  expect_equal(covariance$excluded_features$feature, "constant")
  expect_true(covariance$samples$singular[1])
  expect_true(all(is.finite(covariance$pairwise$frobenius)))
})

test_that("singular matrices remain usable and invalid checks have clear errors", {
  result <- covariance_fixture()
  result$info$features <- c("first", "second", "duplicate")
  result$residual_ml_df$duplicate <- result$residual_ml_df$first
  check <- covariance_batch_check(result)
  expect_true(all(check$samples$singular))
  expect_true(all(check$samples$rank_deficient))
  expect_false(any(check$samples$high_dimensional))
  expect_equal(check$samples$status, rep("Rank deficient", 2))
  expect_true(all(is.finite(check$pairwise$frobenius)))
  expect_true(all(is.finite(check$pairwise$spectral)))
  expect_error(covariance_batch_check(result, features = "first"), "at least two")
  expect_error(covariance_batch_check(result, min.batch = 2), "at least three")
  expect_error(covariance_batch_check(result, min.batch = 31), "at least two batches")
  result$residual_ml_df$second <- "not numeric"
  expect_error(covariance_batch_check(result), "numeric residual")
})

test_that("feature counts and rank bounds use complete rows without excluding small batches", {
  result <- covariance_fixture()
  for (j in 3:6) result$residual_ml_df[[paste0("feature", j)]] <- sin(seq_len(60) * j / 5)
  result$info$features <- setdiff(names(result$residual_ml_df), "site")
  result$residual_ml_df <- result$residual_ml_df[c(1:6, 31:60), ]
  for (type in c("correlation", "covariance")) {
    check <- covariance_batch_check(result, matrix = type)
    expect_equal(check$samples$rows_complete, c(6L, 30L))
    expect_equal(check$samples$feature_count, c(6L, 6L))
    expect_equal(check$samples$matrix_rank, c(5L, 6L))
    expect_equal(check$samples$high_dimensional, c(TRUE, FALSE))
    expect_equal(check$samples$rank_deficient, c(TRUE, FALSE))
    expect_equal(check$samples$status, c("Rank deficient", "Full rank"))
    expect_true(all(check$samples$eligible))
    expect_true(all(is.finite(unlist(check$pairwise[3:6]))))
    expect_identical(unique(covariance_check_plot(check)$data$panel), names(check$matrices))
  }
})

test_that("shared PCA spread equals projected complete-row score SDs", {
  result <- covariance_fixture()
  result$residual_ml_df$first[1:30] <- 3 * result$residual_ml_df$first[1:30] + 100
  result$residual_ml_df$second[31:60] <- result$residual_ml_df$second[31:60] - 50
  result$residual_ml_df$first[1L] <- NA_real_
  check <- covariance_batch_check(result)
  pca <- check$pca
  expect_equal(pca$retained, 2L)
  expect_equal(pca$explained, 1)
  expect_equal(crossprod(pca$loadings), diag(2), ignore_attr = TRUE)
  for (batch in names(check$matrices)) {
    data <- result$residual_ml_df
    data <- data[data$site == batch & stats::complete.cases(data), check$features]
    scores <- scale(as.matrix(data), center = TRUE, scale = pca$feature_sd) %*% pca$loadings
    actual <- pca$spread[pca$spread$batch == batch, ]
    expect_equal(actual$variance, unname(apply(scores, 2L, stats::var)))
    expect_equal(actual$sd_ratio,
      unname(apply(scores, 2L, stats::sd)) / sqrt(pca$eigenvalues[seq_len(pca$retained)]))
  }
  sizes <- check$samples$rows_complete
  ratios <- matrix(pca$spread$sd_ratio^2, nrow = pca$retained)
  expect_equal(as.vector(ratios %*% (sizes - 1) / sum(sizes - 1)), rep(1, pca$retained))
  expect_equal(covariance_batch_check(result, matrix = "covariance")$pca, pca)
  capped <- covariance_batch_check(result, pca.variance = 1, pca.max = 1)$pca
  expect_equal(capped$retained, 1L)
  expect_equal(capped$explained, capped$eigenvalues[1L] / sum(capped$eigenvalues))
  expect_lt(capped$explained, 1)
  expect_error(covariance_batch_check(result, pca.variance = 0), "pca.variance")
  expect_error(covariance_batch_check(result, pca.max = 1.5), "pca.max")
})

test_that("PCA retention respects sample support and shared numerical rank", {
  result <- covariance_fixture()
  result$residual_ml_df <- result$residual_ml_df[c(1:3, 31:60), ]
  check <- covariance_batch_check(result, pca.variance = 1)
  expect_equal(check$pca$retained, 1L)
  expect_equal(check$pca$sample_limit, 1L)
  expect_lt(check$pca$explained, 1)
  result <- covariance_fixture()
  result$residual_ml_df$second <- result$residual_ml_df$first
  check <- covariance_batch_check(result, pca.variance = 1)
  expect_equal(check$pca$retained, 1L)
  expect_equal(check$pca$explained, 1)
  expect_true(all(is.finite(check$pca$spread$sd_ratio)))
})

test_that("covariance plots retain matrices, shared scales, and distance choices", {
  check <- covariance_batch_check(covariance_fixture())
  for (type in c("batch", "difference", "distance", "pca")) {
    plot <- covariance_check_plot(check, type)
    expect_s3_class(plot, "ggplot")
    expect_no_error(ggplot2::ggplot_build(plot))
  }
  batch <- covariance_check_plot(check)
  expect_equal(batch$scales$get_scales("fill")$limits, c(-1, 1))
  expect_s3_class(batch$theme$axis.text.y, "element_blank")
  expect_s3_class(covariance_check_plot(check, show.labels = TRUE)$theme$axis.text.y, "element_text")
  for (metric in names(check$distance_matrices)) {
    plot <- covariance_check_plot(check, "distance", metric = metric)
    expect_equal(sort(plot$data$value), sort(as.vector(check$distance_matrices[[metric]])))
  }
  full <- covariance_check_plot(check, "difference")
  subset <- covariance_check_plot(check, "difference", batches = "A")
  expect_equal(full$scales$get_scales("fill")$limits, subset$scales$get_scales("fill")$limits)
  expect_error(covariance_check_plot(check, batches = character()), "eligible batch")
})

test_that("Shiny covariance display filters all results without recomputing", {
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  run_check <- covariance_batch_check
  calls <- 0L
  local_mocked_bindings(covariance_batch_check = function(...) {
    calls <<- calls + 1L
    run_check(...)
  }, combat_harm = function(...) stop("Covariance diagnostics must not harmonize"))
  app <- comfam_shiny(result)
  page <- app$httpHandler(list(REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = ""))$content
  sidebar <- substr(page, regexpr('id="combatqc-sidebar"', page, fixed = TRUE)[1],
    regexpr('id="combatqc-main"', page, fixed = TRUE)[1])
  expect_match(sidebar, 'id="covariance_matrix"', fixed = TRUE)
  expect_match(sidebar, 'id="covariance_plot_batches"', fixed = TRUE)
  expect_match(sidebar, 'id="covariance_pca_variance"', fixed = TRUE)
  expect_false(grepl("For correlation matrices, Frobenius distance equals", page, fixed = TRUE))
  expect_false(grepl('id="covariance_features"', page, fixed = TRUE))
  expect_false(grepl('id="covariance_distance_metric"', page, fixed = TRUE))
  expect_false(grepl('id="covariance_distances"', page, fixed = TRUE))
  expect_match(page, "Distance metric formulas", fixed = TRUE)
  expect_false(grepl("Detailed distances", page, fixed = TRUE))
  expect_false(grepl("covariance_show_frobenius", page, fixed = TRUE))
  expect_match(page, 'id="covariance_pca"', fixed = TRUE)
  expect_equal(length(gregexpr('<math xmlns=', page, fixed = TRUE)[[1]]), 4L)
  testServer(app, {
    session$setInputs(covariance_matrix = "correlation",
      covariance_plot_batches = levels(result$info$df[[result$info$batch]]),
      covariance_view = "batch", covariance_show_labels = FALSE)
    expect_false(covariance_ready())
    session$setInputs(covariance_run = 1)
    expect_true(covariance_ready())
    expect_identical(calls, 1L)
    saved <- covariance_check_s()
    expect_s3_class(saved, "covariance_check")
    expect_true(nzchar(output$covariance_matrices$src))
    expect_match(output$covariance_status, "3 eligible batches", fixed = TRUE)
    expect_match(output$covariance_pca_status, paste0(saved$pca$retained, " retained PC"), fixed = TRUE)
    expect_match(output$covariance_pca_status, sprintf("%.1f%%", 100 * saved$pca$explained), fixed = TRUE)
    expect_equal(nrow(covariance_sample_rows()), 3L)
    expect_equal(nrow(covariance_pairwise_rows()), 3L)
    expect_true("Spectral norm" %in% names(covariance_pairwise_rows()))
    expect_true("Largest absolute difference" %in% names(covariance_sample_rows()))
    expect_false("Excluded rows" %in% names(covariance_sample_rows()))
    expect_equal(covariance_pairwise_rows()$`Spectral norm`, saved$pairwise$spectral)
    expect_equal(covariance_sample_rows()$`Spectral norm`, saved$to_pooled$spectral)
    expect_true("Frobenius distance" %in% names(covariance_pairwise_rows()))
    expect_equal(covariance_sample_rows()$`Features (p)`, saved$samples$feature_count)
    expect_equal(covariance_sample_rows()$`Complete rows (n)`, saved$samples$rows_complete)
    expect_no_error(output$covariance_sample_table)
    expect_no_error(output$covariance_pairwise_table)
    expect_true(nzchar(output$covariance_pca$src))

    session$setInputs(covariance_pca_variance = 1)
    expect_equal(covariance_pca_check()$pca$retained, 1L)
    expect_equal(nlevels(covariance_pca_plot()$data$pc), 1L)
    expect_match(output$covariance_pca_status, "target 1%", fixed = TRUE)
    session$setInputs(covariance_pca_variance = 100)
    expect_equal(covariance_pca_check()$pca$retained, ncol(saved$pca$available_loadings))
    expect_match(output$covariance_pca_status, "target 100%", fixed = TRUE)
    expect_identical(covariance_pca_check()$pca$available_loadings, saved$pca$available_loadings)
    expect_identical(covariance_sample_rows()$`Spectral norm`, saved$to_pooled$spectral)
    expect_true(covariance_ready())
    expect_identical(covariance_check_s(), saved)
    expect_identical(calls, 1L)
    session$setInputs(covariance_pca_variance = 90)

    session$setInputs(covariance_plot_batches = names(saved$matrices)[1:2])
    expect_equal(nrow(covariance_sample_rows()), 2L)
    expect_equal(nrow(covariance_pairwise_rows()), 1L)
    expect_length(unique(covariance_matrices_plot()$data$panel), 2L)
    expect_equal(as.character(unique(covariance_pca_plot()$data$batch)), names(saved$matrices)[1:2])
    expect_equal(covariance_pca_plot()$scales$get_scales("fill")$limits,
      covariance_check_plot(saved, "pca")$scales$get_scales("fill")$limits)
    session$setInputs(covariance_view = "difference", covariance_show_labels = TRUE,
      covariance_plot_batches = names(saved$matrices)[1L])
    expect_true(nzchar(output$covariance_matrices$src))
    expect_length(unique(covariance_matrices_plot()$data$panel), 1L)
    expect_equal(nrow(covariance_sample_rows()), 1L)
    expect_equal(nrow(covariance_pairwise_rows()), 0L)
    expect_true("Frobenius distance" %in% names(covariance_pairwise_rows()))
    expect_equal(covariance_sample_rows()$`Off-diagonal RMS`, saved$to_pooled$off_diagonal_rms[1L])
    expect_identical(covariance_check_s(), saved)
    expect_identical(calls, 1L)

    session$setInputs(covariance_plot_batches = character())
    expect_equal(nrow(covariance_sample_rows()), 0L)
    expect_equal(nrow(covariance_pairwise_rows()), 0L)
    expect_error(covariance_matrices_plot(), class = "shiny.silent.error")
    expect_no_error(output$covariance_sample_table)
    expect_no_error(output$covariance_pairwise_table)
    expect_error(covariance_pca_plot(), class = "shiny.silent.error")
    session$setInputs(covariance_plot_batches = names(saved$matrices), covariance_matrix = "covariance")
    expect_false(covariance_ready())
    expect_match(output$covariance_prompt$html, "settings have changed")
    expect_error(current_covariance_check(), class = "shiny.silent.error")
    session$setInputs(covariance_run = 2)
    expect_true(covariance_ready())
    expect_identical(calls, 2L)
    expect_identical(current_covariance_check()$matrix, "covariance")

    # Editing a draft does not invalidate the saved check until Apply is clicked.
    session$setInputs(covariance_feature_choices = result$info$features[1:2])
    expect_true(covariance_ready())
    session$setInputs(covariance_apply_features = 1)
    expect_false(covariance_ready())
    expect_equal(covariance_features_s(), result$info$features[1:2])
    session$setInputs(covariance_run = 3)
    expect_true(covariance_ready())
    expect_identical(calls, 3L)
    expect_equal(current_covariance_check()$features, result$info$features[1:2])
  })
})

test_that("Shiny sampling caution covers hidden batches and clears for a smaller feature set", {
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  data <- result$residual_ml_df
  rows <- unlist(lapply(split(seq_len(nrow(data)), data[[result$info$batch]]), head, 3L))
  result$residual_ml_df <- data[rows, , drop = FALSE]
  testServer(comfam_shiny(result), {
    session$setInputs(covariance_matrix = "correlation", covariance_plot_batches = character(),
      covariance_run = 1)
    expect_true(covariance_ready())
    expect_match(output$covariance_sampling_caution$html, "p &gt;= n", fixed = TRUE)
    expect_match(output$covariance_sampling_caution$html, "hidden batches", fixed = TRUE)
    session$setInputs(covariance_plot_batches = character())
    expect_match(output$covariance_sampling_caution$html, "Limited sample support", fixed = TRUE)
    expect_true(all(covariance_check_s()$samples$rank_deficient))
    session$setInputs(covariance_feature_choices = result$info$features[1:2], covariance_apply_features = 1)
    session$setInputs(covariance_run = 2)
    expect_false(any(covariance_check_s()$samples$high_dimensional))
    expect_null(output$covariance_sampling_caution)
  })
})

test_that("feature selection uses a searchable dialog and explicit Apply", {
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  extra <- paste0("feature_", 5:62)
  for (name in extra) {
    result$info$df[[name]] <- result$info$df[[result$info$features[1L]]]
    result$residual_ml_df[[name]] <- result$residual_ml_df[[result$info$features[1L]]]
  }
  result$info$features <- c(result$info$features, extra)
  testServer(comfam_shiny(result), {
    expect_equal(covariance_features_s(), utils::head(result$info$features, 20))
    expect_match(output$covariance_feature_summary, "20 of 62 features selected", fixed = TRUE)
    modal <- NULL
    updates <- list()
    send_modal <- session$sendModal
    send_input <- session$sendInputMessage
    session$sendModal <- function(type, message) {
      if (type == "show") modal <<- message$html
    }
    session$sendInputMessage <- function(inputId, message) updates[[inputId]] <<- message
    session$setInputs(covariance_change_features = 1)
    expect_match(modal, 'id="covariance_feature_search"', fixed = TRUE)
    expect_match(modal, 'id="covariance_feature_choices"', fixed = TRUE)
    expect_match(modal, 'value="feature_62"', fixed = TRUE)
    session$setInputs(covariance_all_features = 1)
    expect_equal(updates$covariance_feature_choices$value, result$info$features)
    session$setInputs(covariance_clear_features = 1)
    expect_length(updates$covariance_feature_choices$value, 0)
    session$setInputs(covariance_first_features = 1)
    expect_equal(updates$covariance_feature_choices$value, utils::head(result$info$features, 20))
    expect_length(covariance_features_s(), 20L)
    session$setInputs(covariance_feature_search = "feature_62", covariance_feature_choices = result$info$features)
    expect_length(covariance_features_s(), 20L)
    session$setInputs(covariance_apply_features = 1)
    expect_equal(covariance_features_s(), result$info$features)
    expect_match(output$covariance_feature_summary, "62 of 62 features selected", fixed = TRUE)
    session$setInputs(covariance_feature_choices = result$info$features[1L], covariance_apply_features = 2)
    expect_length(covariance_features_s(), 62L)
    session$sendModal <- send_modal
    session$sendInputMessage <- send_input
  })
})
