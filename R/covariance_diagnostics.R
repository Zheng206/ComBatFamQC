#' Check Batch Differences in Residual Covariance
#'
#' Compare batch-specific covariance or correlation matrices using the prepared
#' residuals from `visual_prep()`. No models are fitted and no data are harmonized.
#'
#' @param result Output of `visual_prep()`.
#' @param features Features to compare; defaults to all prepared features.
#' @param matrix Matrix type: `"correlation"` or `"covariance"`. Covariance is
#'   expressed in pooled within-batch SD units to make feature scales comparable.
#' @param min.batch Minimum number of complete residual rows per batch, at least
#'   three. Defaults to three.
#' @param pca.variance Target fraction of pooled within-batch variance explained
#'   by retained PCs, in (0, 1]. Defaults to 0.9.
#' @param pca.max Maximum retained PCs. Defaults to 20; also limited by numerical
#'   rank and to fewer than the smallest eligible batch's complete row count minus one.
#' @return A `covariance_check` list containing batch matrices, the pooled matrix,
#'   batch sample sizes, feature counts, matrix ranks, rank-deficiency and
#'   high-dimensional flags, eligibility, excluded features, distances from the
#'   pooled matrix, pairwise distances, symmetric distance matrices, and `pca`
#'   (shared loadings, retained count, achieved variance explained, and batch PC SD ratios).
#' @details
#' Uses `result$residual_ml_df`, whose fitted location model includes biological
#' covariates and batch. Complete rows across all selected features are used
#' within each batch. Nonfinite residuals and missing batch labels are excluded.
#' Batches below `min.batch` are excluded. Features with zero pooled within-batch
#' variance are excluded; correlation additionally requires positive variance in
#' every eligible batch. Exclusions are reported in the returned object.
#'
#' The pooled covariance is the average of within-batch sample covariances,
#' weighted by their residual row counts minus one. Correlation comparisons use
#' the same weights to average within-batch correlation matrices. Covariance comparisons
#' divide each matrix entry by the pooled SDs of its two features. Singular sample
#' matrices are retained; the distances do not require inversion.
#' With \eqn{n_b} complete rows and \eqn{p} usable features, sample matrix rank
#' is at most \eqn{\min(p, n_b-1)}. The sample metadata flags \eqn{p \ge n_b}
#' separately from numerical rank deficiency, which can also arise from collinearity.
#'
#' Distances are the Frobenius norm of the matrix difference, the root mean square
#' difference over off-diagonal entries, the largest absolute entry difference,
#' and the spectral norm. For symmetric matrices \eqn{A} and \eqn{B}, let
#' \eqn{\Delta = A-B}, with \eqn{p} compared features. The metric formulas are:
#' \deqn{d_{\mathrm{RMS}} = \sqrt{\frac{\sum_{i \ne j}\Delta_{ij}^2}{p(p-1)}}}
#' \deqn{d_{\max} = \max_{i,j}|\Delta_{ij}|}
#' \deqn{d_2 = \|\Delta\|_2 = \sigma_{\max}(\Delta) = \max_k|\lambda_k(\Delta)|}
#' \deqn{d_F = \|\Delta\|_F = \sqrt{\sum_{i=1}^{p}\sum_{j=1}^{p}\Delta_{ij}^2}}
#' Spectral norm is the largest singular value, or the largest absolute eigenvalue
#' for these symmetric differences; it captures the largest absolute change along
#' a unit-length feature direction. Only off-diagonal RMS excludes diagonal entries.
#'
#' They are descriptive, have no universal decision threshold, and are sensitive
#' to sampling variability and the selected features. Large correlation differences
#' can motivate assessing CovBat beyond location and scale harmonization. These
#' diagnostics do not establish that CovBat is needed or that batch is the cause;
#' covariate composition, model adequacy, outliers, and repeated observations can
#' influence the results. Counts refer to rows, not independent subjects.
#' Off-diagonal RMS is the primary descriptive metric. When \eqn{p \ge n_b},
#' sampling variability can be substantial; spectral norm and the largest entry
#' difference may emphasize unstable directions or pairs. A large distance alone
#' does not establish a batch effect. Consider fewer scientifically chosen features
#' or a shared PCA basis with fewer components than the smallest batch can estimate.
#' A PCA comparison describes only the retained subspace; separate batch-specific
#' PCA bases do not provide comparable feature coordinates.
#'
#' PCA uses the same complete rows and usable features as the covariance check.
#' One shared basis diagonalizes the pooled within-batch covariance after common
#' scaling by pooled feature SDs, regardless of the selected matrix type. Thus
#' within-batch feature scale differences remain visible in PC spread. Batch PC
#' variances are computed as the quadratic forms of these shared loadings and each
#' batch covariance, equivalent to variances of projected complete-row scores.
#' Spread is batch PC SD divided by pooled within-batch PC SD; 1 means equal
#' estimated spread. Achieved variance explained refers to the retained subspace
#' of the pooled, scaled within-batch variance. It can fall below the target when
#' the sample-support or PC-count limit applies. These are descriptive comparisons,
#' and marginal PC spreads do not summarize all differences in PC covariances.
#' Projections for all PCs allowed by the component and sample-support limits
#' are cached in `pca$available_loadings` and `pca$available_spread`, allowing
#' Shiny to change its variance target without recomputing the shared basis.
#' @references
#' Chen et al. (2022), doi:10.1002/hbm.25688.
#' @md
#' @export
covariance_batch_check <- function(result, features = result$info$features,
                                   matrix = c("correlation", "covariance"), min.batch = 3L,
                                   pca.variance = 0.9, pca.max = 20L) {
  matrix <- match.arg(matrix)
  features <- unique(features)
  if (!is.character(features) || length(features) < 2L || anyNA(features) ||
      any(!features %in% result$info$features))
    stop("Select at least two prepared features for covariance diagnostics.", call. = FALSE)
  if (length(min.batch) != 1L || !is.numeric(min.batch) || !is.finite(min.batch) ||
      min.batch < 3 || min.batch != as.integer(min.batch))
    stop("min.batch must be an integer of at least three.", call. = FALSE)
  if (length(pca.variance) != 1L || !is.numeric(pca.variance) || !is.finite(pca.variance) ||
      pca.variance <= 0 || pca.variance > 1)
    stop("pca.variance must be a fraction in (0, 1].", call. = FALSE)
  if (length(pca.max) != 1L || !is.numeric(pca.max) || !is.finite(pca.max) ||
      pca.max < 1 || pca.max > .Machine$integer.max || pca.max != as.integer(pca.max))
    stop("pca.max must be a positive integer.", call. = FALSE)
  data <- result$residual_ml_df
  batch <- result$info$batch
  if (!is.data.frame(data) || length(batch) != 1L || !all(c(features, batch) %in% names(data)))
    stop("Prepared residuals must contain the selected features and batch column.", call. = FALSE)
  if (!all(vapply(data[features], is.numeric, logical(1))))
    stop("Covariance diagnostics require numeric residual features.", call. = FALSE)
  values <- as.matrix(data[features])
  groups <- as.character(data[[batch]])
  levels <- if (is.factor(data[[batch]])) levels(droplevels(data[[batch]])) else unique(groups[!is.na(groups)])
  complete <- rowSums(!is.finite(values)) == 0L
  samples <- do.call(rbind, lapply(levels, function(level) {
    rows <- which(!is.na(groups) & groups == level)
    n <- sum(complete[rows])
    data.frame(batch = level, rows_total = length(rows), rows_complete = n,
      rows_excluded = length(rows) - n, eligible = n >= min.batch,
      status = if (n >= min.batch) "Included" else "Too few complete rows", stringsAsFactors = FALSE)
  }))
  if (is.null(samples) || sum(samples$eligible) < 2L)
    stop("Covariance diagnostics require at least two batches with enough complete rows.", call. = FALSE)
  batches <- samples$batch[samples$eligible]
  sizes <- samples$rows_complete[samples$eligible]
  covariances <- setNames(lapply(batches, function(level) {
    stats::cov(values[which(!is.na(groups) & groups == level & complete), , drop = FALSE])
  }), batches)
  variances <- vapply(covariances, diag, numeric(length(features)))
  pooled <- Reduce(`+`, Map(function(covariance, n) covariance * (n - 1), covariances, sizes)) / sum(sizes - 1)
  keep <- is.finite(diag(pooled)) & diag(pooled) > 0
  if (matrix == "correlation") keep <- keep & apply(variances, 1L, function(x) all(is.finite(x) & x > 0))
  excluded <- data.frame(feature = features[!keep], reason = if (matrix == "correlation")
    rep("Zero or nonfinite residual variance in an eligible batch", sum(!keep)) else
    rep("Zero or nonfinite pooled within-batch variance", sum(!keep)), stringsAsFactors = FALSE)
  if (sum(keep) < 2L)
    stop("Fewer than two usable features remain; check within-batch residual variation or select covariance matrices.", call. = FALSE)
  covariances <- lapply(covariances, function(value) value[keep, keep, drop = FALSE])
  pooled <- pooled[keep, keep, drop = FALSE]
  if (any(!is.finite(pooled)) || any(vapply(covariances, function(value) any(!is.finite(value)), logical(1))))
    stop("Residual covariance estimates are nonfinite; check feature magnitudes and variation.", call. = FALSE)
  if (matrix == "correlation") {
    matrices <- lapply(covariances, stats::cov2cor)
    pooled_matrix <- Reduce(`+`, Map(function(value, n) value * (n - 1), matrices, sizes)) / sum(sizes - 1)
  } else {
    divisor <- outer(sqrt(diag(pooled)), sqrt(diag(pooled)))
    matrices <- lapply(covariances, function(value) value / divisor)
    pooled_matrix <- pooled / divisor
  }
  samples$matrix_rank <- NA_integer_
  samples$feature_count <- sum(keep)
  samples$matrix_rank[samples$eligible] <- pmin(sizes - 1L,
    vapply(matrices, function(value) qr(value)$rank, integer(1)))
  samples$rank_deficient <- samples$eligible & !is.na(samples$matrix_rank) & samples$matrix_rank < samples$feature_count
  samples$high_dimensional <- samples$eligible & samples$feature_count >= samples$rows_complete
  samples$singular <- samples$rank_deficient
  samples$status[samples$eligible] <- ifelse(samples$rank_deficient[samples$eligible], "Rank deficient", "Full rank")
  metrics <- function(left, right) {
    difference <- left - right
    c(frobenius = sqrt(sum(difference^2)),
      off_diagonal_rms = sqrt(mean(difference[row(difference) != col(difference)]^2)),
      max_abs = max(abs(difference)),
      spectral = max(abs(eigen(difference, symmetric = TRUE, only.values = TRUE)$values)))
  }
  to_pooled <- do.call(rbind, lapply(seq_along(batches), function(i) {
    values <- metrics(matrices[[i]], pooled_matrix)
    data.frame(batch = batches[i], rows = sizes[i], frobenius = unname(values[1L]),
      off_diagonal_rms = unname(values[2L]), max_abs = unname(values[3L]),
      spectral = unname(values["spectral"]))
  }))
  pairs <- utils::combn(seq_along(batches), 2L)
  pairwise <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(i) {
    pair <- pairs[, i]
    values <- metrics(matrices[[pair[1L]]], matrices[[pair[2L]]])
    data.frame(batch_1 = batches[pair[1L]], batch_2 = batches[pair[2L]],
      frobenius = unname(values[1L]), off_diagonal_rms = unname(values[2L]), max_abs = unname(values[3L]),
      spectral = unname(values["spectral"]))
  }))
  metric_names <- c("frobenius", "off_diagonal_rms", "max_abs", "spectral")
  pca <- .covariance_shared_pca(covariances, pooled, sizes, pca.variance, pca.max)
  distance_matrices <- setNames(lapply(metric_names, function(metric) {
    out <- base::matrix(0, length(batches), length(batches), dimnames = list(batches, batches))
    for (i in seq_len(ncol(pairs))) {
      out[pairs[1L, i], pairs[2L, i]] <- out[pairs[2L, i], pairs[1L, i]] <- pairwise[[metric]][i]
    }
    out
  }), metric_names)
  structure(list(matrices = matrices, pooled = pooled_matrix, matrix = matrix,
    features = features[keep], requested_features = features, excluded_features = excluded,
    samples = samples, missing_batch_rows = sum(is.na(groups)), to_pooled = to_pooled,
    pairwise = pairwise, distance_matrices = distance_matrices, pca = pca, batch = batch,
    source = "Prepared location-model residuals (residual_ml_df)"), class = "covariance_check")
}

.covariance_shared_pca <- function(covariances, pooled, sizes, target, max_pcs) {
  feature_sd <- sqrt(diag(pooled))
  divisor <- outer(feature_sd, feature_sd)
  decomposition <- eigen(pooled / divisor, symmetric = TRUE)
  eigenvalues <- pmax(decomposition$values, 0)
  rank <- sum(eigenvalues > max(eigenvalues) * nrow(pooled) * .Machine$double.eps)
  proportion <- eigenvalues / sum(eigenvalues)
  components <- seq_len(min(rank, max_pcs, min(sizes) - 2L))
  loadings <- decomposition$vectors[, components, drop = FALSE]
  dimnames(loadings) <- list(rownames(pooled), paste0("PC", components))
  spread <- do.call(rbind, lapply(names(covariances), function(batch) {
    projected <- crossprod(loadings, (covariances[[batch]] / divisor) %*% loadings)
    variance <- pmax(diag(projected), 0)
    data.frame(batch = batch, pc = colnames(loadings), variance = variance,
      sd_ratio = sqrt(variance / eigenvalues[components]), stringsAsFactors = FALSE)
  }))
  .covariance_pca_select(list(available_loadings = loadings, available_spread = spread,
    feature_sd = feature_sd, eigenvalues = eigenvalues, proportion = proportion,
    max_pcs = max_pcs, sample_limit = min(sizes) - 2L), target)
}

.covariance_pca_select <- function(pca, target) {
  needed <- which(cumsum(pca$proportion) >= target - 1e-12)[1L]
  retained <- min(needed, ncol(pca$available_loadings))
  pca$loadings <- pca$available_loadings[, seq_len(retained), drop = FALSE]
  pca$spread <- pca$available_spread[pca$available_spread$pc %in% colnames(pca$loadings), , drop = FALSE]
  rownames(pca$spread) <- NULL
  pca$retained <- retained
  pca$explained <- sum(pca$proportion[seq_len(retained)])
  pca$target <- target
  pca
}

#' Plot Batch Covariance Diagnostics
#' @param check Output of `covariance_batch_check()`.
#' @param type Show batch matrices, differences from the pooled matrix, or
#'   pairwise batch distances, or a batch by PC spread heatmap (`"pca"`).
#' @param batches Batch names to display in matrix panels. Defaults to all eligible
#'   batches. Also filters PCA rows without changing the shared basis, retained
#'   PCs, pooled PC SD reference, or color limits. Distance plots include every eligible batch.
#' @param show.labels Show feature names on matrix axes (default FALSE).
#' @param metric Distance metric: `"frobenius"`, `"off_diagonal_rms"`, `"max_abs"`,
#'   or `"spectral"`.
#' @return A ggplot with common feature ordering and a shared color scale across
#'   matrix panels. Correlations range from -1 to 1; other matrix scales are
#'   symmetric around zero using all eligible batches, including hidden panels.
#' @md
#' @export
covariance_check_plot <- function(check, type = c("batch", "difference", "distance", "pca"),
                                  batches = names(check$matrices), show.labels = FALSE,
                                  metric = c("frobenius", "off_diagonal_rms", "max_abs", "spectral")) {
  stopifnot(inherits(check, "covariance_check"))
  type <- match.arg(type)
  metric <- match.arg(metric)
  if (type == "pca") return(.covariance_pca_plot(check, batches))
  if (type == "distance") {
    value <- check$distance_matrices[[metric]]
    data <- expand.grid(horizontal = colnames(value), vertical = rownames(value), stringsAsFactors = FALSE)
    data$value <- as.vector(value)
    data$horizontal <- factor(data$horizontal, levels = colnames(value))
    data$vertical <- factor(data$vertical, levels = rev(rownames(value)))
    title <- c(frobenius = "Frobenius distance", off_diagonal_rms = "Off-diagonal RMS difference",
               max_abs = "Largest absolute difference", spectral = "Spectral norm")[[metric]]
    return(ggplot2::ggplot(data, ggplot2::aes(x = .data[["horizontal"]], y = .data[["vertical"]], fill = .data[["value"]])) +
      ggplot2::geom_tile(color = "white", linewidth = .25) +
      ggplot2::scale_fill_gradient(low = "#EFF4F8", high = "#234E70", name = title) +
      ggplot2::coord_fixed() + ggplot2::labs(x = "Batch", y = "Batch") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = ggplot2::element_text(face = "bold"),
        legend.title = ggplot2::element_text(face = "bold")))
  }
  if (!length(batches) || any(!batches %in% names(check$matrices)))
    stop("Select at least one eligible batch for the matrix panels.", call. = FALSE)
  matrices <- if (type == "difference") lapply(check$matrices, function(value) value - check$pooled) else check$matrices
  limit <- if (type == "batch" && check$matrix == "correlation") 1 else max(abs(unlist(matrices)))
  if (limit == 0) limit <- 1
  data <- do.call(rbind, lapply(unique(batches), function(batch) {
    value <- matrices[[batch]]
    data <- expand.grid(horizontal = check$features, vertical = check$features, stringsAsFactors = FALSE)
    data$value <- as.vector(value)
    data$panel <- batch
    data
  }))
  data$horizontal <- factor(data$horizontal, levels = check$features)
  data$vertical <- factor(data$vertical, levels = rev(check$features))
  label <- if (check$matrix == "correlation") "Correlation" else "Covariance\n(pooled SD units)"
  if (type == "difference") label <- paste("Difference from pooled", tolower(label), sep = "\n")
  ggplot2::ggplot(data, ggplot2::aes(x = .data[["horizontal"]], y = .data[["vertical"]], fill = .data[["value"]])) +
    ggplot2::geom_tile() + ggplot2::facet_wrap(~panel,
      ncol = .covariance_panel_columns(length(unique(batches)), isTRUE(show.labels))) +
    ggplot2::scale_fill_gradient2(low = "#24558A", mid = "#F7F9FC", high = "#C75462",
      midpoint = 0, limits = c(-limit, limit), name = label) +
    ggplot2::coord_fixed() + ggplot2::labs(x = "Feature", y = "Feature") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = if (isTRUE(show.labels)) ggplot2::element_text(angle = 90, hjust = 1, size = 8, face = "bold") else ggplot2::element_blank(),
      axis.text.y = if (isTRUE(show.labels)) ggplot2::element_text(size = 8, face = "bold") else ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"))
}

.covariance_panel_columns <- function(count, show.labels = FALSE) {
  max(1L, min(if (show.labels) 2L else 3L, count))
}

.covariance_pca_plot <- function(check, batches) {
  if (!length(batches) || any(!batches %in% names(check$matrices)))
    stop("Select at least one eligible batch for the PCA heatmap.", call. = FALSE)
  data <- check$pca$spread
  limit <- max(2, ceiling(max(data$sd_ratio) * 2) / 2)
  data <- data[data$batch %in% batches, , drop = FALSE]
  data$pc <- factor(data$pc, levels = colnames(check$pca$loadings))
  data$batch <- factor(data$batch, levels = rev(unique(batches)))
  n <- check$samples$rows_complete[match(levels(data$batch), check$samples$batch)]
  labels <- setNames(paste0(levels(data$batch), " (n = ", n, ")"), levels(data$batch))
  ggplot2::ggplot(data, ggplot2::aes(x = .data[["pc"]], y = .data[["batch"]], fill = .data[["sd_ratio"]])) +
    ggplot2::geom_tile(color = "white", linewidth = .5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data[["sd_ratio"]])),
      size = 4, fontface = "bold") +
    ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::scale_fill_gradient2(low = "#79AFD3", mid = "#F7F9FC", high = "#DD8A83",
      midpoint = 1, limits = c(0, limit), name = "PC SD / pooled PC SD") +
    ggplot2::labs(x = "Shared principal component", y = "Batch") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom", legend.title = ggplot2::element_text(face = "bold"))
}

# Native MathML keeps the formulas available without an external math renderer.
.covariance_metric_formulas <- function() {
  delta <- "<mi>&#x394;</mi>"
  delta_ij <- paste0("<msub>", delta, "<mrow><mi>i</mi><mi>j</mi></mrow></msub>")
  squared <- paste0("<msup>", delta_ij, "<mn>2</mn></msup>")
  sum_squares <- function(index) paste0("<munder><mo>&#x2211;</mo><mrow>", index, "</mrow></munder>", squared)
  norm <- function(subscript) paste0("<msub><mrow><mo>&#x2016;</mo>", delta,
    "<mo>&#x2016;</mo></mrow><mi>", subscript, "</mi></msub>")
  metric_row <- function(label, expression, explanation) {
    shiny::tags$tr(shiny::tags$th(scope = "row", label),
      shiny::tags$td(shiny::HTML(paste0('<math xmlns="http://www.w3.org/1998/Math/MathML">', expression, "</math>"))),
      shiny::tags$td(explanation))
  }
  shiny::tagList(
    shiny::p("Let A and B be the compared correlation or covariance matrices, or a batch matrix and the pooled reference. Write \u0394 = A \u2212 B, with p compared features and indices from 1 to p."),
    shiny::div(style = "overflow-x: auto;", shiny::tags$table(class = "table table-sm",
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Metric"), shiny::tags$th("Formula"), shiny::tags$th("Interpretation"))),
      shiny::tags$tbody(
        metric_row("Off-diagonal RMS", paste0("<msqrt><mfrac><mrow>",
          sum_squares("<mi>i</mi><mo>&#x2260;</mo><mi>j</mi>"),
          "</mrow><mrow><mi>p</mi><mo>(</mo><mi>p</mi><mo>&#x2212;</mo><mn>1</mn><mo>)</mo></mrow></mfrac></msqrt>"),
          "Typical difference across off-diagonal feature relationships."),
        metric_row("Spectral norm", paste0(norm("2"),
          "<mo>=</mo><msub><mi>&#x3C3;</mi><mtext>max</mtext></msub><mo>(</mo>", delta, "<mo>)</mo><mo>=</mo>",
          "<munder><mo>max</mo><mi>k</mi></munder><mo>|</mo><msub><mi>&#x3BB;</mi><mi>k</mi></msub><mo>(</mo>", delta, "<mo>)</mo><mo>|</mo>"),
          "Largest absolute change along a unit-length combination of features; computed from absolute eigenvalues of the symmetric difference."),
        metric_row("Largest absolute difference", paste0("<munder><mo>max</mo><mrow><mi>i</mi><mo>,</mo><mi>j</mi></mrow></munder><mo>|</mo>", delta_ij, "<mo>|</mo>"),
          "Most discrepant single matrix entry, including diagonal variance differences."),
        metric_row("Frobenius norm", paste0(norm("F"), "<mo>=</mo><msqrt>",
          sum_squares("<mi>i</mi><mo>,</mo><mi>j</mi>"), "</msqrt>"),
          "Overall difference across all matrix entries.")
      ))),
    shiny::p("Only off-diagonal RMS excludes diagonal entries. Spectral norm uses the largest singular value (\u03c3); for symmetric differences, this is the largest absolute eigenvalue (\u03bb)."))
}
