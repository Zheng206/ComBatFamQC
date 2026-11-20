# Shiny-only displays: leave the exported plotting and prediction interfaces intact.
.combatls_plot_text_theme <- function() {
  theme(text = element_text(face = "bold"),
    axis.text = element_text(face = "bold"), axis.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "bold"),
    plot.caption = element_text(face = "bold"), strip.text = element_text(face = "bold"))
}

.combatls_profile_summary <- function(check, x, group = NULL) {
  profile <- vapply(setdiff(check$covariates, x), function(variable) {
    values <- check$model_data[[variable]]
    value <- if (identical(variable, group)) {
      paste0(paste(levels(values), collapse = " / "), " (separate curves)")
    } else if (is.numeric(values)) {
      format(signif(stats::median(values), 6), trim = TRUE)
    } else names(which.max(table(values)))
    paste(variable, "=", value)
  }, character(1))
  target <- if (is.null(check$ref.batch)) "pooled" else as.character(check$ref.batch)
  paste(c(paste(x, "varies"), profile, paste("Batch target =", target)), collapse = " \u00b7 ")
}

.combatls_observation_bins <- function(check, feature, x, group = NULL) {
  if (length(feature) != 1L || !feature %in% check$features ||
      length(x) != 1L || !x %in% check$covariates || !is.numeric(check$data[[x]]))
    stop("Select a checked feature and a numeric horizontal covariate.", call. = FALSE)
  if (!is.null(group) && (length(group) != 1L || !group %in% check$covariates ||
                         is.numeric(check$data[[group]])))
    stop("Select a categorical grouping covariate.", call. = FALSE)
  observations <- data.frame(row = seq_len(nrow(check$data)), horizontal = check$data[[x]],
    observed = check$data[[feature]],
    curve_group = if (is.null(group)) "All" else as.character(check$data[[group]]))
  observations <- observations[is.finite(observations$horizontal) &
    is.finite(observations$observed) & !is.na(observations$curve_group), , drop = FALSE]
  if (!nrow(observations)) stop("No finite observations for this selection.", call. = FALSE)
  groups <- if (is.null(group)) "All" else levels(droplevels(factor(check$data[[group]])))
  limits <- range(observations$horizontal)
  if (diff(limits) == 0) {
    limits <- limits + c(-1, 1) * max(.5, abs(limits[1L]) * .01)
    breaks <- limits
  } else {
    # Equal-width, readable bins, shared across groups. Retain empty bins so
    # sparse regions remain visible and never connect a trend across a gap.
    count <- max(1L, min(10L, grDevices::nclass.FD(observations$horizontal)))
    interior <- pretty(limits, n = count)
    breaks <- c(limits[1L], interior[interior > limits[1L] & interior < limits[2L]], limits[2L])
  }
  observations$bin <- as.integer(cut(observations$horizontal, breaks,
    include.lowest = TRUE, right = TRUE))
  observations$curve_group <- factor(observations$curve_group, levels = groups)
  bins <- expand.grid(bin = seq_len(length(breaks) - 1L), curve_group = groups,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  bins$curve_group <- factor(bins$curve_group, levels = groups)
  bins$lower <- breaks[bins$bin]
  bins$upper <- breaks[bins$bin + 1L]
  bins$horizontal <- (bins$lower + bins$upper) / 2
  bins$n <- vapply(seq_len(nrow(bins)), function(i) sum(observations$bin == bins$bin[i] &
    observations$curve_group == bins$curve_group[i]), integer(1))
  list(observations = observations, bins = bins, limits = limits, x = x, group = group)
}

.combatls_count_strip <- function(binned) {
  counts <- binned$bins
  # Centile facets use character labels, hence alphabetical panel order.
  counts$curve_group <- as.character(counts$curve_group)
  maximum <- max(1, counts$n)
  counts$text_color <- ifelse(counts$n / maximum > .55, "white", "#16324F")
  plot <- ggplot(counts, aes(x = .data[["horizontal"]])) +
    geom_rect(aes(xmin = .data[["lower"]], xmax = .data[["upper"]], ymin = 0, ymax = 1,
      fill = .data[["n"]]), color = "white", linewidth = .5) +
    geom_text(aes(y = .5, label = .data[["n"]], color = .data[["text_color"]]), size = 3, fontface = "bold") +
    scale_fill_gradient(low = "#EFF6FF", high = "#1E5AA8", limits = c(0, maximum), guide = "none") +
    scale_color_identity() +
    scale_x_continuous(limits = binned$limits) +
    scale_y_continuous(breaks = NULL, expand = expansion(mult = 0)) +
    labs(title = paste("Observed sample size by", binned$x), x = binned$x, y = NULL) +
    theme_minimal() + .combatls_plot_text_theme() +
    theme(panel.grid = element_blank(), plot.title = element_text(size = 10, face = "bold"))
  if (!is.null(binned$group)) plot <- plot + facet_wrap(~curve_group) +
    theme(strip.text = element_text(size = 8, face = "bold"), strip.background = element_blank())
  plot
}

# Align panel columns (including facets) using only ggplot2/grid. No new dependency
# and no changes to the ggplot objects returned by the exported R interface.
.combatls_centiles_with_counts <- function(plot, binned) {
  upper <- ggplotGrob(plot + scale_x_continuous(limits = binned$limits) +
    labs(x = NULL, caption = NULL) + .combatls_plot_text_theme() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))
  lower <- ggplotGrob(.combatls_count_strip(binned))
  widths <- grid::unit.pmax(upper$widths, lower$widths)
  upper$widths <- lower$widths <- widths
  upper$name <- "centile-curves"
  lower$name <- "observation-counts"
  upper$vp <- grid::viewport(layout.pos.row = 1)
  lower$vp <- grid::viewport(layout.pos.row = 2)
  panel_rows <- length(unique(lower$layout$t[grepl("^panel", lower$layout$name)]))
  grid::grobTree(upper, lower, vp = grid::viewport(layout = grid::grid.layout(2, 1,
    heights = grid::unit.c(grid::unit(1, "null"), grid::unit(.85 + .4 * panel_rows, "inches")))))
}

.combatls_fit_view_data <- function(check, feature, binned, min_n = 10L, trend_min_n = 30L) {
  models <- c("proposed", "screening")
  points <- summaries <- status <- list()
  for (model in models) {
    fit <- if (model == "proposed") check$fits[[feature]] else check$screen_fits[[feature]]
    usable <- .combatls_fit_ok(fit)
    parameters <- check$parameters
    rows <- if (usable && nrow(parameters)) parameters[
      parameters$feature == feature & parameters$model == model, , drop = FALSE] else data.frame()
    matched <- if (nrow(rows)) match(rows$row, binned$observations$row) else integer()
    keep <- if (nrow(rows)) !is.na(matched) & is.finite(rows$residual) else logical()
    point <- binned$observations[matched[keep], , drop = FALSE]
    point$residual <- if (nrow(rows)) rows$residual[keep] else numeric()
    point$model <- rep(model, nrow(point))
    points[[model]] <- point
    bins <- binned$bins
    bins$model <- model
    bins$n <- integer(nrow(bins))
    bins$rms <- rep(NA_real_, nrow(bins))
    for (i in seq_len(nrow(bins))) {
      values <- point$residual[point$bin == bins$bin[i] & point$curve_group == bins$curve_group[i]]
      bins$n[i] <- length(values)
      if (length(values) >= min_n) bins$rms[i] <- sqrt(mean(values^2))
    }
    summaries[[model]] <- bins
    status[[model]] <- data.frame(model = model, n = nrow(point),
      message = if (!usable) "Unavailable: fit failed, did not converge, or was not identifiable." else
        if (!nrow(point)) "Unavailable: no finite saved residuals." else "", stringsAsFactors = FALSE)
  }
  points <- do.call(rbind, points)
  bins <- do.call(rbind, summaries)
  status <- do.call(rbind, status)
  label_model <- function(model) factor(model, levels = models, labels = c("Proposed", "Screening"))
  points$scale_model <- label_model(points$model)
  bins$scale_model <- label_model(bins$model)
  status$scale_model <- label_model(status$model)
  # Only adjacent, adequately populated bins can contribute a line segment.
  segments <- bins
  segments$xend <- segments$yend <- NA_real_
  segments$nend <- 0L
  for (i in seq_len(nrow(segments))) {
    next_bin <- which(bins$model == bins$model[i] & bins$curve_group == bins$curve_group[i] &
      bins$bin == bins$bin[i] + 1L)
    if (length(next_bin)) {
      segments$xend[i] <- bins$horizontal[next_bin]
      segments$yend[i] <- bins$rms[next_bin]
      segments$nend[i] <- bins$n[next_bin]
    }
  }
  segments <- segments[is.finite(segments$rms) & is.finite(segments$yend) &
    segments$n >= trend_min_n & segments$nend >= trend_min_n, , drop = FALSE]
  bins$connected <- vapply(seq_len(nrow(bins)), function(i) any(
    segments$model == bins$model[i] & segments$curve_group == bins$curve_group[i] &
      (segments$bin == bins$bin[i] | segments$bin + 1L == bins$bin[i])), logical(1))
  list(points = points, bins = bins, segments = segments, status = status,
    limits = binned$limits, x = binned$x, group = binned$group,
    min_n = min_n, trend_min_n = trend_min_n)
}

.combatls_fit_view_plot <- function(data, view = c("residuals", "qq")) {
  view <- match.arg(view)
  panels <- unique(data$bins[c("curve_group", "scale_model")])
  panels$horizontal <- mean(data$limits)
  panels$message <- data$status$message[match(panels$scale_model, data$status$scale_model)]
  panels$message[nzchar(panels$message)] <- "Residuals unavailable.\nSee fitting status."
  points <- data$points
  if (view == "qq") {
    panels$n <- vapply(seq_len(nrow(panels)), function(i) sum(points$curve_group == panels$curve_group[i] &
      points$scale_model == panels$scale_model[i]), integer(1))
    panels$message[panels$n < 2 & !nzchar(panels$message)] <- "Too few residuals\nfor a Q-Q comparison."
    # stat_qq_line needs two observations; points remain visible for singleton groups.
    sizes <- if (nrow(points)) stats::ave(rep(1L, nrow(points)),
      interaction(points$curve_group, points$scale_model, drop = TRUE), FUN = sum) else integer()
    line_points <- points[sizes >= 2L, , drop = FALSE]
    plot <- ggplot(points, aes(sample = .data[["residual"]])) + stat_qq(alpha = .4) +
      stat_qq_line(data = line_points, color = "#3b6ea8") +
      geom_text(data = panels[nzchar(panels$message), ], aes(x = 0, y = 0, label = .data[["message"]]),
        inherit.aes = FALSE, size = 3, lineheight = .9, fontface = "bold") +
      labs(x = "Normal theoretical quantile", y = "Normalized residual", title = "Residual Q-Q plots") +
      theme_minimal() + .combatls_plot_text_theme()
    if (is.null(data$group)) return(plot + facet_grid(~scale_model))
    return(plot + facet_grid(curve_group ~ scale_model))
  }
  diagnostics <- c("Normalized residual", "Binned residual RMS")
  points$diagnostic <- factor(rep(diagnostics[1L], nrow(points)), levels = diagnostics)
  bins <- data$bins
  bins$connection <- factor(ifelse(bins$connected, "Connected bins", "Unconnected bins"),
    levels = c("Connected bins", "Unconnected bins"))
  bins$diagnostic <- factor(rep(diagnostics[2L], nrow(bins)), levels = diagnostics)
  segments <- data$segments
  segments$diagnostic <- factor(rep(diagnostics[2L], nrow(segments)), levels = diagnostics)
  reference <- rbind(transform(panels, diagnostic = "Normalized residual", reference = 0),
    transform(panels, diagnostic = "Binned residual RMS", reference = 1))
  reference$diagnostic <- factor(reference$diagnostic,
    levels = c("Normalized residual", "Binned residual RMS"))
  reference$reference_label <- ifelse(reference$reference == 0, "Residual reference: 0", "RMS reference: 1")
  unavailable <- reference[nzchar(reference$message), ]
  plot <- ggplot() +
    geom_hline(data = reference, aes(yintercept = .data[["reference"]],
      linetype = .data[["reference_label"]]), color = "grey45") +
    geom_point(data = points, aes(x = .data[["horizontal"]], y = .data[["residual"]]), alpha = .3, size = 1) +
    geom_blank(data = bins, aes(x = .data[["horizontal"]], y = 0)) +
    geom_segment(data = segments, aes(x = .data[["horizontal"]], y = .data[["rms"]],
      xend = .data[["xend"]], yend = .data[["yend"]], color = "Connected bins")) +
    geom_point(data = bins[is.finite(bins$rms), ],
      aes(x = .data[["horizontal"]], y = .data[["rms"]], color = .data[["connection"]]), size = 2) +
    geom_text(data = bins, aes(x = .data[["horizontal"]], y = -Inf,
      label = paste0("n=", .data[["n"]])), vjust = -.4, size = 2.7, fontface = "bold") +
    geom_text(data = unavailable, aes(x = .data[["horizontal"]], y = .5, label = .data[["message"]]),
      size = 3, lineheight = .9, fontface = "bold") +
    scale_x_continuous(limits = data$limits) +
    scale_y_continuous(expand = expansion(mult = c(.15, .1))) +
    scale_color_manual(name = "Binned RMS", values = c("Connected bins" = "#3b6ea8", "Unconnected bins" = "#A8AAAE"),
      limits = c("Connected bins", "Unconnected bins"), drop = FALSE,
      labels = c(paste0("Connected bins (n >= ", data$trend_min_n, ")"),
        paste0("Unconnected bins (n >= ", data$min_n, ")"))) +
    scale_linetype_manual(name = NULL, values = c("Residual reference: 0" = 2, "RMS reference: 1" = 2)) +
    guides(color = guide_legend(override.aes = list(linetype = c(1, 0), shape = 16)),
      linetype = guide_legend(override.aes = list(color = "grey45"))) +
    labs(x = data$x, y = NULL, title = "Fit on observed covariates and batches") +
    theme_minimal() + .combatls_plot_text_theme() +
    theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "bold"),
      legend.position = "bottom")
  # Row-specific labels sit outside the y ticks, in the axis-title position.
  if (is.null(data$group)) plot + facet_grid(diagnostic ~ scale_model, scales = "free_y", switch = "y")
  else plot + facet_grid(diagnostic + curve_group ~ scale_model, scales = "free_y", switch = "y")
}
