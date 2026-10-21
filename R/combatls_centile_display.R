.combatls_centile_plot <- function(centiles, feature, x, group = NULL) {
  data <- centiles[centiles$probability == .5, , drop = FALSE]
  data$scale_model <- factor(data$model, levels = c("proposed", "screening"),
                            labels = c("Proposed", "Screening"))
  data$lower90 <- data$mu + stats::qnorm(.05) * data$sigma
  data$upper90 <- data$mu + stats::qnorm(.95) * data$sigma
  data$lower50 <- data$mu + stats::qnorm(.25) * data$sigma
  data$upper50 <- data$mu + stats::qnorm(.75) * data$sigma
  colors <- c(Proposed = "#3b6ea8", Screening = "#d17c32")
  plot <- ggplot(data, aes(x = .data[[x]],
    group = base::interaction(.data[["model"]], .data[["curve_group"]]))) +
    geom_ribbon(aes(ymin = .data[["lower90"]], ymax = .data[["upper90"]],
                    fill = .data[["scale_model"]]), alpha = .12) +
    geom_ribbon(aes(ymin = .data[["lower50"]], ymax = .data[["upper50"]],
                    fill = .data[["scale_model"]]), alpha = .26) +
    geom_line(aes(y = .data[["mu"]], color = .data[["scale_model"]]), linewidth = .8) +
    scale_fill_manual(values = colors, drop = TRUE) +
    scale_color_manual(values = colors, drop = TRUE) +
    labs(x = x, y = feature, fill = "Scale model", color = "Scale model",
      title = "Fitted median and centile bands",
      caption = "Darker: 25th-75th centiles; lighter: 5th-95th centiles. Bands are not confidence intervals.") +
    theme_minimal()
  if (!is.null(group)) plot <- plot + facet_wrap(~curve_group)
  plot
}

.combatls_add_observations <- function(plot, check, feature, x, group = NULL) {
  observations <- data.frame(horizontal = check$data[[x]], measurement = check$data[[feature]],
    curve_group = if (is.null(group)) "All" else as.character(check$data[[group]]))
  plot + geom_point(data = observations,
    aes(x = .data[["horizontal"]], y = .data[["measurement"]]),
    inherit.aes = FALSE, color = "grey45", alpha = .2, size = .8) +
    labs(caption = paste(plot$labels$caption,
      "Raw points retain their own covariates and batch effects; descriptive overlay only.", sep = "\n"))
}

.combatls_centile_variability <- function(check, feature, x, group = NULL, model = "both") {
  grid <- combatls_check_centiles(check, feature, x, group, model = model,
                                 probabilities = .5, x.quantiles = c(.25, .75))
  rows <- split(seq_len(nrow(grid)), list(grid$model, grid$curve_group), drop = TRUE)
  do.call(rbind, lapply(rows, function(indices) {
    lower <- grid[indices[1L], , drop = FALSE]
    upper <- grid[indices[2L], , drop = FALSE]
    data.frame(model = lower$model, curve_group = lower$curve_group,
      x_lower = lower[[x]], x_upper = upper[[x]],
      sd_lower = lower$sigma, sd_upper = upper$sigma,
      sd_change_pct = 100 * (upper$sigma / lower$sigma - 1), row.names = NULL)
  }))
}

.combatls_variability_display <- function(data, x, group = NULL) {
  display <- data.frame(`Scale model` = ifelse(data$model == "proposed", "Proposed", "Screening"),
    check.names = FALSE)
  if (!is.null(group)) display[[group]] <- data$curve_group
  display[[paste0(x, " (25th percentile)")]] <- .combatls_display_number(data$x_lower)
  display[[paste0(x, " (75th percentile)")]] <- .combatls_display_number(data$x_upper)
  display[["SD at lower value"]] <- .combatls_display_number(data$sd_lower)
  display[["SD at upper value"]] <- .combatls_display_number(data$sd_upper)
  display[["SD change (%)"]] <- ifelse(is.finite(data$sd_change_pct),
    sprintf("%+.3f%%", data$sd_change_pct), "\u2014")
  display
}

.combatls_centile_feature_order <- function(check, x) {
  tests <- check$screening[check$screening$covariate == x, , drop = FALSE]
  evidence <- tests$q.value[match(check$features, tests$feature)]
  check$features[order(evidence, seq_along(check$features), na.last = TRUE)]
}
