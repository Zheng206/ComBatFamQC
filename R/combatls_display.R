# Presentation only: keep statistical results and their numeric precision intact.
.combatls_display_number <- function(x) {
  ifelse(is.finite(x), sprintf("%.3f", x), "\u2014")
}

.combatls_display_p <- function(x) {
  ifelse(is.na(x), "NA", ifelse(x < .001, "<0.001", sprintf("%.3f", x)))
}

.combatls_display_sig <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***",
    ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

.combatls_screening_display <- function(check) {
  summary <- check$summary
  if (is.null(summary) || !nrow(summary))
    return(list(summary = data.frame(), details = data.frame()))

  ratios <- rep(NA_real_, nrow(summary))
  contrasts <- rep("See feature-wise effects or centile curves", nrow(summary))
  for (i in seq_len(nrow(summary))) {
    effects <- check$effect_summary
    if (!nrow(effects)) next
    effects <- effects[effects$covariate == summary$covariate[i], , drop = FALSE]
    if (nrow(effects) == 1L && effects$estimated_features > 0) {
      ratios[i] <- effects$sd_ratio_median
      contrasts[i] <- effects$contrast
    } else if (nrow(effects) > 1L) {
      contrasts[i] <- "Multiple contrasts; see feature-wise results"
    }
  }
  main <- data.frame(summary$covariate, summary$tested,
    paste(summary$significant, "/", summary$total_features),
    paste0(.combatls_display_number(summary$significant_pct_all), "%"),
    .combatls_display_number(ratios), contrasts, check.names = FALSE)
  names(main) <- c("Biological covariate", "Features tested", "Significant features",
                   "Percentage of features", "Median SD ratio", "SD ratio contrast")

  tests <- check$screening
  effects <- check$effects
  details <- merge(tests, effects, by = c("feature", "covariate"), all.x = TRUE,
                   sort = FALSE, suffixes = c(".test", ".effect"))
  details <- details[order(details$q.value, match(details$covariate, summary$covariate),
                           match(details$feature, check$features), details$contrast,
                           na.last = TRUE), , drop = FALSE]
  significant <- !is.na(details$q.value) & details$q.value < .05
  display <- data.frame(details$feature, details$covariate, details$contrast,
    .combatls_display_number(details$sd_ratio), .combatls_display_number(details$sd_change_pct),
    .combatls_display_p(details$p.value),
    .combatls_display_p(details$q.value), .combatls_display_sig(details$q.value),
    significant, ifelse(is.na(details$q.value), 2, details$q.value), check.names = FALSE)
  names(display) <- c("Feature", "Biological covariate", "SD ratio contrast", "SD ratio",
    "SD change (%)", "Unadjusted p-value", "BH-adjusted p-value", "sig", ".significant", ".fdr_sort")
  list(summary = main, details = display)
}


.combatls_filter_associations <- function(details, covariate = NULL, significant_only = FALSE) {
  if (!nrow(details)) return(details)
  if (length(covariate) && any(nzchar(covariate)))
    details <- details[details[["Biological covariate"]] %in% covariate, , drop = FALSE]
  if (isTRUE(significant_only))
    details <- details[details$.significant, , drop = FALSE]
  details
}

.combatls_associations_table <- function(details) {
  if (!ncol(details)) return(DT::datatable(details, rownames = FALSE))
  sort_column <- match(".fdr_sort", names(details)) - 1L
  p_column <- match("BH-adjusted p-value", names(details)) - 1L
  hidden <- match(c("Unadjusted p-value", ".significant", ".fdr_sort"), names(details)) - 1L
  table <- DT::datatable(details, rownames = FALSE,
    options = list(pageLength = 10, scrollX = TRUE,
      order = list(list(sort_column, "asc")),
      columnDefs = list(list(visible = FALSE, targets = hidden),
                        list(targets = p_column, orderData = sort_column))))
  table <- DT::formatStyle(table, ".significant", target = "row",
    backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("lightyellow", "white")))
  DT::formatStyle(table, "BH-adjusted p-value", valueColumns = ".significant",
    color = DT::styleEqual(c(TRUE, FALSE), c("red", "inherit")))
}
