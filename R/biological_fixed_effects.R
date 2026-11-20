# Biological coefficients are extracted from the fitted diagnostic location model.
# No fitting, standardization of responses, or cross-run comparisons occur here.
.biological_empty <- function() {
  data.frame(feature = character(), term = character(), coefficient = character(),
    contrast_or_unit = character(), estimate = double(), std_error = double(),
    conf_low = double(), conf_high = double(), estimate_status = character(),
    interval_status = character(), effect_type = character(), feature_unit = character(),
    model_type = character(), model_formula = character(), n_observations = integer(),
    covariate_coding = character(), categorical_references = character(),
    interval_method = character(), fit_status = character(), stringsAsFactors = FALSE)
}

.biological_column_labels <- function() {
  c("Feature", "Biological term", "Model coefficient", "Contrast or numeric unit",
    "Estimate", "SE", "95% CI lower", "95% CI upper", "Estimate status", "Interval status",
    "Effect type", "Feature unit", "Model", "Model formula", "Observations",
    "Covariate coding", "Categorical references", "Interval method", "Fit status")
}

.biological_units <- function(data, variables, supplied = NULL) {
  out <- setNames(vapply(variables, function(variable) {
    unit <- attr(data[[variable]], "units", exact = TRUE)
    if (is.null(unit)) unit <- attr(data[[variable]], "unit", exact = TRUE)
    if (is.character(unit) && length(unit) == 1L && !is.na(unit) && nzchar(unit)) unit else "Not recorded"
  }, character(1)), variables)
  if (!is.null(supplied)) {
    if (!is.character(supplied) || is.null(names(supplied)) || anyNA(supplied) ||
        any(!nzchar(supplied)) || any(!names(supplied) %in% variables) || anyDuplicated(names(supplied)))
      stop("Units must be a named character vector for the requested variables.", call. = FALSE)
    out[names(supplied)] <- supplied
  }
  out
}

.biological_variables <- function(term) {
  all.vars(stats::as.formula(paste("~", term)))
}

.biological_coding <- function(frame, variables, contrasts, units) {
  components <- list()
  descriptions <- references <- character()
  reference_values <- list()
  for (variable in intersect(variables, names(frame))) {
    value <- frame[[variable]]
    if (is.character(value)) value <- factor(value)
    if (is.factor(value)) {
      spec <- contrasts[[variable]]
      if (!is.null(spec)) stats::contrasts(value) <- spec
      coding <- stats::contrasts(value)
      zero <- which(rowSums(abs(coding)) == 0)
      treatment <- length(zero) == 1L && all(coding %in% c(0, 1)) && all(colSums(coding) == 1)
      reference <- if (treatment) rownames(coding)[zero] else "None (non-treatment coding)"
      reference_values[[variable]] <- reference
      references <- c(references, paste0(variable, " = ", reference))
      descriptions <- c(descriptions, paste0(variable, " levels: ", paste(levels(value), collapse = ", "),
        "; contrasts: ", paste(vapply(seq_len(ncol(coding)), function(j) {
          paste0(colnames(coding)[j], " [", paste(paste0(rownames(coding), "=",
            format(coding[, j], digits = 16, trim = TRUE)), collapse = ", "), "]")
        }, character(1)), collapse = "; ")))
      # Match actual single-variable design names, including non-default contrasts.
      contrast_arg <- if (is.null(spec)) NULL else setNames(list(spec), variable)
      single <- stats::model.matrix(stats::reformulate(paste0("`", variable, "`")),
        data = frame, contrasts.arg = contrast_arg)
      column_names <- colnames(single)[attr(single, "assign") == 1L]
      for (j in seq_along(column_names)) {
        components[[column_names[j]]] <- if (treatment) {
          paste0(rownames(coding)[which(coding[, j] == 1)], " vs ", reference, " (", variable, ")")
        } else {
          paste0(variable, " contrast ", colnames(coding)[j], " (", paste(paste0(rownames(coding), "=",
            format(coding[, j], digits = 8, trim = TRUE)), collapse = ", "), ")")
        }
      }
    } else if (is.numeric(value)) {
      unit <- unname(units[variable])
      if (is.null(unit) || is.na(unit)) unit <- "Not recorded"
      components[[variable]] <- paste0("Per 1 unit of ", variable, " (", unit, ")")
      descriptions <- c(descriptions, paste0(variable, ": numeric; unit = ", unit,
        "; range = ", paste(format(range(value, na.rm = TRUE), digits = 8, trim = TRUE), collapse = " to ")))
      reference_values[[variable]] <- "0"
    } else {
      descriptions <- c(descriptions, paste0(variable, ": ", paste(class(value), collapse = "/")))
    }
  }
  list(components = components, description = paste(descriptions, collapse = " | "),
    references = if (length(references)) paste(references, collapse = " | ") else "No categorical modeled covariates",
    reference_values = reference_values)
}

.biological_fixed_effects <- function(model, feature, batch, biological,
                                      feature_unit = "Not recorded", covariate_units = character()) {
  mixed <- inherits(model, "merMod")
  additive <- inherits(model, "gam")
  frame <- stats::model.frame(model)
  formula <- stats::formula(model)
  terms <- if (mixed) stats::terms(lme4::nobars(formula)) else if (additive) model$pterms else stats::terms(model)
  contrast_spec <- if (mixed) attr(lme4::getME(model, "X"), "contrasts") else model$contrasts
  design <- stats::model.matrix(terms, frame, contrasts.arg = contrast_spec)
  labels <- attr(terms, "term.labels")
  assignment <- attr(design, "assign")
  term_vars <- lapply(labels, .biological_variables)
  eligible <- vapply(term_vars, function(vars) length(vars) > 0 &&
    !any(vars %in% batch) && all(vars %in% biological), logical(1))
  columns <- which(assignment > 0L & assignment %in% which(eligible))
  coefficients <- if (mixed) lme4::fixef(model, add.dropped = TRUE) else stats::coef(model)
  covariance <- tryCatch(as.matrix(stats::vcov(model)), error = function(e) NULL)
  se <- if (is.null(covariance)) numeric() else sqrt(pmax(diag(covariance), 0))
  if (!is.null(covariance)) se[diag(covariance) < 0] <- NA_real_
  names(se) <- if (is.null(covariance)) character() else rownames(covariance)
  coding <- .biological_coding(frame, unique(c(biological, batch)), contrast_spec, covariate_units)
  fit_status <- "Fitted"
  converged <- TRUE
  if (mixed) {
    messages <- model@optinfo$conv$lme4$messages
    converged <- !length(messages) && all(model@optinfo$conv$opt == 0)
    if (!converged) fit_status <- "Convergence warning"
    if (lme4::isSingular(model)) fit_status <- paste(fit_status, "singular random-effect fit", sep = "; ")
  } else if (additive) {
    converged <- !identical(model$converged, FALSE)
    if (!converged) fit_status <- "Convergence warning"
  }
  method <- if (mixed) "Approximate normal Wald (population fixed effects)" else if (additive)
    "Approximate normal Wald (mgcv Bayesian covariance; smoothing parameters fixed)" else "Student t (residual df)"
  critical <- if (mixed || additive) stats::qnorm(.975) else if (stats::df.residual(model) > 0)
    stats::qt(.975, stats::df.residual(model)) else NA_real_
  # mgcv can encode a non-estimable parametric coefficient as zero. Retain its
  # row, but never interpret that computational zero as an estimated effect.
  aliased <- character()
  if (additive) {
    qr <- qr(design)
    if (qr$rank < ncol(design)) aliased <- colnames(design)[qr$pivot[seq.int(qr$rank + 1L, ncol(design))]]
  }
  make_row <- function(term, coefficient, contrast, effect_type, smooth = FALSE) {
    estimate <- unname(coefficients[coefficient])
    error <- unname(se[coefficient])
    if (!length(estimate)) estimate <- NA_real_
    if (!length(error)) error <- NA_real_
    available <- !smooth && is.finite(estimate) && !coefficient %in% aliased
    if (!available) { estimate <- NA_real_; error <- NA_real_ }
    valid_interval <- available && is.finite(error) && error >= 0 && is.finite(critical) && converged
    data.frame(feature = feature, term = term, coefficient = coefficient, contrast_or_unit = contrast,
      estimate = estimate, std_error = error,
      conf_low = if (valid_interval) estimate - critical * error else NA_real_,
      conf_high = if (valid_interval) estimate + critical * error else NA_real_,
      estimate_status = if (smooth) "Separate smooth-effect view required; no single slope" else if (available)
        "Available" else "Unavailable: aliased or non-estimable coefficient",
      interval_status = if (valid_interval) "Available" else if (smooth) "Not applicable to a single coefficient" else
        if (!converged) "Unavailable: fit convergence warning" else "Unavailable: estimate, SE, or residual degrees of freedom invalid",
      effect_type = effect_type, feature_unit = feature_unit,
      model_type = if (mixed) "lmer" else if (additive) "gam" else "lm",
      model_formula = paste(deparse(formula, width.cutoff = 500L), collapse = " "),
      n_observations = stats::nobs(model), covariate_coding = coding$description,
      categorical_references = coding$references, interval_method = if (smooth) "Not applicable" else method,
      fit_status = fit_status, stringsAsFactors = FALSE)
  }
  rows <- lapply(columns, function(j) {
    term_index <- assignment[j]
    term <- labels[term_index]
    variables <- term_vars[[term_index]]
    column <- colnames(design)[j]
    split_column <- function(column) {
      keys <- names(coding$components)
      if (column %in% keys) return(column)
      for (key in keys[order(nchar(keys), decreasing = TRUE)]) {
        if (startsWith(column, paste0(key, ":")))
          return(c(key, split_column(substring(column, nchar(key) + 2L))))
      }
      strsplit(column, ":", fixed = TRUE)[[1L]]
    }
    parts <- split_column(column)
    contrast <- paste(vapply(parts, function(part) {
      description <- coding$components[[part]]
      if (is.null(description)) paste0("Per 1 unit of model column ", part) else description
    }, character(1)), collapse = " x ")
    interaction <- length(variables) > 1L
    conditional <- unique(unlist(term_vars[vapply(term_vars, function(x)
      length(x) > length(variables) && all(variables %in% x), logical(1))]))
    conditional <- setdiff(conditional, variables)
    if (length(conditional)) contrast <- paste0(contrast, "; conditional at ",
      paste(vapply(conditional, function(variable) paste0(variable, " = ",
        coding$reference_values[[variable]]), character(1)), collapse = ", "))
    if (interaction) contrast <- paste0("Conditional interaction: ", contrast)
    make_row(term, column, contrast, if (interaction) "Conditional interaction" else
      if (length(conditional)) "Conditional main effect" else "Biological fixed effect")
  })
  if (additive) for (smooth in model$smooth) {
    variables <- unique(c(smooth$term, if (!identical(smooth$by, "NA")) smooth$by))
    if (inherits(smooth, "random.effect") || any(variables %in% batch) || !all(variables %in% biological)) next
    rows[[length(rows) + 1L]] <- make_row(smooth$label, smooth$label,
      "Smooth effect; requires a separate fitted-effect curve, not a slope",
      "Smooth effect (separate view required)", smooth = TRUE)
  }
  if (!length(rows)) .biological_empty() else do.call(rbind, rows)
}

.biological_report_table <- function(result) {
  if (!is.null(result$biological_fixed_effects)) return(result$biological_fixed_effects)
  # Old saved results contain no fitted coefficients. Do not refit during export.
  rows <- .biological_empty()
  features <- result$info$features
  if (!length(features)) return(rows)
  rows[seq_along(features), ] <- NA
  rows$feature <- features
  rows$term <- "Not stored"
  rows$estimate_status <- "Unavailable: rerun visual_prep() on the intended dataset"
  rows$interval_status <- "Unavailable: fitted coefficients were not stored"
  rows
}

.biological_group_labels <- function(table) {
  paste(table$term, table$contrast_or_unit, sep = ": ")
}

.biological_effect_labels <- function(table) {
  vapply(seq_len(nrow(table)), function(i) {
    term <- gsub(":", " \u00d7 ", table$term[i], fixed = TRUE)
    contrast <- table$contrast_or_unit[i]
    if (is.na(contrast)) contrast <- ""
    contrast <- sub("^Conditional interaction: ", "", contrast)
    contrast <- sub("; conditional at .*", "", contrast)
    parts <- strsplit(contrast, " x ", fixed = TRUE)[[1L]]
    contrasts <- vapply(parts, function(part) {
      if (grepl(" vs ", part, fixed = TRUE)) return(sub(" \\([^()]*\\)$", "", part))
      if (grepl(" contrast ", part, fixed = TRUE))
        return(sub("^.*? contrast ([^ ]+).*$", "contrast \\1", part, perl = TRUE))
      ""
    }, character(1))
    contrasts <- contrasts[nzchar(contrasts)]
    if (length(contrasts)) paste0(term, ": ", paste(contrasts, collapse = "; ")) else term
  }, character(1))
}

.biological_excel_table <- function(table) {
  # Keep fitted values and R metadata intact; shorten only Excel display text.
  out <- table
  out$term <- .biological_effect_labels(table)
  out$contrast_or_unit <- sub("^Conditional interaction: ", "", out$contrast_or_unit)
  out$contrast_or_unit <- sub("; conditional at ", "; at ", out$contrast_or_unit, fixed = TRUE)
  out$contrast_or_unit <- gsub("[[:space:]]*\\(Not recorded\\)", "", out$contrast_or_unit, ignore.case = TRUE)
  unknown <- !is.na(out$feature_unit) & tolower(trimws(out$feature_unit)) == "not recorded"
  out$feature_unit[unknown] <- ""
  out$covariate_coding <- gsub("; unit = Not recorded", "", out$covariate_coding, fixed = TRUE)
  names(out) <- .biological_column_labels()
  out
}

.biological_report_widget <- function(table) {
  group <- match(.biological_group_labels(table), unique(.biological_group_labels(table)))
  valid_ci <- is.finite(table$conf_low) & is.finite(table$conf_high)
  data <- data.frame(feature = table$feature, estimate = table$estimate,
    std_error = table$std_error,
    confidence_interval = ifelse(valid_ci, sprintf("%.4g to %.4g", table$conf_low, table$conf_high), "Unavailable"),
    stringsAsFactors = FALSE)
  labels <- c("Feature", "Estimate", "SE", "95% CI")
  numeric_columns <- c(1L, 2L)
  data$selection_group <- group
  group_column <- ncol(data) - 1L
  # Apply the group selected above the plot, including on initialization.
  init <- DT::JS(paste0("function(){var api=this.api();var update=function(){",
    "var select=document.getElementById('biological-term-select');",
    "if(select){api.column(", group_column, ").search('^'+select.value+'$',true,false).draw();}",
    "};document.addEventListener('combatqc-biological-term-change',update);update();}"))
  definitions <- list(list(targets = group_column, visible = FALSE, searchable = TRUE),
    list(targets = "_all", defaultContent = "Unavailable"))
  if (length(numeric_columns)) definitions <- c(definitions, list(list(targets = numeric_columns,
    render = DT::JS("function(data,type){if(type==='display'){return data===null||data===''?'Unavailable':Number(data).toPrecision(5);}return data;}"))))
  DT::datatable(data, rownames = FALSE, colnames = c(labels, "Selection group"),
    options = list(pageLength = 10, scrollX = TRUE, dom = "ftp", order = list(),
      initComplete = init, columnDefs = definitions))
}

# Self-contained HTML/SVG: selection works in an exported HTML file without
# Shiny, a network connection, or an optional plotting package.
.biological_coefficient_view <- function(table) {
  if (!nrow(table)) return(shiny::p("No biological fixed-effect terms were fitted."))
  eligible <- !is.na(table$effect_type)
  data <- table[eligible, , drop = FALSE]
  if (!nrow(data)) return(shiny::p("Biological coefficient estimates were not stored in this saved diagnosis. Rerun visual_prep() to include them."))
  groups <- split(seq_len(nrow(data)), factor(.biological_group_labels(data),
    levels = unique(.biological_group_labels(table))))
  groups <- groups[lengths(groups) > 0L]
  keys <- unique(.biological_group_labels(table))
  display_labels <- .biological_effect_labels(table[match(keys, .biological_group_labels(table)), , drop = FALSE])
  display_labels <- setNames(make.unique(display_labels, sep = " "), keys)
  tags <- shiny::tags
  panels <- lapply(seq_along(groups), function(index) {
    rows <- data[groups[[index]], , drop = FALSE]
    if (all(rows$effect_type == "Smooth effect (separate view required)")) {
      return(tags$div(`data-biological-panel` = match(names(groups)[index], unique(.biological_group_labels(table))),
        style = if (index == 1L) "display: block;" else "display: none;",
        tags$p(tags$strong(display_labels[[names(groups)[index]]])),
        tags$p("This smooth requires a separate fitted-effect curve. A single coefficient and interval are unavailable; smooth basis coefficients are not shown as slopes.")))
    }
    values <- c(0, rows$estimate, rows$conf_low, rows$conf_high)
    limits <- range(values[is.finite(values)])
    padding <- max(diff(limits) * .08, if (all(limits == 0)) 1 else max(abs(limits)) * .02)
    limits <- limits + c(-padding, padding)
    x <- function(value) 320 + (value - limits[1L]) / diff(limits) * 650
    height <- 70 + 28 * nrow(rows)
    ticks <- pretty(limits, n = 5)
    ticks <- ticks[ticks >= limits[1L] & ticks <= limits[2L]]
    svg <- tags$svg(xmlns = "http://www.w3.org/2000/svg", viewBox = paste(0, 0, 1020, height),
      width = "100%", style = "min-width: 650px; display: block;", role = "img",
      `aria-label` = paste("Biological fixed effects:", display_labels[[names(groups)[index]]]),
      tags$line(x1 = x(0), x2 = x(0), y1 = 10, y2 = height - 50,
        stroke = "#666666", `stroke-dasharray` = "5 4"),
      lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        y <- 22 + (i - 1) * 28
        label <- if (nchar(row$feature) > 38) paste0(substr(row$feature, 1, 35), "...") else row$feature
        unit <- if (!is.na(row$feature_unit) && row$feature_unit != "Not recorded") row$feature_unit else NULL
        tags$g(tags$title(paste(c(row$feature, unit, row$estimate_status), collapse = "; ")),
          tags$text(x = 308, y = y + 4, `text-anchor` = "end", `font-size` = 13, label),
          if (is.finite(row$conf_low) && is.finite(row$conf_high))
            tags$line(x1 = x(row$conf_low), x2 = x(row$conf_high), y1 = y, y2 = y, stroke = "#234E70", `stroke-width` = 2),
          if (is.finite(row$estimate)) tags$circle(cx = x(row$estimate), cy = y, r = 4, fill = "#234E70",
            tags$title(paste0("Estimate: ", signif(row$estimate, 6), "; 95% CI: ",
              if (is.finite(row$conf_low) && is.finite(row$conf_high)) paste(signif(c(row$conf_low, row$conf_high), 6), collapse = " to ") else "Unavailable"))) else
                tags$text(x = 330, y = y + 4, `font-size` = 12, fill = "#666666", "Estimate unavailable"))
      }),
      lapply(ticks, function(value) tags$text(x = x(value), y = height - 30,
        `text-anchor` = "middle", `font-size` = 12, format(signif(value, 4), trim = TRUE))),
      tags$text(x = 645, y = height - 7, `text-anchor` = "middle", `font-size` = 13, "Estimate"))
    tags$div(`data-biological-panel` = match(names(groups)[index], unique(.biological_group_labels(table))),
      style = if (index == 1L) "display: block;" else "display: none;",
      tags$p(tags$strong(display_labels[[names(groups)[index]]])),
      tags$div(style = "max-height: 460px; overflow: auto; border: 1px solid #e3e8ed; padding: 0.5rem;", svg))
  })
  shiny::tagList(
    tags$label(`for` = "biological-term-select", "Fixed effect"),
    tags$select(id = "biological-term-select", class = "form-select",
      style = "max-width: 100%; margin-bottom: 0.75rem;",
      onchange = "var panels=document.getElementById('biological-coefficient-panels').children; for(var i=0;i<panels.length;i++){panels[i].style.display=(panels[i].getAttribute('data-biological-panel')===this.value)?'block':'none';}document.dispatchEvent(new Event('combatqc-biological-term-change'));",
      lapply(seq_along(groups), function(i) tags$option(
        value = match(names(groups)[i], unique(.biological_group_labels(table))), display_labels[[names(groups)[i]]]))),
    tags$div(id = "biological-coefficient-panels", panels))
}
