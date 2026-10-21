#' Exploratory ComBatLS Scale Model Check
#'
#' Fit normal GAMLSS models before harmonization. Proposed models use
#' `sigma.formula`; separate screening models use `screen.formula` (by default,
#' all candidate covariates linearly). Batch effects enter both location and
#' scale. No harmonized observations are produced and no formula is changed.
#'
#' @param result Optional output of `visual_prep()`.
#' @param features,batch,covariates,df Dataset arguments as in `combat_harm()`.
#' @param formula Required location formula with response `y`.
#' @param sigma.formula Proposed biological scale formula, default `~ 1`.
#' @param candidates Covariate names to screen jointly, conditional on the other
#'   terms. Defaults to all supplied covariates. Use `character()` for fit checks
#'   without screening.
#' @param screen.formula Optional exploratory scale formula. Must contain every
#'   candidate; e.g. `~ pb(AGE) + SEX`. It is independent of `sigma.formula`.
#'   Offsets are not supported in the screening formula.
#' @param ref.batch Reference batch for target-distribution predictions. If NULL,
#'   batch effects are averaged on the link scale using observed batch proportions.
#' @param control GAMLSS fitting controls.
#' @return A `combatls_check` list containing proposed and exploratory fits,
#'   reduced fits, screening tests, per-covariate summaries, convergence/warning
#'   diagnostics, observed fitted distribution parameters and normalized residuals,
#'   effect sizes, and an optional liberal screening draft for manual review.
#' @details
#' Tests remove every scale term containing a candidate, including its interaction
#' and smooth contributions, while leaving the location model unchanged. The
#' likelihood-ratio statistic uses global deviance and fitted degrees of freedom,
#' following `gamlss::LR.test`. Comparisons involving penalized smooths are
#' approximate; failed, nonconverged, rank-deficient, or invalid comparisons have
#' missing p-values. BH adjustment is across features separately for each candidate,
#' counting failed comparisons in the correction family. The exploratory models
#' assume independent observations; subject-level random effects are not included.
#' Screening is evidence for reviewing prespecified biological effects, not an
#' automatic selection rule. The advisory draft includes candidates with at least
#' one feature with q < 0.05, retaining interaction marginal terms; this liberal
#' rule does not identify an optimal model. Percentages report significant features
#' relative to all features and to valid tests separately.
#' `effects` reports screening-model SD ratios for additive linear terms per one
#' original unit, and categorical levels versus the first factor level (irrespective
#' of contrast coding). Smooth, transformed, and interaction effects have no single
#' constant ratio and are marked unavailable. `effect_summary` reports median and
#' quartiles across all estimable features, including nonsignificant features.
#' Ratios are conditional associations, not causal effects or harmonization effects.
#' In-sample residuals assess fit, not validation.
#' Within each feature, identical proposed, screening, and reduced scale models
#' reuse one fit, preserving separate diagnostics for each comparison.
#' @references
#' Gardner et al. (2025), doi:10.1002/hbm.70197.
#' GAMLSS reference manual, `LR.test` and `predict.gamlss`.
#' @export
combatls_model_check <- function(result = NULL, features = NULL, batch = NULL,
                                 covariates = NULL, df = NULL, formula,
                                 sigma.formula = ~ 1, candidates = NULL,
                                 screen.formula = NULL, ref.batch = NULL,
                                 control = gamlss::gamlss.control(trace = FALSE, n.cyc = 100)) {
  .validate_combatls_formula(formula, TRUE)
  .validate_combatls_formula(sigma.formula, FALSE)
  .require_combatfamily()
  info <- data_prep(result = result, features = features, batch = batch,
                    covariates = covariates, df = df)
  df <- info$df
  features <- info$features
  covariates <- info$cov_shiny
  batch <- info$batch
  if (!length(features)) stop("No varying features remain for the model check.", call. = FALSE)
  if (any(c("y", ".combatls_batch") %in% c(covariates, batch)))
    stop("y and .combatls_batch are reserved model-check column names.", call. = FALSE)
  if (batch %in% covariates) stop("Batch must not be included among biological covariates.", call. = FALSE)
  if (is.null(candidates)) candidates <- covariates
  candidates <- unique(candidates)
  if (any(!candidates %in% covariates)) stop("Candidates must be listed in covariates.", call. = FALSE)
  if (!is.null(ref.batch) && !ref.batch %in% levels(df[[batch]]))
    stop("Reference batch must be in the prepared batch levels.", call. = FALSE)
  location <- .combatls_formula(formula, df[covariates], TRUE)
  proposed <- .combatls_formula(sigma.formula, df[covariates], FALSE)
  if (is.null(screen.formula)) {
    screen.formula <- if (length(candidates)) .combatls_terms_formula(vapply(candidates, function(name) paste(deparse(as.name(name)), collapse = ""), character(1)), environment(location)) else ~ 1
  }
  exploratory <- .combatls_formula(screen.formula, df[covariates], FALSE)
  if (any(!candidates %in% all.vars(exploratory)))
    stop("The exploratory screen.formula must explicitly include every candidate scale covariate.", call. = FALSE)
  if (length(attr(stats::terms(exploratory), "offset")))
    stop("screen.formula does not support offsets; use explicit candidate scale terms.", call. = FALSE)
  terms <- attr(stats::terms(exploratory), "term.labels")
  term_vars <- lapply(terms, function(term) all.vars(stats::as.formula(paste("~", term))))
  if (!length(terms) && length(candidates)) stop("Candidate scale terms are required for screening.", call. = FALSE)
  mu <- stats::update(location, ~ . + .combatls_batch)
  sigma <- stats::update(proposed, ~ . + .combatls_batch)
  screen_sigma <- stats::update(exploratory, ~ . + .combatls_batch)
  scale_formulas <- c(list(sigma, screen_sigma), lapply(candidates, function(candidate) {
    keep <- !vapply(term_vars, function(vars) candidate %in% vars, logical(1))
    reduced <- .combatls_terms_formula(terms[keep], environment(exploratory),
                                      attr(stats::terms(exploratory), "intercept"))
    stats::update(reduced, ~ . + .combatls_batch)
  }))
  unique_formulas <- list()
  fit_index <- integer(length(scale_formulas))
  for (i in seq_along(scale_formulas)) {
    match <- which(vapply(unique_formulas, .combatls_same_formula, logical(1),
                          right = scale_formulas[[i]]))
    if (!length(match)) {
      unique_formulas[[length(unique_formulas) + 1L]] <- scale_formulas[[i]]
      match <- length(unique_formulas)
    }
    fit_index[i] <- match[1L]
  }
  model_data <- droplevels(df[covariates])
  model_data$.combatls_batch <- droplevels(df[[batch]])
  fits <- screen_fits <- reduced_fits <- setNames(vector("list", length(features)), features)
  diagnostics <- parameters <- tests <- list()
  add_diagnostic <- function(run, feature, model) {
    diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
      feature = feature, model = model, converged = !is.null(run$fit) && isTRUE(run$fit$converged),
      valid = .combatls_fit_ok(run$fit),
      iterations = if (is.null(run$fit)) NA_real_ else run$fit$iter,
      deviance = if (is.null(run$fit)) NA_real_ else run$fit$G.deviance,
      df = if (is.null(run$fit)) NA_real_ else run$fit$df.fit,
      message = paste(c(run$error, run$warnings), collapse = "; "), stringsAsFactors = FALSE)
  }
  approximate <- any(c("pb", "ps", "cs", "scs", "lo", "fp", "random", "re") %in%
                       c(all.names(location), all.names(exploratory)))
  for (feature in features) {
    dat <- model_data
    dat$y <- df[[feature]]
    if (!is.numeric(dat$y) || any(!is.finite(dat$y)))
      stop("Model-check features must be finite numeric measurements: ", feature, call. = FALSE)
    runs <- lapply(unique_formulas, function(scale) .combatls_exploratory_fit(mu, scale, dat, control))
    proposed_run <- runs[[fit_index[1L]]]
    full_run <- runs[[fit_index[2L]]]
    fits[feature] <- list(proposed_run$fit)
    screen_fits[feature] <- list(full_run$fit)
    add_diagnostic(proposed_run, feature, "proposed")
    add_diagnostic(full_run, feature, "screening")
    for (model in c("proposed", "screening")) {
      fit <- if (model == "proposed") proposed_run$fit else full_run$fit
      if (is.null(fit)) next
      parameters[[length(parameters) + 1L]] <- data.frame(
        feature = feature, model = model, row = seq_len(nrow(dat)), observed = dat$y,
        mu = as.numeric(fit$mu.fv), sigma = as.numeric(fit$sigma.fv),
        residual = as.numeric(fit$residuals), batch = as.character(dat$.combatls_batch))
    }
    reduced_fits[[feature]] <- list()
    for (i in seq_along(candidates)) {
      candidate <- candidates[i]
      reduced_run <- runs[[fit_index[i + 2L]]]
      reduced_fits[[feature]][candidate] <- list(reduced_run$fit)
      add_diagnostic(reduced_run, feature, paste0("without ", candidate))
      comparison <- .combatls_scale_lrt(reduced_run$fit, full_run$fit)
      tests[[length(tests) + 1L]] <- data.frame(
        feature = feature, covariate = candidate, statistic = comparison$statistic,
        df = comparison$df, p.value = comparison$p.value, status = comparison$status,
        method = if (approximate) "Approximate LRT (penalized smooths; effective df)" else "Likelihood-ratio test",
        stringsAsFactors = FALSE)
    }
  }
  screening <- if (length(tests)) do.call(rbind, tests) else
    data.frame(feature = character(), covariate = character(), statistic = numeric(),
               df = numeric(), p.value = numeric(), status = character(), method = character())
  screening$q.value <- rep(NA_real_, nrow(screening))
  for (candidate in candidates) {
    rows <- which(screening$covariate == candidate)
    screening$q.value[rows] <- stats::p.adjust(screening$p.value[rows], method = "BH", n = length(features))
  }
  summary <- do.call(rbind, lapply(candidates, function(candidate) {
    q <- screening$q.value[screening$covariate == candidate]
    data.frame(covariate = candidate, tested = sum(is.finite(q)),
               significant = sum(q < 0.05, na.rm = TRUE), total_features = length(features))
  }))
  if (!is.null(summary)) {
    summary$significant_pct_all <- 100 * summary$significant / summary$total_features
    summary$significant_pct_tested <- ifelse(summary$tested > 0,
      100 * summary$significant / summary$tested, NA_real_)
  }
  effects <- .combatls_scale_effects(screen_fits, model_data, candidates)
  effect_summary <- if (nrow(effects)) do.call(rbind, lapply(
    split(effects, list(effects$covariate, effects$contrast), drop = TRUE), function(rows) {
      ratios <- rows$sd_ratio[is.finite(rows$sd_ratio)]
      quartiles <- if (length(ratios)) stats::quantile(ratios, c(.25, .5, .75), names = FALSE) else rep(NA_real_, 3)
      data.frame(covariate = rows$covariate[1L], contrast = rows$contrast[1L],
        estimated_features = length(ratios), total_features = length(features),
        sd_ratio_q25 = quartiles[1L], sd_ratio_median = quartiles[2L], sd_ratio_q75 = quartiles[3L])
    })) else data.frame()
  supported <- if (is.null(summary)) character() else summary$covariate[summary$significant > 0]
  selected_terms <- vapply(term_vars, function(vars) any(vars %in% supported), logical(1))
  draft_variables <- unique(unlist(term_vars[selected_terms]))
  # Retain marginal terms when a supported interaction enters the draft.
  draft_terms <- terms[vapply(term_vars, function(vars) all(vars %in% draft_variables), logical(1))]
  draft <- if (any(is.finite(screening$q.value)))
    .combatls_terms_formula(draft_terms, environment(exploratory)) else NULL
  structure(list(fits = fits, screen_fits = screen_fits, reduced_fits = reduced_fits,
                  screening = screening, summary = summary, effects = effects, effect_summary = effect_summary, diagnostics = do.call(rbind, diagnostics),
                  parameters = if (length(parameters)) do.call(rbind, parameters) else data.frame(),
                  data = df, model_data = model_data, features = features, covariates = covariates,
                  batch = batch, formula = location, sigma.formula = proposed,
                  screen.formula = exploratory, candidates = candidates, ref.batch = ref.batch,
                  suggested.formula = draft,
                  suggestion.rule = paste("Liberal screening draft: includes candidates with at least one feature q < 0.05,",
                    "retaining interaction marginal terms. Not an optimal model; review percentages, effect sizes and scientific relevance.")), class = "combatls_check")
}

.combatls_scale_effects <- function(fits, data, candidates) {
  rows <- list()
  for (feature in names(fits)) for (candidate in candidates) {
    fit <- fits[[feature]]
    column <- data[[candidate]]
    categorical <- is.factor(column)
    levels <- if (categorical) levels(column) else NULL
    contrasts <- if (categorical) paste0(levels[-1L], " vs ", levels[1L]) else "per 1 original unit"
    if (!length(contrasts)) contrasts <- "unavailable"
    values <- rep(NA_real_, length(contrasts))
    status <- "failed or nonconverged fit"
    if (.combatls_fit_ok(fit)) {
      labels <- attr(fit$sigma.terms, "term.labels")
      involved <- vapply(labels, function(term) candidate %in%
        all.vars(stats::as.formula(paste("~", term))), logical(1))
      raw <- vapply(labels, function(term) identical(str2lang(term), as.name(candidate)), logical(1))
      if (sum(involved) != 1L || !any(raw & involved)) {
        status <- "nonconstant or transformed effect; inspect centile curves"
      } else {
        indices <- which(attr(fit$sigma.x, "assign") == which(raw & involved))
        coefficients <- fit$sigma.coefficients[colnames(fit$sigma.x)[indices]]
        if (categorical && length(levels) > 1L) {
          reference <- which(column == levels[1L])[1L]
          values <- vapply(levels[-1L], function(level) {
            target <- which(column == level)[1L]
            sum((fit$sigma.x[target, indices] - fit$sigma.x[reference, indices]) * coefficients)
          }, numeric(1))
        } else if (is.numeric(column) && length(indices) == 1L) {
          values <- unname(coefficients)
        }
        status <- if (all(is.finite(values))) "ok" else "effect not estimable"
      }
    }
    rows[[length(rows) + 1L]] <- data.frame(feature = feature, covariate = candidate,
      contrast = contrasts, log_sd_ratio = values, sd_ratio = exp(values),
      sd_change_pct = 100 * expm1(values), status = status, stringsAsFactors = FALSE)
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

.combatls_exploratory_fit <- function(mu, sigma, data, control) {
  warnings <- character()
  error <- NULL
  fit <- tryCatch(withCallingHandlers(
    gamlss::gamlss(formula = mu, sigma.formula = sigma, data = data,
                   family = gamlss.dist::NO(), control = control),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) { error <<- conditionMessage(e); NULL })
  if (!is.null(fit) && !isTRUE(fit$converged)) warnings <- c(warnings, "GAMLSS did not converge.")
  if (!is.null(fit) && any(!is.finite(c(fit$mu.coefficients, fit$sigma.coefficients))))
    warnings <- c(warnings, "Aliased or nonfinite coefficients; scale tests are unavailable.")
  list(fit = fit, warnings = unique(warnings), error = error)
}

.combatls_same_formula <- function(left, right) {
  if (length(left) != length(right) ||
      !all(vapply(seq_along(left), function(i) identical(left[[i]], right[[i]]), logical(1))))
    return(FALSE)
  if (identical(environment(left), environment(right))) return(TRUE)
  call_functions <- function(expr) {
    if (!is.call(expr)) return(character())
    head <- expr[[1L]]
    if (!is.symbol(head)) return(NA_character_)
    c(as.character(head), unlist(lapply(as.list(expr)[-1L], call_functions), use.names = FALSE))
  }
  helpers <- unique(call_functions(left))
  if (anyNA(helpers)) return(FALSE)
  all(vapply(helpers, function(name) {
    a <- tryCatch(get(name, envir = environment(left), mode = "function"), error = function(e) NULL)
    b <- tryCatch(get(name, envir = environment(right), mode = "function"), error = function(e) NULL)
    !is.null(a) && identical(a, b)
  }, logical(1)))
}

.combatls_fit_ok <- function(fit) {
  !is.null(fit) && isTRUE(fit$converged) && is.finite(fit$G.deviance) &&
    is.finite(fit$df.fit) && all(is.finite(c(fit$mu.coefficients, fit$sigma.coefficients)))
}

.combatls_scale_lrt <- function(reduced, full) {
  out <- list(statistic = NA_real_, df = NA_real_, p.value = NA_real_, status = "failed or nonconverged fit")
  if (!.combatls_fit_ok(reduced) || !.combatls_fit_ok(full)) return(out)
  out$statistic <- reduced$G.deviance - full$G.deviance
  out$df <- full$df.fit - reduced$df.fit
  if (out$df <= 0 || out$statistic < -1e-5) { out$status <- "invalid nested-model comparison"; return(out) }
  out$statistic <- max(0, out$statistic)
  out$p.value <- stats::pchisq(out$statistic, df = out$df, lower.tail = FALSE)
  out$status <- "ok"
  out
}

#' Model-implied Centiles from a Saved ComBatLS Model Check
#'
#' Predict on a new covariate grid without fitting GAMLSS models again. Other
#' numeric covariates are held at their medians and categorical covariates at
#' their most frequent levels. Batch location and log-scale effects are averaged
#' using the check's observed proportions, or its selected reference batch.
#' These conditional normal-model centiles are not empirical population centiles
#' and are not predictions for harmonized observations.
#' @param check Output of `combatls_model_check()`.
#' @param feature A checked feature.
#' @param x Numeric covariate for the horizontal axis.
#' @param group Optional categorical covariate defining separate curves.
#' @param probabilities Centile probabilities strictly between zero and one.
#' @param n Number of grid points.
#' @param x.quantiles Optional probabilities in `[0, 1]` at which to evaluate the
#'   horizontal covariate's observed quantiles, separately within each group.
#'   Overrides the equally spaced grid and `n`; e.g. `c(.25, .75)` for SD comparisons.
#' @param model Use the proposed or exploratory screening scale model; `"both"`
#'   compares both on the same grid using saved fits.
#' @return A data frame with grid covariates, predicted mu and sigma, probability,
#'   centile, feature, and model. Fitting errors and nonconvergence prevent curves.
#' @export
combatls_check_centiles <- function(check, feature, x, group = NULL,
                                    probabilities = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    n = 80, model = c("proposed", "screening", "both"),
                                    x.quantiles = NULL) {
  stopifnot(inherits(check, "combatls_check"))
  model <- match.arg(model)
  if (model == "both") {
    proposed <- combatls_check_centiles(check, feature, x, group, probabilities, n,
      model = "proposed", x.quantiles = x.quantiles)
    screening <- if (identical(check$fits[[feature]], check$screen_fits[[feature]])) proposed else
      combatls_check_centiles(check, feature, x, group, probabilities, n,
        model = "screening", x.quantiles = x.quantiles)
    screening$model <- "screening"
    return(rbind(proposed, screening))
  }
  if (length(feature) != 1L || length(x) != 1L || !feature %in% check$features || !x %in% check$covariates || !is.numeric(check$data[[x]]))
    stop("Select a checked feature and a numeric covariate for centile curves.", call. = FALSE)
  if (!is.null(group) && (length(group) != 1L || !group %in% check$covariates || is.numeric(check$data[[group]])))
    stop("Curve groups must be categorical covariates.", call. = FALSE)
  if (!is.numeric(probabilities) || !length(probabilities) || any(!is.finite(probabilities) | probabilities <= 0 | probabilities >= 1) ||
      (is.null(x.quantiles) && (length(n) != 1 || !is.finite(n) || n < 2 || n != as.integer(n))))
    stop("Use probabilities between zero and one and an integer n >= 2.", call. = FALSE)
  if (!is.null(x.quantiles)) {
    if (!is.numeric(x.quantiles) || !length(x.quantiles) || any(!is.finite(x.quantiles) | x.quantiles < 0 | x.quantiles > 1))
      stop("x.quantiles must contain probabilities between zero and one.", call. = FALSE)
    n <- length(x.quantiles)
  }
  fit <- if (model == "proposed") check$fits[[feature]] else check$screen_fits[[feature]]
  if (!.combatls_fit_ok(fit)) stop("Centiles require a converged, identifiable model.", call. = FALSE)
  base <- check$model_data[1L, , drop = FALSE]
  for (variable in check$covariates) {
    column <- check$model_data[[variable]]
    base[[variable]] <- if (is.numeric(column)) stats::median(column) else {
      level <- names(which.max(table(column)))
      factor(level, levels = levels(column))
    }
  }
  groups <- if (is.null(group)) "All" else levels(check$model_data[[group]])
  grid <- base[rep(1L, n * length(groups)), , drop = FALSE]
  grid[[x]] <- unlist(lapply(groups, function(level) {
    observed <- if (is.null(group)) check$data[[x]] else check$data[[x]][as.character(check$data[[group]]) == level]
    if (is.null(x.quantiles)) seq(min(observed), max(observed), length.out = n)
    else as.numeric(stats::quantile(observed, probs = x.quantiles, names = FALSE))
  }), use.names = FALSE)
  if (!is.null(group)) grid[[group]] <- factor(rep(groups, each = n), levels = groups)
  weights <- prop.table(table(check$model_data$.combatls_batch))
  if (!is.null(check$ref.batch)) weights[] <- as.numeric(names(weights) == check$ref.batch)
  train <- check$model_data
  train$y <- check$data[[feature]]
  weights <- weights[weights > 0]
  # Predict all target batches together, evaluating each smooth only once per
  # distribution parameter, then average on the same link scales as before.
  prediction_grid <- grid[rep(seq_len(nrow(grid)), times = length(weights)), , drop = FALSE]
  prediction_grid$.combatls_batch <- factor(rep(names(weights), each = nrow(grid)),
                                           levels = levels(train$.combatls_batch))
  mu <- drop(matrix(as.numeric(stats::predict(fit, newdata = prediction_grid, data = train,
    what = "mu", type = "link")), nrow = nrow(grid)) %*% as.numeric(weights))
  log_sigma <- drop(matrix(as.numeric(stats::predict(fit, newdata = prediction_grid, data = train,
    what = "sigma", type = "link")), nrow = nrow(grid)) %*% as.numeric(weights))
  grid$.combatls_batch <- NULL
  grid$mu <- mu
  grid$sigma <- exp(log_sigma)
  if (any(!is.finite(mu) | !is.finite(grid$sigma) | grid$sigma <= 0))
    stop("The fitted model produced invalid distribution parameters on this grid.", call. = FALSE)
  grid$curve_group <- rep(groups, each = n)
  do.call(rbind, lapply(probabilities, function(probability) {
    out <- grid
    out$probability <- probability
    out$centile <- out$mu + stats::qnorm(probability) * out$sigma
    out$feature <- feature
    out$model <- model
    out
  }))
}

#' Plot a Cached ComBatLS Scale Model Check
#' @param check Output of `combatls_model_check()`.
#' @param type Plot type: scale-screening heatmap, normalized residual Q-Q plot,
#'   residual-versus-covariate trends, or conditional centiles.
#' @param feature A checked feature for residuals or centiles.
#' @param x Covariate for residual trends, or numeric covariate for centiles.
#' @param group Optional categorical grouping for centile curves.
#' @param model Proposed or screening model; `"both"` overlays centiles only.
#' @param show.points For centiles, overlay raw measurements (default FALSE).
#'   Their covariates and batches are not standardized to the curve's profile;
#'   the overlay is descriptive, not a calibrated check of centile coverage.
#' @param show.y.labels For the screening heatmap, show feature names on the
#'   y-axis (default TRUE). Does not change the y-axis title or other plot types.
#' @return A ggplot object. The heatmap shows BH-adjusted p-values separately
#'   across features per candidate, labeled BH-adjusted p-values. Axis and legend
#'   text are bold; tiles have no numeric labels. Missing tests are gray. Its data
#'   retain feature names and numeric test results. Plot changes reuse
#'   cached models and never run GAMLSS fitting. Centile plots use solid median
#'   lines and darker 25th-75th and lighter 5th-95th centile bands, colored by model.
#'   The bands are predicted centiles, not confidence intervals. Ungrouped curves
#'   have no facet header.
#' @export
combatls_check_plot <- function(check, type = c("screening", "qq", "residuals", "centiles"),
                                feature = check$features[1L], x = NULL, group = NULL,
                                model = c("proposed", "screening", "both"), show.points = FALSE,
                                show.y.labels = TRUE) {
  stopifnot(inherits(check, "combatls_check"))
  type <- match.arg(type)
  model <- match.arg(model)
  if (type == "screening") {
    if (!nrow(check$screening)) stop("Select candidate scale covariates to screen.", call. = FALSE)
    data <- check$screening
    data$evidence <- cut(data$q.value, breaks = c(-Inf, .01, .05, Inf),
      labels = c("Strong evidence (adjusted p < 0.01)", "Evidence (0.01 <= adjusted p < 0.05)",
                 "Not significant (adjusted p >= 0.05)"), right = FALSE)
    return(ggplot(data, aes(x = .data[["covariate"]], y = .data[["feature"]], fill = .data[["evidence"]])) +
      geom_tile(color = "white", linewidth = .25) +
      scale_fill_manual(values = c("Strong evidence (adjusted p < 0.01)" = "#234E70",
        "Evidence (0.01 <= adjusted p < 0.05)" = "#5AB4AC",
        "Not significant (adjusted p >= 0.05)" = "#EEF2F6"), na.value = "#B8BEC7", drop = FALSE,
        breaks = levels(data$evidence),
        labels = c("Strong evidence (< 0.01)", "Evidence (0.01 to < 0.05)",
                   "Not significant (>= 0.05)")) +
      scale_x_discrete(expand = expansion(mult = 0, add = .02)) +
      scale_y_discrete(expand = expansion(mult = 0, add = .02)) +
      labs(title = NULL, x = "Biological covariate", y = "Feature",
           fill = "BH-adjusted p-value", caption = NULL) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "top")) +
      theme_minimal(base_size = 12) +
      theme(axis.title = element_text(face = "bold", color = "#263442"),
        axis.text.x = element_text(face = "bold", color = "#263442", margin = margin(t = 8)),
        axis.text.y = if (isTRUE(show.y.labels)) element_text(face = "bold", size = 9,
          color = "#263442") else element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_rect(color = "#DFE5EC", fill = NA, linewidth = .4),
        legend.position = "bottom", legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(face = "bold", size = 10),
        legend.key.height = grid::unit(.5, "cm"),
        plot.margin = margin(12, 12, 8, 8)))
  }
  if (type == "centiles") {
    data <- combatls_check_centiles(check, feature, x, group, model = model)
    plot <- .combatls_centile_plot(data, feature, x, group)
    if (isTRUE(show.points)) plot <- .combatls_add_observations(plot, check, feature, x, group)
    return(plot)
  }
  if (model == "both") stop("Use model = 'both' only for centile comparisons.", call. = FALSE)
  if (!nrow(check$parameters) || !feature %in% check$features) stop("No fitted residuals for this feature.", call. = FALSE)
  data <- check$parameters[check$parameters$feature == feature & check$parameters$model == model, ]
  if (!nrow(data)) stop("The selected feature model did not fit.", call. = FALSE)
  if (type == "qq") return(ggplot(data, aes(sample = .data[["residual"]])) + stat_qq() + stat_qq_line() +
    labs(title = paste("Normalized residual Q-Q:", feature, model), y = "Normalized quantile residual", x = "Normal theoretical quantile") + theme_minimal())
  if (length(x) != 1L || !x %in% check$covariates) stop("Select a covariate for residual trends.", call. = FALSE)
  data$covariate <- check$data[[x]][data$row]
  plot <- ggplot(data, aes(x = .data[["covariate"]], y = .data[["residual"]])) + geom_hline(yintercept = 0, linetype = 2)
  if (is.numeric(data$covariate)) plot <- plot + geom_point(alpha = .35) + geom_smooth(method = "loess", formula = y ~ x)
  else plot <- plot + geom_boxplot()
  plot + labs(title = paste("Normalized residual trends:", feature, model), x = x, y = "Normalized quantile residual") + theme_minimal()
}

.combatls_terms_formula <- function(terms, env, intercept = 1L) {
  if (!length(terms)) stats::as.formula(if (intercept) "~ 1" else "~ 0", env = env)
  else stats::reformulate(terms, intercept = intercept, env = env)
}
