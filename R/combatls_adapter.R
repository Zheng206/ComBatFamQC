.require_combatfamily <- function() {
  if (!requireNamespace("ComBatFamily", quietly = TRUE) ||
      getNamespaceVersion("ComBatFamily") < "0.2.2" ||
      !"comfam" %in% getNamespaceExports("ComBatFamily")) {
    stop("ComBatLS requires ComBatFamily >= 0.2.2. Run check_dependencies(install = TRUE).",
         call. = FALSE)
  }
}

# Keep the statistical implementation in ComBatFamily; translate only the API.
.combatls_adapter <- function(data, bat, covar, formula, sigma.formula,
                              ref.batch, eb, robust.LS = FALSE, ...) {
  formula <- .combatls_formula(formula, covar, location = TRUE)
  sigma.formula <- .combatls_formula(sigma.formula, covar, location = FALSE)
  # combatls() uses an unqualified NO() in upstream 0.2.2. Call its core
  # implementation with explicit namespaces so installed-package use does not
  # depend on gamlss.dist being attached to the search path.
  ComBatFamily::comfam(data = data, bat = bat, covar = covar,
                        model = gamlss::gamlss, family = gamlss.dist::NO(),
                        formula = formula, sigma.formula = sigma.formula,
                        ref.batch = ref.batch, eb = eb, robust.LS = robust.LS, ...)
}

# Validate shape before data preparation and again at the backend boundary.
.validate_combatls_formula <- function(formula, location) {
  expected_length <- if (location) 3L else 2L
  label <- if (location) "formula" else "sigma.formula"
  if (!inherits(formula, "formula") || length(formula) != expected_length)
    stop(label, if (location) " must be a two-sided formula with response y." else
      " must be a one-sided formula, for example ~ AGE + SEX.", call. = FALSE)
  if (location && !identical(formula[[2L]], quote(y)))
    stop("formula must use y as its response.", call. = FALSE)
}

# Resolve GAMLSS helpers without rewriting terms or discarding the caller's scope.
.combatls_formula <- function(formula, covar, location) {
  .validate_combatls_formula(formula, location)
  label <- if (location) "formula" else "sigma.formula"
  has_mgcv_smoother <- function(expr) {
    if (!is.call(expr)) return(FALSE)
    head <- expr[[1L]]
    if (is.call(head) && identical(head[[1L]], quote(`::`))) head <- head[[3L]]
    if (is.symbol(head) && as.character(head) %in% c("s", "te", "ti", "t2")) return(TRUE)
    any(vapply(as.list(expr)[-1L], has_mgcv_smoother, logical(1)))
  }
  if (has_mgcv_smoother(formula))
    stop("Unsupported mgcv smoother in ", label,
         "; specify a GAMLSS-compatible formula, for example pb(AGE). No smoother conversion is performed.", call. = FALSE)
  variables <- all.vars(if (location) formula[[3L]] else formula)
  if (any(!variables %in% names(covar)))
    stop("All ", label, " variables must be listed in covariates.", call. = FALSE)
  scope <- environment(formula)
  helpers <- intersect(all.names(formula), getNamespaceExports("gamlss"))
  helpers <- helpers[!vapply(helpers, exists, logical(1), envir = scope, inherits = TRUE)]
  if (length(helpers)) {
    bindings <- lapply(helpers, function(name) getExportedValue("gamlss", name))
    names(bindings) <- helpers
    environment(formula) <- list2env(bindings, parent = scope)
  }
  formula
}
