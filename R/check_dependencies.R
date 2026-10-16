#' Check and optionally install package dependencies
#'
#' Checks required packages and optional ComBatFamily, including declared version
#' constraints and dependencies of installed packages. Nothing is installed by
#' default. Installation uses CRAN for ordinary packages and the upstream GitHub
#' repository for ComBatFamily. Only missing, outdated, or unloadable packages
#' are targeted; unrelated packages are not upgraded.
#'
#' @param install Logical; install or update packages with problems if TRUE.
#' @param include_suggests Logical; also check all suggested packages, rather
#'   than only the optional ComBatFamily backend.
#' @param lib Library in which to install packages. Defaults to the first R library.
#' @param repos CRAN repositories passed to the installers.
#' @return A data frame with package, required version, installed version, status,
#'   and installation command. Status is `ok`, `missing`, `outdated`,
#'   `unavailable` (installed but cannot load), or `restart_required`.
#'   After installation the dependencies are checked again. Dependencies of a
#'   missing package are resolved by the installer, since their metadata is not
#'   available locally. R version problems must be resolved by updating R.
#' @export
#' @examples
#' \dontrun{
#' check_dependencies()
#' check_dependencies(install = TRUE)
#' check_dependencies(include_suggests = TRUE)
#' }
check_dependencies <- function(install = FALSE, include_suggests = FALSE,
                               lib = .libPaths()[1L], repos = getOption("repos")) {
  stopifnot(is.logical(install), length(install) == 1L, !is.na(install),
            is.logical(include_suggests), length(include_suggests) == 1L,
            !is.na(include_suggests))
  if (is.null(repos) || !length(repos) || any(repos == "@CRAN@"))
    repos <- c(CRAN = "https://cloud.r-project.org")
  report <- .dependency_report(include_suggests, lib)
  if (install) {
    if (any(report$package == "R" & report$status != "ok"))
      stop("Update R to meet the reported version requirement before installing dependencies.", call. = FALSE)
    targets <- report$package[report$status %in% c("missing", "outdated", "unavailable")]
    cran <- setdiff(targets, "ComBatFamily")
    if (length(cran)) utils::install.packages(cran, lib = lib, repos = repos)
    if ("ComBatFamily" %in% targets) {
      if (!requireNamespace("remotes", quietly = TRUE, lib.loc = unique(c(lib, .libPaths()))))
        utils::install.packages("remotes", lib = lib, repos = repos)
      if (!requireNamespace("remotes", quietly = TRUE, lib.loc = unique(c(lib, .libPaths()))))
        stop("Could not load remotes after installation. Check the installation output.", call. = FALSE)
      remotes::install_github("andy1764/ComBatFamily", lib = lib, repos = repos,
                              dependencies = NA, upgrade = "never",
                              build_vignettes = FALSE)
    }
    report <- .dependency_report(include_suggests, lib)
  }
  problems <- report$package[report$status != "ok"]
  if (length(problems)) {
    message("Dependencies needing attention: ", paste(problems, collapse = ", "),
            ". See the returned status and installation commands.")
  } else message("All checked dependencies meet their version requirements and can load.")
  report
}

.dependency_description <- function(package, lib) {
  suppressWarnings(utils::packageDescription(package, lib.loc = unique(c(lib, .libPaths()))))
}

.dependency_report <- function(include_suggests, lib) {
  root <- .dependency_description("ComBatFamQC", lib)
  fields <- c("Depends", "Imports", "LinkingTo")
  specs <- c(unlist(root[fields]), "ComBatFamily (>= 0.2.2)")
  if (include_suggests) specs <- c(specs, root$Suggests)
  requirements <- list()
  visited <- "ComBatFamQC"
  while (length(specs)) {
    entries <- trimws(unlist(strsplit(paste(specs, collapse = ","), ",")))
    entries <- entries[nzchar(entries)]
    specs <- character()
    for (entry in entries) {
      package <- sub("\\s*\\(.*$", "", entry)
      constraint <- if (grepl("(", entry, fixed = TRUE))
        sub(".*\\((.*)\\).*", "\\1", entry) else ""
      requirements[[package]] <- unique(c(requirements[[package]], constraint))
      if (package %in% c(visited, "R")) next
      visited <- c(visited, package)
      desc <- .dependency_description(package, lib)
      if (is.list(desc)) specs <- c(specs, unlist(desc[fields]))
    }
  }
  rows <- lapply(names(requirements), function(package) {
    constraints <- requirements[[package]]
    constraints <- constraints[nzchar(constraints)]
    desc <- if (package != "R") .dependency_description(package, lib) else NULL
    version <- if (package == "R") as.character(getRversion()) else
      if (is.list(desc)) desc$Version else NA_character_
    status <- if (is.na(version)) "missing" else "ok"
    if (status == "ok" && length(constraints)) {
      meets <- vapply(constraints, function(constraint) {
        parts <- strsplit(trimws(constraint), "[[:space:]]+")[[1]]
        comparison <- utils::compareVersion(version, parts[2L])
        switch(parts[1L], ">=" = comparison >= 0, ">" = comparison > 0,
               "<=" = comparison <= 0, "<" = comparison < 0,
               "==" = comparison == 0, "!=" = comparison != 0, FALSE)
      }, logical(1))
      if (!all(meets)) status <- "outdated"
    }
    if (status == "ok" && package != "R") {
      if (package %in% loadedNamespaces() &&
          as.character(getNamespaceVersion(package)) != version) {
        status <- "restart_required"
      } else if (!requireNamespace(package, quietly = TRUE, lib.loc = unique(c(lib, .libPaths())))) status <- "unavailable"
    }
    command <- if (package == "ComBatFamily")
      'remotes::install_github("andy1764/ComBatFamily", upgrade = "never", build_vignettes = FALSE)' else
      if (package == "R") "Update R" else sprintf('install.packages("%s")', package)
    data.frame(package = package, required_version = paste(constraints, collapse = "; "),
               installed_version = version, status = status,
               install_command = command, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
