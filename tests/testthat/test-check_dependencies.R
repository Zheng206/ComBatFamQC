test_that("dependency checks report missing and outdated packages without installing", {
  local_mocked_bindings(.dependency_description = function(package, lib) {
    switch(package,
           ComBatFamQC = list(Depends = "R (>= 4.1.0)",
                             Imports = "stats (>= 999.0)",
                             Suggests = "missingSuggestedPackage"),
           stats = list(Version = as.character(getRversion())),
           ComBatFamily = list(Version = "0.1.0", Imports = "missingBackendDependency"),
           NA)
  })
  expect_message(report <- check_dependencies(), "needing attention")
  status <- setNames(report$status, report$package)
  expect_equal(unname(status[c("R", "stats", "ComBatFamily", "missingBackendDependency")]),
               c("ok", "outdated", "outdated", "missing"))
  expect_false("missingSuggestedPackage" %in% report$package)
  expect_match(report$install_command[report$package == "ComBatFamily"], "andy1764/ComBatFamily", fixed = TRUE)
  expect_message(all <- check_dependencies(include_suggests = TRUE), "needing attention")
  expect_true("missingSuggestedPackage" %in% all$package)
})

test_that("installer targets only broken packages and rechecks afterwards", {
  skip_if_not_installed("remotes")
  installed <- FALSE
  local_mocked_bindings(.dependency_report = function(include_suggests, lib) {
    data.frame(package = c("stats", "missingPackage", "ComBatFamily"),
               status = if (installed) rep("ok", 3) else c("ok", "missing", "outdated"))
  })
  cran_calls <- list()
  github_calls <- list()
  local_mocked_bindings(install.packages = function(pkgs, ...) {
    cran_calls[[length(cran_calls) + 1L]] <<- list(pkgs = pkgs, ...)
  }, .package = "utils")
  local_mocked_bindings(install_github = function(repo, ...) {
    github_calls[[1L]] <<- list(repo = repo, ...)
    installed <<- TRUE
  }, .package = "remotes")
  expect_message(report <- check_dependencies(install = TRUE), "All checked")
  expect_identical(cran_calls[[1L]]$pkgs, "missingPackage")
  expect_identical(github_calls[[1L]]$repo, "andy1764/ComBatFamily")
  expect_identical(github_calls[[1L]]$upgrade, "never")
  expect_false(github_calls[[1L]]$build_vignettes)
  expect_true(all(report$status == "ok"))
})

test_that("incompatible R versions are reported and cannot be installed as packages", {
  local_mocked_bindings(.dependency_description = function(package, lib) {
    if (package == "ComBatFamQC") list(Depends = "R (>= 999.0)") else NA
  })
  expect_message(report <- check_dependencies(), "needing attention")
  expect_identical(report$status[report$package == "R"], "outdated")
  expect_error(check_dependencies(install = TRUE), "Update R")
})
