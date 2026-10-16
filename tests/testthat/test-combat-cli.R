run_combat_cli <- function(args) {
  for (package in c("argparser", "readr", "readxl", "stringr"))
    skip_if_not_installed(package)
  scope <- new.env(parent = globalenv())
  scope$parse_args <- function(parser) argparser::parse_args(parser, argv = args)
  captured <- NULL
  scope$combat_harm <- function(...) {
    captured <<- list(...)
    ComBatFamQC::combat_harm(...)
  }
  sys.source(system.file("combatQC_CLI.R", package = "ComBatFamQC"), envir = scope)
  list(arguments = captured, result = scope$harm_result)
}

test_that("CLI forwards ComBatLS formulas and fitting controls", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  input <- tempfile(fileext = ".csv")
  output <- tempfile(fileext = ".csv")
  model <- tempfile(fileext = ".rds")
  utils::write.csv(iris, input, row.names = FALSE)
  cli <- run_combat_cli(c(input, "--diagnosis", "FALSE", "--family", "combatls",
                          "--features", "1-2", "--batch", "5", "--covariates", "3-4",
                          "--formula", "y ~ pb(Petal.Length, df = 3) + Petal.Width",
                          "--sigma.formula", "~ Petal.Length + Petal.Width",
                          "--eb", "FALSE", "--robust.LS", "TRUE",
                          "--outdir", output, "--mout", model))
  expect_identical(cli$arguments$formula[[3L]],
                   quote(pb(Petal.Length, df = 3) + Petal.Width))
  expect_identical(cli$arguments$sigma.formula[[2L]], quote(Petal.Length + Petal.Width))
  expect_false(cli$arguments$eb)
  expect_true(cli$arguments$robust.LS)
  expect_identical(cli$result$com_family, "combatls")
  expect_equal(dim(utils::read.csv(output)), dim(iris))
  saved <- readRDS(model)
  expect_identical(saved$family, "combatls")
  expect_null(saved$ComBat.model$dat.combat)
})

test_that("CLI keeps the constant-scale default with an explicit location formula", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  input <- tempfile(fileext = ".csv")
  output <- tempfile(fileext = ".csv")
  utils::write.csv(iris, input, row.names = FALSE)
  cli <- run_combat_cli(c(input, "--diagnosis", "FALSE", "--family", "combatls",
                          "--features", "1-2", "--batch", "5", "--covariates", "3-4",
                          "--formula", "y ~ Petal.Length * Petal.Width", "--eb", "FALSE", "--outdir", output))
  expect_false("sigma.formula" %in% names(cli$arguments))
  expect_identical(cli$arguments$formula[[3L]], quote(Petal.Length * Petal.Width))
  expect_null(cli$arguments$smooth_int_type)
  expect_null(cli$arguments$interaction)
  expect_identical(cli$result$com_family, "combatls")
  expect_equal(all.vars(cli$result$combat.object$ComBat.model$fits[[1L]]$sigma.formula), character())
})

test_that("CLI rejects misplaced or malformed ComBatLS formula arguments", {
  base <- c("unused.csv", "--diagnosis", "FALSE", "--family", "combatls")
  expect_error(run_combat_cli(c("unused.csv", "--formula", "y ~ AGE")), "require --family combatls")
  expect_error(run_combat_cli(c(base, "--smooth", "2")), "does not support")
  expect_error(run_combat_cli(c(base, "--random", "2")), "does not support")
  expect_error(run_combat_cli(base), "requires --formula")
  expect_error(run_combat_cli(c(base, "--sigma.formula", "~ AGE")), "requires --formula")
  expect_error(run_combat_cli(c(base, "--interaction", "2*3")), "does not support")
  expect_error(run_combat_cli(c(base, "--formula", "y ~ AGE", "--interaction", "2*3")), "does not support")
  expect_error(run_combat_cli(c(base, "--predict", "TRUE")), "Saved-model prediction")
  expect_error(run_combat_cli(c(base, "--formula", "~ AGE")), "two-sided")
  expect_error(run_combat_cli(c(base, "--formula", "AGE ~ SEX")), "response")
  expect_error(run_combat_cli(c(base, "--formula", "y ~ AGE", "--sigma.formula", "y ~ AGE")), "one-sided")
  expect_error(run_combat_cli(c(base, "--formula", "y ~ AGE", "--sigma.formula", "~ (")), "Invalid --sigma.formula")
})

test_that("CLI reports missing ComBatFamily without installing it", {
  input <- tempfile(fileext = ".csv")
  output <- tempfile(fileext = ".csv")
  utils::write.csv(iris, input, row.names = FALSE)
  local_mocked_bindings(.require_combatfamily = function() {
    stop("ComBatLS requires ComBatFamily >= 0.2.2. Run check_dependencies(install = TRUE).", call. = FALSE)
  })
  expect_error(run_combat_cli(c(input, "--diagnosis", "FALSE", "--family", "combatls",
                                "--features", "1-2", "--batch", "5", "--formula", "y ~ 1", "--outdir", output)),
               "check_dependencies(install = TRUE)", fixed = TRUE)
  expect_false(file.exists(output))
})

test_that("CLI reports missing formula covariates before loading the backend", {
  input <- tempfile(fileext = ".csv")
  output <- tempfile(fileext = ".csv")
  utils::write.csv(iris, input, row.names = FALSE)
  local_mocked_bindings(.require_combatfamily = function() stop("backend should not load"))
  base <- c(input, "--diagnosis", "FALSE", "--family", "combatls",
            "--features", "1-2", "--batch", "5", "--outdir", output)
  expect_error(run_combat_cli(c(base, "--covariates", "3",
                                "--formula", "y ~ pb(Petal.Length) + Petal.Width")),
               "Formula variables missing from --covariates: Petal.Width", fixed = TRUE)
  expect_error(run_combat_cli(c(base, "--covariates", "3",
                                "--formula", "y ~ Petal.Length",
                                "--sigma.formula", "~ Petal.Width")),
               "Formula variables missing from --covariates: Petal.Width", fixed = TRUE)
  # Omitting --covariates lists each missing variable once across both formulas.
  expect_error(run_combat_cli(c(base, "--formula", "y ~ Petal.Length + Petal.Width",
                                "--sigma.formula", "~ Petal.Length")),
               "Formula variables missing from --covariates: Petal.Length, Petal.Width", fixed = TRUE)
  expect_false(file.exists(output))
})

test_that("ComBatLS CLI selects GAMLSS independently of the model option", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  input <- tempfile(fileext = ".csv")
  output <- tempfile(fileext = ".csv")
  utils::write.csv(iris, input, row.names = FALSE)
  args <- c(input, "--diagnosis", "FALSE", "--family", "combatls",
            "--features", "1-2", "--batch", "5", "--covariates", "3-4",
            "--formula", "y ~ pb(Petal.Length, df = 3) + Petal.Width",
            "--sigma.formula", "~ Petal.Width", "--eb", "FALSE", "--outdir", output)
  default <- run_combat_cli(args)
  for (model in c("lm", "gam", "lmer", "gamlss")) {
    explicit <- run_combat_cli(c(args, "--model", model))
    expect_identical(explicit$arguments$type, "lm")
    expect_s3_class(explicit$result$combat.object$ComBat.model$fits[[1L]], "gamlss")
    expect_equal(explicit$result$harmonized_df, default$result$harmonized_df)
  }
})

test_that("CLI still honors the model option for built-in ComBat", {
  input <- tempfile(fileext = ".csv")
  output <- tempfile(fileext = ".csv")
  utils::write.csv(iris, input, row.names = FALSE)
  args <- c(input, "--diagnosis", "FALSE", "--family", "comfam",
            "--features", "1-2", "--batch", "5", "--covariates", "3-4",
            "--eb", "FALSE", "--outdir", output)
  for (model in c("lm", "gam")) {
    smooth_args <- if (model == "gam") c("--smooth", "3") else character()
    fit <- run_combat_cli(c(args, "--model", model, smooth_args))
    expect_identical(fit$arguments$type, model)
    expect_s3_class(fit$result$combat.object$ComBat.model$fits[[1L]], model)
  }
})
