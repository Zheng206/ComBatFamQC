test_that("ComBatLS matches the external implementation and preserves the API", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  args <- list(df = iris, features = names(iris)[1:2], batch = "Species",
               covariates = names(iris)[3:4], family = "combatls", eb = FALSE,
               formula = y ~ Petal.Length + Petal.Width,
               control = gamlss::gamlss.control(trace = FALSE))
  fit <- do.call(combat_harm, args)
  direct <- ComBatFamily::comfam(
    iris[1:2], iris$Species, covar = iris[3:4],
    model = gamlss::gamlss, family = gamlss.dist::NO(),
    formula = y ~ Petal.Length + Petal.Width,
    sigma.formula = ~ 1, eb = FALSE,
    control = gamlss::gamlss.control(trace = FALSE))
  expect_equal(as.matrix(fit$harmonized_df[1:2]), direct$dat.combat)
  expect_identical(fit$harmonized_df[3:5], iris[3:5])
  expect_identical(names(fit$harmonized_df), names(iris))
  expect_identical(fit$com_family, "combatls")
  expect_identical(fit$combat.object$family, "combatls")
  expect_s3_class(fit$combat.object$ComBat.model$fits[[1]], "gamlss")
  expect_error(combat_harm(df = iris, predict = TRUE, object = fit$combat.object),
               "Saved-model prediction is not supported")

  # Biological scale effects are an explicit choice, independent of the mean.
  args$formula <- y ~ Petal.Length + Petal.Width
  args$sigma.formula <- ~ Petal.Length + Petal.Width
  biological <- do.call(combat_harm, args)
  direct_biological <- ComBatFamily::comfam(
    iris[1:2], iris$Species, covar = iris[3:4],
    model = gamlss::gamlss, family = gamlss.dist::NO(),
    formula = y ~ Petal.Length + Petal.Width,
    sigma.formula = ~ Petal.Length + Petal.Width, eb = FALSE,
    control = gamlss::gamlss.control(trace = FALSE))
  expect_equal(as.matrix(biological$harmonized_df[1:2]), direct_biological$dat.combat)
  expect_false(isTRUE(all.equal(biological$harmonized_df[1:2], fit$harmonized_df[1:2])))

  args$sigma.formula <- ~ 1
  args$ref.batch <- "setosa"
  reference_fit <- do.call(combat_harm, args)
  expect_equal(reference_fit$harmonized_df[1:50, 1:2], iris[1:50, 1:2])
})

test_that("ComBatLS uses GAMLSS smooths and supports reference data", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  set.seed(731)
  data <- iris
  data$Petal.Length <- rnorm(nrow(data))
  data$Petal.Width <- rnorm(nrow(data))
  data$Sepal.Length <- 3 + sin(data$Petal.Length) + rnorm(nrow(data)) * exp(0.1 * data$Petal.Length)
  data$Sepal.Width <- 2 + data$Petal.Width + rnorm(nrow(data)) * exp(0.2 * data$Petal.Length)
  args <- list(df = data, features = names(data)[1:2], batch = "Species",
               covariates = names(data)[3:4], family = "combatls", eb = FALSE,
               formula = y ~ pb(Petal.Length, df = 3) + Petal.Width,
               sigma.formula = ~ pb(Petal.Length, df = 3) + Petal.Width,
               control = gamlss::gamlss.control(trace = FALSE))
  fit <- do.call(combat_harm, args)
  direct <- ComBatFamily::comfam(
    data[1:2], data$Species, covar = data[3:4],
    model = gamlss::gamlss, family = gamlss.dist::NO(),
    formula = stats::as.formula("y ~ pb(Petal.Length, df = 3) + Petal.Width", env = asNamespace("gamlss")),
    sigma.formula = stats::as.formula("~ pb(Petal.Length, df = 3) + Petal.Width", env = asNamespace("gamlss")), eb = FALSE,
    control = gamlss::gamlss.control(trace = FALSE))
  expect_equal(as.matrix(fit$harmonized_df[1:2]), direct$dat.combat, tolerance = 1e-8)

  args$type <- "lm"
  args$smooth <- NULL
  args$reference <- data[1:50, ]
  args$out_ref_include <- FALSE
  ref_fit <- do.call(combat_harm, args)
  expect_identical(names(ref_fit$harmonized_df), names(data))
  expect_equal(ref_fit$harmonized_df[1:50, 1:2], data[1:50, 1:2])
  expect_equal(nrow(ref_fit$harmonized_df), nrow(data))
})

test_that("unsupported ComBatLS combinations fail clearly", {
  expect_error(combat_harm(family = "typo"), "arg")
  expect_error(combat_harm(family = "combatls", predict = TRUE), "Saved-model")
  expect_error(combat_harm(family = "combatls", eb_check = TRUE), "eb_check")
  expect_error(combat_harm(family = "combatls", type = "lmer"), "uses GAMLSS")
  expect_error(combat_harm(family = "combatls", smooth_int_type = "tensor"), "does not support")
  expect_error(combat_harm(sigma.formula = ~ 1), "requires family")
  expect_error(.combatls_adapter(iris[1:2], iris$Species, iris[3:4], y ~ Petal.Width,
                                 y ~ Petal.Width, NULL, FALSE), "one-sided")
  expect_error(.combatls_adapter(iris[1:2], iris$Species, iris[3:4], y ~ Petal.Width,
                                 ~ missing_column, NULL, FALSE), "listed in covariates")
})

test_that("single-feature inputs retain their shape", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  fit <- combat_harm(df = iris, features = names(iris)[1L], batch = "Species",
                     family = "combatls", formula = y ~ 1,
                     control = gamlss::gamlss.control(trace = FALSE))
  expect_equal(dim(fit$harmonized_df), dim(iris))
  expect_false(fit$combat.object$eb)
  explicit <- combat_harm(df = iris, features = names(iris)[1L], batch = "Species",
                           family = "combatls", eb = FALSE, formula = y ~ 1,
                           control = gamlss::gamlss.control(trace = FALSE))
  expect_equal(fit$harmonized_df, explicit$harmonized_df)

})

test_that("missing optional backend gives actionable installation advice", {
  local_mocked_bindings(.require_combatfamily = function() {
    stop("ComBatLS requires ComBatFamily >= 0.2.2. Run check_dependencies(install = TRUE).")
  })
  expect_error(combat_harm(family = "combatls", formula = y ~ 1), "check_dependencies", fixed = TRUE)
  # Other families remain usable without the optional backend.
  fit <- combat_harm(df = iris, features = names(iris)[1:2], batch = "Species", eb = FALSE)
  expect_identical(fit$com_family, "comfam")
})

test_that("ComBatLS rejects unsupported formulas without rewriting them", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  args <- list(df = iris, features = names(iris)[1:2], batch = "Species",
               covariates = names(iris)[3:4], family = "combatls", eb = FALSE)
  expect_error(do.call(combat_harm, c(args, list(type = "gam", smooth = "Petal.Length"))), "specify GAMLSS effects directly")
  expect_error(do.call(combat_harm, c(args, list(formula = y ~ s(Petal.Length)))), "Unsupported mgcv smoother")
  expect_error(do.call(combat_harm, c(args, list(formula = y ~ Petal.Width, sigma.formula = ~ mgcv::s(Petal.Length)))), "Unsupported mgcv smoother")
  expect_error(do.call(combat_harm, c(args, list(formula = ~ Petal.Length))), "two-sided")
  expect_error(do.call(combat_harm, c(args, list(formula = response ~ Petal.Length))), "response")
  expect_error(do.call(combat_harm, c(args, list(formula = y ~ missing_column))), "listed in covariates")
  expect_error(do.call(combat_harm, c(args, list(formula = y ~ Petal.Width, sigma.formula = NULL))), "one-sided")
  expect_error(do.call(combat_harm, c(args, list(formula = y ~ Petal.Length, interaction = "Petal.Length,Petal.Width"))), "does not support")
  expect_error(combat_harm(formula = y ~ Petal.Length), "requires family")
})

test_that("GAMLSS formula helpers retain the caller's scope and terms", {
  local_transform <- function(x) x^2
  location <- y ~ pb(Petal.Length) + local_transform(Petal.Width)
  prepared <- .combatls_formula(location, iris[3:4], location = TRUE)
  expect_identical(prepared[[3L]], location[[3L]])
  expect_identical(get("pb", envir = environment(prepared)), gamlss::pb)
  expect_identical(get("local_transform", envir = environment(prepared)), local_transform)
  # A variable named s is not a call to mgcv's s().
  expect_no_error(.combatls_formula(y ~ s, data.frame(s = 1:10), location = TRUE))
})

test_that("ComBatLS requires its formula and rejects legacy modeling arguments", {
  expect_error(combat_harm(family = "combatls"), "requires an explicit location formula")
  expect_error(combat_harm(family = "combatls", sigma.formula = ~ 1), "requires an explicit location formula")
  for (legacy in list(list(smooth = "AGE"), list(interaction = "AGE,SEX"),
                      list(random = "subject"), list(smooth_int_type = "linear"))) {
    expect_error(do.call(combat_harm, c(list(family = "combatls", formula = y ~ 1), legacy)),
                 "does not support")
    expect_error(do.call(combat_harm, c(list(family = "combatls"), legacy)),
                 "does not support")
  }
  expect_error(.combatls_adapter(iris[1:2], iris$Species, iris[3:4], NULL, ~ 1, NULL, FALSE),
               "two-sided")
})

test_that("ComBatLS validates formula structure before data or backend loading", {
  local_mocked_bindings(.require_combatfamily = function() stop("backend should not load"))
  for (invalid in list(~ AGE + SEX, "y ~ AGE", AGE ~ SEX)) {
    expect_error(combat_harm(family = "combatls", formula = invalid), "formula must")
  }
  expect_error(combat_harm(family = "combatls", formula = y ~ AGE,
                           sigma.formula = y ~ AGE), "one-sided")
})

test_that("ComBatLS forwards robust location and scale explicitly", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  fit <- combat_harm(df = iris, features = names(iris)[1:2], batch = "Species",
                     covariates = names(iris)[3:4], family = "combatls",
                     formula = y ~ Petal.Length + Petal.Width,
                     sigma.formula = ~ Petal.Length, robust.LS = TRUE, eb = FALSE,
                     control = gamlss::gamlss.control(trace = FALSE))
  direct <- ComBatFamily::comfam(
    iris[1:2], iris$Species, covar = iris[3:4],
    model = gamlss::gamlss, family = gamlss.dist::NO(),
    formula = y ~ Petal.Length + Petal.Width, sigma.formula = ~ Petal.Length,
    robust.LS = TRUE, eb = FALSE, control = gamlss::gamlss.control(trace = FALSE))
  expect_equal(as.matrix(fit$harmonized_df[1:2]), direct$dat.combat, tolerance = 1e-8)
})

test_that("separate reference data preserves formula scope and matches ComBatFamily", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  set.seed(917)
  data <- data.frame(id = seq_len(240),
                     site = rep(c("old_site", "site_a", "site_b"), each = 80),
                     AGE = rnorm(240), SEX = rep(c("F", "M"), 120))
  shift <- rep(c(0, 2, -1), each = 80)
  data$feature1 <- 3 + data$AGE + shift + rnorm(240) * exp(0.1 * data$AGE)
  data$feature2 <- 5 + 0.5 * data$AGE + shift + rnorm(240) * exp(0.2 * data$AGE)
  reference_data <- data[1:80, ]
  new_data <- data[81:240, ]
  new_data$AGE[3] <- NA_real_
  kept <- new_data[stats::complete.cases(new_data), ]
  local_center <- function(x) x - 0.25
  location <- y ~ local_center(AGE) + SEX
  scale <- ~ AGE + SEX
  args <- list(features = c("feature1", "feature2"), batch = "site",
               covariates = c("AGE", "SEX"), df = new_data,
               reference = reference_data, family = "combatls",
               formula = location, sigma.formula = scale, eb = FALSE,
               out_ref_include = FALSE, control = gamlss::gamlss.control(trace = FALSE))
  fit <- do.call(combat_harm, args)

  ref <- reference_data
  ref$site <- "reference"
  combined <- rbind(ref, kept)
  combined$site <- factor(combined$site)
  combined$SEX <- factor(combined$SEX)
  direct <- ComBatFamily::comfam(
    combined[args$features], combined$site, combined[c("AGE", "SEX")],
    model = gamlss::gamlss, family = gamlss.dist::NO(),
    formula = location, sigma.formula = scale, ref.batch = "reference",
    eb = FALSE, control = gamlss::gamlss.control(trace = FALSE))
  expect_equal(unname(as.matrix(fit$harmonized_df[args$features])),
                unname(direct$dat.combat[80 + seq_len(nrow(kept)), , drop = FALSE]),
                tolerance = 1e-8)
  expect_identical(fit$harmonized_df$id, kept$id)
  expect_s3_class(fit$harmonized_df$SEX, "factor")
  expect_identical(fit$combat.object$ComBat.model$batch.info$ref.batch, "reference")
  expect_equal(unname(direct$dat.combat[1:80, ]),
                unname(as.matrix(reference_data[args$features])))

  args$out_ref_include <- TRUE
  included <- do.call(combat_harm, args)
  expect_identical(included$harmonized_df$id, c(reference_data$id, kept$id))
  expect_equal(unname(as.matrix(included$harmonized_df[args$features])),
                unname(direct$dat.combat), tolerance = 1e-8)
  expect_identical(reference_data, data[1:80, ])
})
