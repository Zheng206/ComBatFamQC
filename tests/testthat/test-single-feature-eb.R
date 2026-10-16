test_that("single-feature ComBat and CovBat record the automatic EB fallback", {
  for (family in c("comfam", "covfam")) {
    args <- list(df = iris, features = "Sepal.Length", batch = "Species",
                 covariates = c("Petal.Length", "Petal.Width"), family = family)
    automatic <- do.call(combat_harm, args)
    explicit <- do.call(combat_harm, c(args, list(eb = FALSE)))
    expect_false(automatic$combat.object$eb)
    expect_equal(automatic$harmonized_df, explicit$harmonized_df)

    args$df <- iris[51:150, ]
    args$reference <- iris[1:50, ]
    args$out_ref_include <- FALSE
    automatic_ref <- do.call(combat_harm, args)
    explicit_ref <- do.call(combat_harm, c(args, list(eb = FALSE)))
    expect_false(automatic_ref$combat.object$eb)
    expect_equal(automatic_ref$harmonized_df, explicit_ref$harmonized_df)
  }
})

test_that("EB fallback uses the feature count after constant features are removed", {
  data <- iris
  data$constant <- 1
  fit <- combat_harm(df = data, features = c("Sepal.Length", "constant"),
                     batch = "Species", covariates = "Petal.Length")
  expect_false(fit$combat.object$eb)
  expect_equal(ncol(fit$combat.object$ComBat.model$dat.combat), 1L)
})

test_that("prediction metadata reflects the saved setting and single-feature fallback", {
  for (family in c("comfam", "covfam")) {
    fit <- combat_harm(df = iris, features = "Sepal.Length", batch = "Species",
                       covariates = "Petal.Length", family = family)
    legacy <- fit$combat.object
    legacy$eb <- TRUE
    predicted <- combat_harm(df = iris[1:20, ], predict = TRUE, object = legacy)
    expected <- combat_harm(df = iris[1:20, ], predict = TRUE, object = fit$combat.object)
    expect_false(predicted$combat.object$eb)
    expect_equal(predicted$harmonized_df, expected$harmonized_df)
  }
  multi <- combat_harm(df = iris, features = names(iris)[1:2], batch = "Species",
                       covariates = "Petal.Length", eb = FALSE)
  predicted <- combat_harm(df = iris[1:20, ], predict = TRUE, object = multi$combat.object)
  expect_false(predicted$combat.object$eb)
  enabled <- combat_harm(df = iris, features = names(iris)[1:2], batch = "Species",
                         covariates = "Petal.Length")
  expect_true(enabled$combat.object$eb)
})
