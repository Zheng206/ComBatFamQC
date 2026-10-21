shiny_combatls_fixture <- function() {
  result <- readRDS(testthat::test_path("previous-results/lm_result.rds"))
  result$info$df <- utils::head(result$info$df, 180)
  result
}

test_that("the original Shiny harmonization controls remain when ComBatFamily is unavailable", {
  local_mocked_bindings(.combatfamily_available = function() FALSE)
  app <- comfam_shiny(shiny_combatls_fixture())
  page <- app$httpHandler(list(REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = ""))$content
  expect_match(page, 'value="comfam"', fixed = TRUE)
  expect_match(page, 'value="covfam"', fixed = TRUE)
  expect_false(grepl('value="combatls"', page, fixed = TRUE))
  expect_false(grepl('id="combatls_formula"', page, fixed = TRUE))
  expect_false(grepl('id="scale_model_check"', page, fixed = TRUE))
  for (id in c("com_model", "interaction", "eb_check_button"))
    expect_match(page, paste0('id="', id, '"'), fixed = TRUE)
})

test_that("Shiny adds ComBatLS formula controls only with an available backend", {
  local_mocked_bindings(.combatfamily_available = function() TRUE)
  app <- comfam_shiny(shiny_combatls_fixture())
  page <- app$httpHandler(list(REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = ""))$content
  expect_match(page, 'value="combatls"', fixed = TRUE)
  expect_match(page, 'id="combatls_formula"', fixed = TRUE)
  expect_match(page, 'id="combatls_sigma_formula"', fixed = TRUE)
  expect_match(page, 'id="scale_model_check"', fixed = TRUE)
  expect_match(page, "Covariate screening", fixed = TRUE)
  expect_match(page, "Centile curves", fixed = TRUE)
  for (id in c("scale_centile_feature", "scale_centile_model"))
    expect_length(gregexpr(paste0('id="', id, '"'), page, fixed = TRUE)[[1L]], 1L)
  for (id in c("scale_feature", "scale_fit_source", "scale_effect_summary", "scale_effects",
               "scale_fit_feature", "scale_fit_model", "scale_qq", "scale_residuals", "scale_convergence"))
    expect_false(grepl(paste0('id="', id, '"'), page, fixed = TRUE))
  expect_match(page, 'height:650px', fixed = TRUE)
  expect_match(page, "Biological Scale Summary", fixed = TRUE)
  expect_match(page, "View feature-wise results", fixed = TRUE)
  expect_match(page, 'id="scale_table_covariate"', fixed = TRUE)
  expect_match(page, 'id="scale_significant_only"', fixed = TRUE)
  expect_false(grepl('>Model fit<', page, fixed = TRUE))
  expect_match(page, "do not establish model adequacy", fixed = TRUE)
  expect_match(page, 'id="scale_check_status"', fixed = TRUE)
  expect_match(page, 'id="scale_show_points"', fixed = TRUE)
  expect_match(page, 'id="scale_variability"', fixed = TRUE)
  expect_match(page, 'value="AGE" selected', fixed = TRUE)
  expect_match(page, 'value="~ 1"', fixed = TRUE)
  expect_match(page, "input.com_type", fixed = TRUE)
})

test_that("Shiny fits and saves ComBatLS while ignoring hidden legacy inputs", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  result <- shiny_combatls_fixture()
  data_path <- tempfile(fileext = ".csv")
  model_path <- tempfile(fileext = ".rds")
  testServer(comfam_shiny(result), {
    session$setInputs(com_type = "combatls", com_model = "lmer", random = "stale_random",
                      smooth = "stale_smooth", interaction = "stale*interaction",
                      eb_control = "No", ref_bat_select = "None",
                      combatls_formula = "y ~ AGE + SEX", combatls_sigma_formula = "~ AGE + SEX",
                      combatls_robust = TRUE,
                      save_path = data_path, model_save_path = model_path)
    session$setInputs(ComBat = 1)
    fitted <- combat_result_s()
    expect_identical(fitted$com_family, "combatls")
    expect_false(fitted$combat.object$eb)
    expect_s3_class(fitted$combat.object$ComBat.model$fits[[1L]], "gamlss")
    expect_equal(nrow(utils::read.csv(data_path)), nrow(fitted$harmonized_df))
    expect_null(output$smooth_select)
    expect_null(output$random_select)
    session$setInputs(eb_check_button = 1)
    expect_identical(combat_result_s(), fitted)
    session$setInputs(ComBat_model = 1)
    saved <- readRDS(model_path)
    expect_identical(saved$family, "combatls")
    expect_null(saved$ComBat.model$dat.combat)
    expect_false(is.null(combat_result_s()$combat.object$ComBat.model$dat.combat))
    # A blank optional scale formula uses the constant-scale default.
    session$setInputs(combatls_sigma_formula = "", ComBat = 2)
    expect_length(all.vars(combat_result_s()$combat.object$ComBat.model$fits[[1L]]$sigma.formula), 0L)
    # Invalid input clears the pending result and does not overwrite saved data.
    previous <- readLines(data_path)
    session$setInputs(combatls_formula = "~ AGE", ComBat = 3)
    expect_null(combat_result_s())
    expect_identical(readLines(data_path), previous)
    session$setInputs(combatls_formula = "", ComBat = 4)
    expect_null(combat_result_s())
    session$setInputs(com_type = "comfam", com_model = "lm", interaction = "", ComBat = 5)
    expect_identical(combat_result_s()$com_family, "comfam")
  })
})

test_that("optional backend availability handles namespace or version failures", {
  local_mocked_bindings(.require_combatfamily = function() stop("missing or incompatible"))
  expect_false(.combatfamily_available())
})

test_that("Shiny renders smooth centiles and SD comparisons from cached fits", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  result <- shiny_combatls_fixture()
  result$info$features <- result$info$features[1L]
  run_fit <- .combatls_exploratory_fit
  fit_calls <- 0L
  local_mocked_bindings(.combatls_exploratory_fit = function(...) {
    fit_calls <<- fit_calls + 1L
    run_fit(...)
  })
  testServer(comfam_shiny(result), {
    session$setInputs(com_type = "combatls", combatls_formula = "y ~ pb(AGE) + SEX",
                      combatls_sigma_formula = "~ 1", scale_candidates = c("AGE", "SEX"),
                      scale_screen_formula = "~ pb(AGE) + SEX", ref_bat_select = "None",
                      scale_centile_feature = result$info$features[1L], scale_curve_x = "AGE",
                      scale_curve_group = "SEX", scale_compare_centiles = TRUE)
    session$setInputs(scale_model_check = 1)
    saved <- scale_check_s()
    expect_true(all(saved$diagnostics$valid))
    saved_fit_calls <- fit_calls
    expected <- combatls_check_plot(saved, "centiles", x = "AGE", group = "SEX", model = "both")
    expect_equal(scale_centile_plot_cached()$data, expected$data)
    expect_equal(scale_variability(), .combatls_centile_variability(saved, saved$features[1L],
      "AGE", "SEX", model = "both"))
    expect_true(all(is.finite(scale_variability()$sd_change_pct)))
    expect_true(nzchar(output$scale_centiles$src))
    session$setInputs(scale_curve_group = "None", scale_compare_centiles = FALSE,
                      scale_centile_model = "screening")
    expect_true(nzchar(output$scale_centiles$src))
    expect_identical(unique(scale_centile_plot_cached()$data$model), "screening")
    expect_identical(scale_check_s(), saved)
    expect_identical(fit_calls, saved_fit_calls)
  })
})

test_that("Shiny caches model checks, detects stale formulas, and only applies drafts on request", {
  suppressWarnings(skip_if_not_installed("ComBatFamily", "0.2.2"))
  result <- shiny_combatls_fixture()
  run_check <- combatls_model_check
  run_plot <- combatls_check_plot
  plotted <- list()
  last_plot <- function(type) tail(Filter(function(call) identical(call$type, type), plotted), 1L)[[1L]]
  check_calls <- 0L
  local_mocked_bindings(combatls_model_check = function(...) {
    check_calls <<- check_calls + 1L
    run_check(...)
  }, combat_harm = function(...) stop("A model check must not harmonize data"),
  combatls_check_plot = function(check, type, feature = check$features[1L], ..., model = "proposed") {
    plotted[[length(plotted) + 1L]] <<- list(type = type, feature = feature, model = model)
    run_plot(check, type, feature, ..., model = model)
  })
  testServer(comfam_shiny(result), {
    session$setInputs(com_type = "combatls", combatls_formula = "y ~ AGE + SEX",
                      combatls_sigma_formula = "~ 1", scale_candidates = c("AGE", "SEX"),
                      scale_screen_formula = "", ref_bat_select = "None",
                      scale_centile_feature = result$info$features[1L], scale_curve_x = "AGE",
                      scale_curve_group = "SEX", scale_centile_model = "proposed")
    expect_false(scale_check_ready())
    expect_false(output$scale_check_ready)
    expect_match(output$scale_check_prompt$html, "Run Model Check to generate")
    expect_error(current_scale_check(), class = "shiny.silent.error")
    session$setInputs(scale_model_check = 1)
    saved <- scale_check_s()
    expect_true(scale_check_ready())
    expect_true(output$scale_check_ready)
    expect_null(output$scale_check_prompt)
    expect_s3_class(saved, "combatls_check")
    expect_identical(check_calls, 1L)
    expect_null(combat_result_s())
    expect_identical(input$combatls_sigma_formula, "~ 1")
    expect_setequal(all.vars(saved$screen.formula), c("AGE", "SEX"))
    expect_match(output$scale_check_status, "Fitting status")
    expect_match(output$scale_check_status, "fits reported warnings or errors")
    expect_match(output$scale_formula_suggestion, "Liberal screening draft")
    session$setInputs(scale_compare_centiles = TRUE)
    invisible(output$scale_centiles)
    expect_equal(nrow(scale_variability()), 2L * nlevels(saved$data$SEX))
    predictions <- scale_variability()
    plot_count <- length(plotted)
    session$setInputs(scale_show_points = TRUE)
    invisible(output$scale_centiles)
    expect_identical(length(plotted), plot_count)
    expect_identical(scale_variability(), predictions)
    session$setInputs(scale_show_points = FALSE)
    session$setInputs(scale_compare_centiles = FALSE)
    invisible(output$scale_centiles)
    expect_identical(last_plot("centiles")$feature, result$info$features[1L])
    expect_identical(last_plot("centiles")$model, "proposed")
    session$setInputs(scale_centile_model = "screening",
                      scale_centile_feature = result$info$features[2L])
    invisible(output$scale_centiles)
    expect_identical(last_plot("centiles")$model, "screening")
    expect_identical(last_plot("centiles")$feature, result$info$features[2L])
    all_rows <- scale_association_rows()
    session$setInputs(scale_table_covariate = "AGE")
    expect_true(all(scale_association_rows()[["Biological covariate"]] == "AGE"))
    session$setInputs(scale_significant_only = TRUE)
    expect_true(all(scale_association_rows()$.significant))
    session$setInputs(scale_table_covariate = "", scale_significant_only = FALSE)
    expect_identical(scale_association_rows(), all_rows)
    expect_identical(scale_check_s(), saved)
    expect_identical(check_calls, 1L)

    # Exercise manual acceptance independently of the sample's particular p-values.
    draft <- scale_check_s()
    draft$suggested.formula <- ~ AGE + SEX
    scale_check_s(draft)
    updates <- list()
    old_sender <- session$sendInputMessage
    session$sendInputMessage <- function(inputId, message) updates[[inputId]] <<- message
    session$setInputs(scale_accept_suggestion = 1)
    session$sendInputMessage <- old_sender
    expect_identical(updates$combatls_sigma_formula$value, "~AGE + SEX")
    expect_identical(input$combatls_sigma_formula, "~ 1")
    session$setInputs(combatls_sigma_formula = "~ AGE + SEX")
    expect_false(scale_check_ready())
    expect_false(output$scale_check_ready)
    expect_match(output$scale_check_prompt$html, "Model settings have changed")
    expect_error(current_scale_check(), class = "shiny.silent.error")
    expect_identical(check_calls, 1L)
    session$setInputs(scale_model_check = 2)
    expect_identical(check_calls, 2L)
    expect_true(output$scale_check_ready)
    expect_null(output$scale_check_prompt)
    session$setInputs(com_type = "comfam")
    expect_false(output$scale_check_ready)
    session$setInputs(com_type = "combatls")
    expect_true(output$scale_check_ready)
    expect_identical(check_calls, 2L)
    expect_setequal(all.vars(current_scale_check()$sigma.formula), c("AGE", "SEX"))
    expect_null(combat_result_s())
  })
})
