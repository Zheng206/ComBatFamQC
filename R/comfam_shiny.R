#' Batch Effect Interactive Visualization
#'
#' Provides an interactive visualization of batch or site effects using a Shiny application.
#'
#' @param result A list derived from `visual_prep()` that contains datasets and statistical test results for Shiny visualization.
#' @param after A boolean variable indicating whether the batch effect diagnostic occurs before or after harmonization (default: `FALSE`).
#'
#' @import ggplot2
#' @import shiny
#' @import shinydashboard
#' @import bslib
#' @importFrom DT datatable formatStyle styleEqual DTOutput renderDT
#' @importFrom stats reorder
#' @importFrom utils write.csv
#'
#' @return This function does not return a value. It launches a Shiny app.
#'
#' @export
#'
#' @details
#' When this function is called, it starts a Shiny application in the
#' user's default web browser. Execution is blocked until the app is closed.
#' Feature-level batch-effect tables can be filtered to Bonferroni-adjusted
#' p-values below 0.05; the reported percentages still use all features.
#' The Covariance Diagnostics tab compares prepared residual covariance and
#' correlation across batches before harmonization, using `covariance_batch_check()`
#' and `covariance_check_plot()`. These exploratory summaries inform review of CovBat.
#' Sidebar controls include a feature-selection dialog. Displayed batches filter
#' the matrices and distance tables while retaining the pooled reference from all
#' eligible batches. Display changes reuse the check; feature or matrix changes
#' require rerunning it. Frobenius distance is an optional table column.
#' When a compatible optional ComBatFamily backend is available, the harmonization
#' tab also offers ComBatLS with an explicit location formula, an optional scale
#' formula (default `~ 1`), and robust estimation. Legacy model controls and the
#' EB assumption diagnostic are replaced by a pre-harmonization Scale Model Check:
#' candidate screening and model-implied centiles, with a compact fitting status.
#' Centiles include an aligned observation-count strip. A collapsed fit check
#' compares saved normalized residuals, binned RMS, and Q-Q plots for both models;
#' these exploratory diagnostics use the fitting data and never refit models.
#' Residual diagnostics remain available through `combatls_check_plot()` in R.
#' Fits are cached until Run Model Check is clicked again; edits mark results stale.
#' Scale-formula drafts are applied only on explicit user acceptance. Otherwise, only the original
#' ComBat/CovBat interface is shown.
#'
#' @examples
#' result_lm <- visual_prep(type = "lm", features = colnames(adni)[43:53],
#' batch = "manufac", covariates = c("AGE", "SEX", "DIAGNOSIS"),
#' df = head(adni, 500), cores = 1)
#' if (interactive()) {
#'   comfam_shiny(result = result_lm)
#' }


comfam_shiny <- function(result, after = FALSE){
  info <- result$info
  type <- info$type
  df <- info$df
  batch <- info$batch
  features <- info$features
  covariates <- info$cov_shiny
  char_var <- info$char_var
  num_var <- setdiff(covariates, char_var)
  numeric_covariates <- covariates[vapply(df[covariates], is.numeric, logical(1))]
  age_covariates <- c(numeric_covariates[tolower(numeric_covariates) == "age"],
    numeric_covariates[grepl("^age_", numeric_covariates, ignore.case = TRUE)])
  default_curve_x <- if (length(age_covariates)) age_covariates[1L] else
    if (length(numeric_covariates)) numeric_covariates[1L] else character()
  combatls_available <- .combatfamily_available()
  harmonization_families <- c("comfam", "covfam", if (combatls_available) "combatls")
  legacy_controls <- function(...) {
    if (combatls_available) conditionalPanel("input.com_type != 'combatls'", ...)
    else tagList(...)
  }

  ## UI Design
  ui <- function(request) {
    fluidPage(
      theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
      tags$head(tags$style(HTML("
        #combatqc-sidebar .row { margin-left: 0; margin-right: 0; }
        #combatqc-sidebar .box,
        #combatqc-sidebar .shiny-input-container {
          width: 100% !important;
          max-width: 100%;
          min-width: 0;
          box-sizing: border-box;
        }
        #combatqc-sidebar .form-control,
        #combatqc-sidebar .selectize-control,
        #combatqc-sidebar .selectize-input {
          max-width: 100%;
          min-width: 0;
          box-sizing: border-box;
        }
        #combatqc-sidebar .box-body,
        #combatqc-sidebar .help-block,
        #combatqc-sidebar label,
        #combatqc-sidebar .shiny-text-output {
          overflow-wrap: anywhere;
        }
        #combatqc-sidebar pre {
          max-width: 100%;
          white-space: pre-wrap;
          overflow-wrap: anywhere;
          word-break: normal;
        }
        #combatqc-sidebar .btn {
          max-width: 100%;
          white-space: normal;
          overflow-wrap: anywhere;
        }
        #combatqc-main { min-width: 0; }
        #scale_compare_centiles + span { white-space: nowrap; }
        #combatqc-sidebar .help-block,
        .combatls-centile-view .help-block {
          margin: 1rem 0;
          line-height: 1.55;
        }
        .combatls-centile-view .combatls-centile-profile {
          margin: 1rem 0;
          font-weight: 600;
          white-space: nowrap;
          overflow-x: auto;
        }
        .combatls-centile-view details { margin: 1.25rem 0; }
        .combatls-check-note,
        .combatls-screen-interpretation p {
          margin: 1rem 0;
          line-height: 1.55;
        }
        #covariance_feature_choices {
          max-height: 45vh;
          overflow-y: auto;
          padding: 0.75rem 1.75rem;
          box-sizing: border-box;
        }
        #covariance_feature_choices .checkbox { margin: 0.5rem 0; }
        #covariance_feature_choices label { overflow-wrap: anywhere; }
        .covariance-feature-actions { display: flex; gap: 0.5rem; flex-wrap: wrap; margin: 0.75rem 0; }
        .covariance-check-actions { display: flex; gap: 0.5rem; flex-wrap: wrap; margin-top: 1rem; }
        .covariance-check-note { margin: 0 0 0.75rem; }
        .covariance-status {
          margin: 1rem 0 1.25rem;
          color: #3c4858;
          font-size: 1.05rem;
          font-weight: 600;
          letter-spacing: 0.01em;
        }
        #combatqc-main .datatables {
          max-width: 100%;
          overflow-x: auto;
        }
        #combatqc-main .combatqc-check-output {
          display: flow-root;
          clear: both;
          margin: 1rem 0;
        }
        #combatqc-main .combatqc-check-output .shiny-output-error {
          height: auto !important;
          min-height: 3rem;
          padding: 0.5rem 0;
          white-space: normal;
          overflow-wrap: anywhere;
          line-height: 1.5;
        }
      ")), tags$script(HTML("
        document.addEventListener('input', function(event) {
          if (event.target.id !== 'covariance_feature_search') return;
          var list = document.getElementById('covariance_feature_choices');
          if (!list) return;
          var query = event.target.value.trim().toLowerCase();
          list.querySelectorAll('.checkbox').forEach(function(item) {
            item.style.display = item.textContent.toLowerCase().indexOf(query) >= 0 ? '' : 'none';
          });
        });
      "))),
      titlePanel("Batch Effect Diagnostics"),
      sidebarLayout(
        sidebarPanel(
          id = "combatqc-sidebar",
          conditionalPanel(condition="input.tabselected==2",
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "Data Summary",
                               radioButtons("type", "Select output type", choices = c("Plot", "Table"), selected = "Plot"),
                               uiOutput("cov_status"),
                               uiOutput("plot_text_status")
                             )
                           )
          ),
          conditionalPanel(condition="input.tabselected==3",
                           selectInput("feature", "Select Feature", choices = features, selected = features[1]),
                           radioButtons("resid_all", "Select whether to include all batch levels", choices = c("Yes", "No"), selected = "Yes"),
                           uiOutput("resid_all_control"),
                           radioButtons("resid_color", "Select whether to color by batch variable", choices = c("Yes", "No"), selected = "No"),
                           radioButtons("resid_label", "Select whether to include labels for the x axis.", choices = c("Yes", "No"), selected = "No"),
                           uiOutput("resid_label_control")
          ),
          conditionalPanel(condition="input.tabselected==4",
                           fluidRow(
                             shinydashboard::box(
                               width = 12,
                               title = "Selection of Principal Components ",
                               selectInput("PC1", "Select the first PC", choices = colnames(result$pr.feature$x), selected = colnames(result$pr.feature$x)[1]),
                               selectInput("PC2", "Select the second PC", choices = colnames(result$pr.feature$x), selected = colnames(result$pr.feature$x)[2]),
                               radioButtons("pca_label", "Select whether to show the legend of plots.", choices = c("Yes", "No"), selected = "No"),
                               radioButtons("pca_all", "Select whether to include all batch levels", choices = c("Yes", "No"), selected = "Yes"),
                               uiOutput("pca_all_control"))),
                           fluidRow(
                             shinydashboard::box(
                               width = 12,
                               title = "Variance Explained",
                               style = "background-color: white;",
                               DT::DTOutput("pc_variance"))),
                           fluidRow(
                             shinydashboard::box(
                               width = 12,
                               title = "MDMR",
                               style = "background-color: white;",
                               uiOutput("mdmr_control")))

          ),
          conditionalPanel(condition="input.tabselected==6",
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "Harmonization Setup",
                               radioButtons("com_type", "Select ComBatFam type", choices = harmonization_families, selected = "comfam"),
                               uiOutput("com_type_note"),
                               legacy_controls(radioButtons("com_model", "Select ComBat Family model to be used", choices = c("lm", "lmer", "gam"), selected = type),
                               uiOutput("com_model_note")),
                               radioButtons("eb_control", "Select whether the Empirical Bayes (EB) method should be used", choices = c("Yes", "No"), selected = "Yes"),
                               legacy_controls(uiOutput("smooth_select"),
                                               uiOutput("random_select")),
                               selectInput("ref_bat_select", "Select the reference batch", choices = c("None", levels(df[[batch]])), selected = "None"),
                               legacy_controls(textInput("interaction", "Enter the potential interaction terms:", value = paste0(gsub("\\,", "\\*", info$interaction_orig), collapse = ",")),
                               uiOutput("interaction_note"),
                               uiOutput("smooth_int_type_control"),
                               uiOutput("smooth_int_type_control_note")),
                               if (combatls_available) conditionalPanel(
                                 "input.com_type == 'combatls'",
                                 textInput("combatls_formula", "Location formula (required)",
                                           value = "", placeholder = "y ~ pb(AGE) + SEX"),
                                 helpText("Use y as the location response. Examples: y ~ AGE + SEX; y ~ pb(AGE) + SEX."),
                                 textInput("combatls_sigma_formula", "Scale formula", value = "~ 1"),
                                 helpText("Scale examples: ~ 1; ~ AGE + SEX."),
                                 checkboxInput("combatls_robust", "Use robust location and scale estimates", value = FALSE),
                                 helpText(paste("Available covariates:", if (length(covariates)) paste(covariates, collapse = ", ") else "none (use y ~ 1 and ~ 1)."))
                               )
                             )
                           ),
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "Harmonization",
                               legacy_controls(uiOutput("eb_check_description"),
                                               actionButton("eb_check_button", "EB Check"),
                                               uiOutput("non_eb"),
                                               selectInput("batch_selection", "Select the batches to be shown on the graph", choices = c("All", levels(info$df[[batch]])), selected = "All")),
                               if (combatls_available) conditionalPanel(
                                 "input.com_type == 'combatls'",
                                 h4("Scale Model Check"),
                                 checkboxGroupInput("scale_candidates", "Candidate biological scale covariates",
                                                    choices = covariates, selected = covariates),
                                 textInput("scale_screen_formula", "Exploratory scale formula (optional)",
                                           placeholder = "~ pb(AGE) + SEX"),
                                 helpText("If blank, the screening model includes all selected candidates linearly, even when the proposed scale formula is ~ 1. Batch effects enter location and scale."),
                                 actionButton("scale_model_check", "Run Model Check"),
                                 conditionalPanel(
                                   "output.scale_check_ready == true",
                                   verbatimTextOutput("scale_formula_suggestion"),
                                   actionButton("scale_accept_suggestion", "Use suggested scale formula")
                                 ),
                                 helpText("Screening informs review; biological knowledge may justify retaining nonsignificant covariates. The proposed scale formula changes only when you accept or edit it.")
                               ),
                               textInput("save_path", "Save harmonized dataframe to:"),
                               actionButton("ComBat", "Harmonize and Save Data"),
                               verbatimTextOutput("output_msg"),
                               textInput("model_save_path", "Save ComBat Model to:"),
                               actionButton("ComBat_model", "Save ComBat Model"),
                               verbatimTextOutput("output_msg_model")
                             )
                           )

          ),
          conditionalPanel(condition="input.tabselected==5",
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "Additive Batch Effect",
                               if(type == "lmer"){
                                 radioButtons("test_batch", "Type of Statistical Tests for Additive Batch Effect", choices = c("ANOVA", "Kruskal-Wallis", "Kenward-Roger (liner mixed model)"), selected = "ANOVA")
                               }else{
                                 radioButtons("test_batch", "Type of Statistical Tests for Additive Batch Effect", choices = c("ANOVA", "Kruskal-Wallis"), selected = "ANOVA")
                               },
                               uiOutput("test_batch_explain"))),
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "Multiplicative Batch Effect",
                               radioButtons("test_variance", "Type of Statistical Tests for Multiplicative Batch Effect", choices = c("Fligner-Killeen", "Levene's Test", "Bartlett's Test"), selected = "Fligner-Killeen"),
                               uiOutput("test_variance_explain"))),
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               uiOutput("mul_adjustment"))),
                           fluidRow(
                             shinydashboard::box(
                               title = "Diagnosis Results Export",
                               width = NULL,
                               textInput("diag_save_path", "Save diagnosis result to:"),
                               actionButton("Diagnosis", "Export Diagnosis Result"),
                               verbatimTextOutput("output_msg_diag")))
          ),
          conditionalPanel(condition="input.tabselected==7",
                           fluidRow(shinydashboard::box(width = NULL, title = "Covariance check",
                             p("Compare feature relationships across batches using covariate-adjusted residuals."),
                             radioButtons("covariance_matrix", "Matrix type",
                               choices = c("Correlation" = "correlation", "Covariance (pooled SD units)" = "covariance"),
                               selected = "correlation"),
                             tags$details(tags$summary(tags$strong("About these matrices")),
                               helpText("Correlation compares feature relationships after removing within-batch feature scale. Covariance also reflects differences in feature variances and is expressed in pooled within-batch SD units.")),
                             h5("Feature set"),
                             p(tags$strong(textOutput("covariance_feature_summary", inline = TRUE))),
                             div(class = "covariance-check-actions",
                               actionButton("covariance_change_features", "Change features"),
                               actionButton("covariance_run", "Run covariance check")),
                             helpText("Changes to the feature set or matrix type require rerunning the check.",
                               class = "covariance-check-note"),
                             uiOutput("covariance_prompt"))),
                           conditionalPanel("output.covariance_ready == true",
                             fluidRow(shinydashboard::box(width = NULL, title = "Covariance display",
                               checkboxGroupInput("covariance_plot_batches", "Displayed batches",
                                 choices = levels(df[[batch]]), selected = utils::head(levels(df[[batch]]), 4)),
                               radioButtons("covariance_view", "Matrix view",
                                 choices = c("Batch matrices" = "batch", "Differences from pooled matrix" = "difference"),
                                 selected = "batch"),
                               checkboxInput("covariance_show_labels", "Show feature names", value = FALSE),
                               sliderInput("covariance_pca_variance", "PCA variance explained target (%)",
                                 min = 1, max = 100, value = 90, step = 1),
                               helpText("Batch selection filters the matrices, PC spread heatmap, and both distance tables. Pooled references and the shared PC basis stay fixed across all eligible batches."))))
          ),
          conditionalPanel(condition="input.tabselected==1",
                           radioButtons("data_view", "Overview Type", choices = c("Complete Data", "Exploratory Analysis"), selected = "Complete Data"),
                           uiOutput("explore_bar")
          )),
        mainPanel(
          id = "combatqc-main",
          tabsetPanel(
            tabPanel("Data Overview", value = 1,
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Usage",
                         style = "background-color: #f0f0f0;",
                         shiny::htmlOutput("data_usage_explain"))),
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Data",
                         DT::DTOutput("data_frame"))),
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         uiOutput("data_ui")))),
            tabPanel("Summary", value = 2,
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Batch Sample Size Summary",
                         shiny::uiOutput("output"))),
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Covariate Distribution",
                         shiny::uiOutput("cov_output")))),
            tabPanel("Residual Plot", value = 3,
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Additive Batch Effect",
                         shiny::uiOutput("res_add_explain"),
                         shiny::plotOutput("res_add"))),
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Multiplicative Batch Effect",
                         shiny::uiOutput("res_ml_explain"),
                         shiny::plotOutput("res_ml")))),
            tabPanel("Diagnosis of Global Batch Effect", value = 4,
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "PCA",
                         shiny::plotOutput("pca"))),
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "T-SNE",
                         shiny::plotOutput("tsne")))),
            tabPanel("Diagnosis of Feature-level Batch Effect", value = 5,
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Additive Batch Effect Test",
                         checkboxInput("batch_significant_only",
                           "Show significant results only (Bonferroni-adjusted p < 0.05)",
                           value = FALSE, width = "100%"),
                         shiny::uiOutput("test_batch_ui"),
                         shiny::uiOutput("sig_pct_batch"))),
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Multiplicative Batch Effect Test",
                         checkboxInput("variance_significant_only",
                           "Show significant results only (Bonferroni-adjusted p < 0.05)",
                           value = FALSE, width = "100%"),
                         shiny::uiOutput("test_variance_ui"),
                         shiny::uiOutput("sig_pct_variance")))),
            tabPanel("Covariance Diagnostics", value = 7,
                     conditionalPanel("output.covariance_ready == true",
                       div(class = "covariance-status", textOutput("covariance_status")),
                       uiOutput("covariance_sampling_caution"),
                       fluidRow(shinydashboard::box(width = 12,
                         title = "Batch-specific matrices",
                         plotOutput("covariance_matrices", height = "auto"),
                         helpText(textOutput("covariance_display_status")),
                         helpText(textOutput("covariance_exclusions")))),
                       fluidRow(shinydashboard::box(width = 12,
                         title = "Batch \u00d7 PC spread",
                         textOutput("covariance_pca_status"),
                         plotOutput("covariance_pca", height = "auto"),
                         helpText("Each row is a batch and each column is a shared PC. A cell compares that batch\u2019s PC score SD with the pooled SD for the same PC: ",
                           tags$strong("1"), " means equal spread, ", tags$strong("1.13"),
                           " means 13% greater spread, and ", tags$strong("0.85"),
                           " means 15% less. Compare batches within a column to see where their spread differs. The plot covers only the PCs shown and does not test whether those differences are statistically significant."))),
                       fluidRow(shinydashboard::box(width = 12,
                         title = "Distances from the pooled matrix",
                         DT::DTOutput("covariance_sample_table"))),
                       fluidRow(shinydashboard::box(width = 12,
                         title = "Distances between batches",
                         DT::DTOutput("covariance_pairwise_table"))),
                       tags$details(tags$summary(tags$strong("How to assess CovBat")),
                         p("CovBat targets batch differences in cross-feature covariance that may remain after location and scale harmonization. Correlation differences highlight changes in feature relationships; covariance differences can also reflect feature variance differences."),
                         p("Both tables show off-diagonal RMS, spectral norm, largest absolute difference, and Frobenius distance. Off-diagonal RMS is the primary descriptive metric for feature relationships. Spectral norm captures the largest absolute change along a feature direction, and largest absolute difference identifies the most discrepant entry. Larger values indicate greater estimated differences, with no universal cutoff for choosing CovBat."),
                         tags$details(tags$summary(tags$strong("Distance metric formulas")), .covariance_metric_formulas()),
                         p("Method reference: ", tags$a("Chen et al., CovBat", href = "https://doi.org/10.1002/hbm.25688", target = "_blank", rel = "noopener noreferrer"))
                       ))),
            if(!after){
              tabPanel("Harmonization", value = 6,
                       if (combatls_available) conditionalPanel(
                         "input.com_type == 'combatls'",
                         h4("ComBatLS - Scale Model Check"),
                         p("Run Model Check before harmonization. These exploratory normal GAMLSS fits include batch effects in both location and scale; no harmonized data are generated. Plot changes reuse the saved fits."),
                         uiOutput("scale_check_prompt"),
                         conditionalPanel(
                           "output.scale_check_ready == true",
                           div(class = "combatls-check-note", helpText(textOutput("scale_check_status"))),
                           tabsetPanel(
                             tabPanel("Covariate screening",
                                      div(class = "combatls-check-note",
                                        helpText("Screen biological effects on variability before harmonization. Smooth-model tests are approximate; gray tiles indicate unavailable tests.")),
                                      checkboxInput("scale_heatmap_y_labels", "Show feature names on y-axis", value = TRUE),
                                      plotOutput("scale_screen_heatmap", height = "650px", width = "100%"),
                                      h4("Biological Scale Summary"),
                                      div(class = "combatqc-check-output",
                                          DT::DTOutput("scale_screen_summary", fill = FALSE)),
                                      div(class = "combatls-screen-interpretation",
                                        p(tags$strong("Significant features"),
                                          " is the number of features with a BH-adjusted p-value below 0.05 for that covariate. BH adjustment is applied across features separately for each covariate. A nonsignificant result does not show that variability is constant."),
                                        p(tags$strong("Median SD ratio"),
                                          " summarizes the estimated change across features with an available estimate, including nonsignificant features. Read it using the contrast shown in the table:"),
                                        tags$ul(
                                          tags$li(tags$strong("1:"), " No estimated change in SD"),
                                          tags$li(tags$strong("Above 1:"), " Higher estimated SD"),
                                          tags$li(tags$strong("Below 1:"), " Lower estimated SD")
                                        ),
                                        p("For smooth effects or covariates with multiple contrasts, use the feature-wise results and centile curves; one ratio cannot summarize the full pattern.")
                                      ),
                                      tags$details(
                                        tags$summary(tags$strong("View feature-wise results")),
                                        h4("Feature-wise Scale Associations"),
                                        selectInput("scale_table_covariate", "Select biological covariate",
                                          choices = c("All" = "", stats::setNames(covariates, covariates)),
                                          selected = "", width = "100%"),
                                        checkboxInput("scale_significant_only",
                                          "Show significant results only (BH-adjusted p < 0.05)", value = FALSE, width = "100%"),
                                        div(class = "combatqc-check-output",
                                            DT::DTOutput("scale_screen_tests", fill = FALSE))
                                      )),
                             tabPanel("Centile curves",
                               div(class = "combatls-centile-view",
                                      selectInput("scale_centile_feature", "Feature", choices = features, selected = features[1], width = "100%"),
                                      checkboxInput("scale_compare_centiles", "Compare proposed and screening centiles", value = TRUE, width = "100%"),
                                      conditionalPanel(
                                        "!input.scale_compare_centiles",
                                        radioButtons("scale_centile_model", "Scale model",
                                          choices = c("Proposed" = "proposed", "Screening" = "screening"),
                                          selected = "proposed", inline = TRUE)
                                      ),
                                      fluidRow(
                                        column(6, selectInput("scale_curve_x", "Horizontal covariate (original units)",
                                          choices = numeric_covariates, selected = default_curve_x, width = "100%")),
                                        column(6, selectInput("scale_curve_group", "Separate curves by",
                                          choices = c("None", char_var), selected = "None", width = "100%"))),
                                      helpText(textOutput("scale_centile_feature_note")),
                                      checkboxInput("scale_show_points", "Show observed measurements", value = FALSE),
                                      div(class = "combatls-centile-profile",
                                        textOutput("scale_centile_profile")),
                                      plotOutput("scale_centiles", height = "500px"),
                                      helpText("At each x-axis value, curves use the displayed profile and batch target, holding other covariates at their median or most common level. Lines show fitted medians; dark and light bands show the 25th\u201375th and 5th\u201395th centiles. ",
                                        tags$strong("These are not confidence intervals.")),
                                      helpText("Raw points retain each person\u2019s covariates and batch, so their position relative to the bands is not a direct fit check. Use ",
                                        tags$strong("Check model fit"), " below to examine residuals. These checks use the same data that fitted the models."),
                                      tags$details(id = "scale_fit_details",
                                        tags$summary(tags$strong("Check model fit")),
                                        helpText("Both saved models are evaluated at the observations' own covariates and batches. Normalized residuals should be centered near zero with spread near one. These are exploratory checks on the fitting data, not validation."),
                                        radioButtons("scale_fit_view", "Residual view",
                                          choices = c("Residuals and binned RMS" = "residuals", "Q-Q plots" = "qq"),
                                          selected = "residuals", inline = TRUE),
                                        helpText(textOutput("scale_fit_status")),
                                        plotOutput("scale_fit_diagnostics", height = "auto"),
                                        helpText("Binned RMS summarizes how far normalized residuals fall from zero: values near 1 are expected under the fitted model, while higher or lower values indicate larger or smaller deviations. Each label shows the number of residuals in that bin. Dots appear only for bins with at least 10 finite residuals, and lines stop at gaps. The sample counts above show where observations are available, not model uncertainty.")),
                                      h4("Estimated conditional variability"),
                                      helpText("SD is compared at the observed 25th and 75th percentiles of the horizontal covariate, within each group when separated. Values use the covariate's original units; changes are descriptive model estimates."),
                                      div(class = "combatqc-check-output",
                                        DT::DTOutput("scale_variability", fill = FALSE))))

                           )
                         )
                       ),
                       legacy_controls(fluidRow(
                         shinydashboard::box(
                           width = 12,
                           shiny::uiOutput("eb_explain"))),
                       fluidRow(
                         shinydashboard::box(
                           width = 12,
                           title = "Location Parameters",
                           shiny::plotOutput("eb_location"))),
                       fluidRow(
                         shinydashboard::box(
                           width = 12,
                           title = "Scale Paramaters",
                           shiny::plotOutput("eb_scale")))))
            },
            id = "tabselected"
          )
        )
      )
    )
  }
  ## Server Design
  server <- function(input, output, session) {
    combat_result_s <- reactiveVal(NULL)
    covariance_check_s <- reactiveVal(NULL)
    covariance_features_s <- reactiveVal(utils::head(features, 20))
    output$covariance_feature_summary <- renderText({
      paste(length(covariance_features_s()), "of", length(features), "features selected")
    })
    output$covariance_feature_draft_summary <- renderText({
      paste(length(input$covariance_feature_choices), "of", length(features), "features selected")
    })
    observeEvent(input$covariance_change_features, {
      showModal(modalDialog(title = "Choose covariance features", size = "l", easyClose = TRUE,
        textInput("covariance_feature_search", "Search features", value = "", width = "100%"),
        div(class = "covariance-feature-actions",
          actionButton("covariance_all_features", "Select all"),
          actionButton("covariance_clear_features", "Clear"),
          actionButton("covariance_first_features", "Use first 20")),
        textOutput("covariance_feature_draft_summary"),
        checkboxGroupInput("covariance_feature_choices", label = NULL, choices = features,
          selected = covariance_features_s(), width = "100%"),
        footer = tagList(modalButton("Cancel"), actionButton("covariance_apply_features", "Apply", class = "btn-primary"))))
    })
    observeEvent(input$covariance_all_features, {
      updateCheckboxGroupInput(session, "covariance_feature_choices", selected = features)
    })
    observeEvent(input$covariance_clear_features, {
      updateCheckboxGroupInput(session, "covariance_feature_choices", selected = character())
    })
    observeEvent(input$covariance_first_features, {
      updateCheckboxGroupInput(session, "covariance_feature_choices", selected = utils::head(features, 20))
    })
    observeEvent(input$covariance_apply_features, {
      selected <- features[features %in% input$covariance_feature_choices]
      if (length(selected) < 2L) {
        showNotification("Select at least two features before applying the selection.", type = "error")
        return()
      }
      covariance_features_s(selected)
      removeModal()
    })
    covariance_spec <- reactive({ list(features = covariance_features_s(), matrix = input$covariance_matrix) })
    covariance_ready <- reactive({
      saved <- covariance_check_s()
      !is.null(saved) && identical(saved$input_spec, covariance_spec())
    })
    output$covariance_ready <- function(...) covariance_ready()
    outputOptions(output, "covariance_ready", suspendWhenHidden = FALSE)
    current_covariance_check <- reactive({ req(covariance_ready()); covariance_check_s() })
    output$covariance_prompt <- renderUI({
      if (is.null(covariance_check_s())) return(helpText("Select at least two features, then run the covariance check."))
      if (!covariance_ready()) return(helpText("Covariance settings have changed. Run the covariance check again."))
      NULL
    })
    observeEvent(input$covariance_run, {
      previous_check <- isolate(covariance_check_s())
      covariance_check_s(NULL)
      tryCatch({
        spec <- covariance_spec()
        checked <- withProgress(message = "Comparing batch covariance matrices...", value = .5, {
          covariance_batch_check(result, features = spec$features, matrix = spec$matrix)
        })
        checked$input_spec <- spec
        covariance_check_s(checked)
        choices <- names(checked$matrices)
        selected <- intersect(isolate(input$covariance_plot_batches), choices)
        if (is.null(previous_check) && !length(selected)) selected <- utils::head(choices, 4)
        updateCheckboxGroupInput(session, "covariance_plot_batches", choices = choices, selected = selected)
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })
    output$covariance_status <- renderText({
      checked <- current_covariance_check()
      paste(length(checked$features), "features \u00b7", length(checked$matrices),
        "eligible batches \u00b7", checked$matrix)
    })
    output$covariance_sampling_caution <- renderUI({
      samples <- current_covariance_check()$samples
      affected <- samples[samples$high_dimensional, , drop = FALSE]
      if (!nrow(affected)) return(NULL)
      div(class = "alert alert-warning", role = "note",
        tags$strong("Limited sample support: "),
        paste0(paste(paste0(affected$batch, " (n = ", affected$rows_complete,
          ", p = ", affected$feature_count, ")"), collapse = "; "), ". "),
        "Here p >= n, so sample matrix rank is at most n - 1. Distances remain computable, but sampling variability may be substantial. A large distance alone does not establish a batch effect. Consider a smaller, scientifically chosen feature set. This notice includes hidden batches used in the pooled reference.")
    })
    covariance_display_batches <- reactive({
      intersect(input$covariance_plot_batches, names(current_covariance_check()$matrices))
    })
    output$covariance_display_status <- renderText({
      paste(length(covariance_display_batches()), "of", length(current_covariance_check()$matrices),
        "batches displayed. The pooled reference uses all eligible batches.")
    })
    output$covariance_exclusions <- renderText({
      checked <- current_covariance_check()
      excluded <- checked$excluded_features$feature
      messages <- character()
      if (length(excluded)) messages <- c(messages, paste("Features excluded for insufficient residual variation:", paste(excluded, collapse = ", ")))
      if (checked$missing_batch_rows > 0) messages <- c(messages,
        paste(checked$missing_batch_rows, "rows excluded for missing batch labels."))
      ineligible <- checked$samples[!checked$samples$eligible, , drop = FALSE]
      if (nrow(ineligible)) messages <- c(messages, paste("Batches excluded for too few complete rows:",
        paste(paste0(ineligible$batch, " (", ineligible$rows_complete, ")"), collapse = ", ")))
      paste(messages, collapse = " ")
    })
    covariance_matrices_plot <- reactive({
      checked <- current_covariance_check()
      req(input$covariance_view)
      batches <- covariance_display_batches()
      validate(need(length(batches) > 0, "Select at least one eligible batch to display."))
      covariance_check_plot(checked, type = input$covariance_view, batches = batches,
        show.labels = isTRUE(input$covariance_show_labels))
    })
    output$covariance_matrices <- renderPlot({ covariance_matrices_plot() }, height = function() {
      count <- length(covariance_display_batches())
      columns <- .covariance_panel_columns(count, isTRUE(input$covariance_show_labels))
      max(440, ceiling(count / columns) * if (isTRUE(input$covariance_show_labels)) 600 else 440)
    })
    covariance_pca_check <- reactive({
      checked <- current_covariance_check()
      target <- input$covariance_pca_variance
      if (is.null(target)) target <- 90
      validate(need(is.numeric(target) && length(target) == 1L && is.finite(target) &&
        target >= 1 && target <= 100, "Choose a PCA variance target between 1% and 100%."))
      checked$pca <- .covariance_pca_select(checked$pca, target / 100)
      checked
    })
    output$covariance_pca_status <- renderText({
      pca <- covariance_pca_check()$pca
      paste0(pca$retained, " retained PC", if (pca$retained == 1L) "" else "s",
        " \u00b7 ", sprintf("%.1f%%", 100 * pca$explained),
        " of pooled, scaled within-batch variance explained (target ",
        sprintf("%g%%", 100 * pca$target), ")",
        if (pca$explained < pca$target - 1e-12) "; PC limit reached" else "")
    })
    covariance_pca_plot <- reactive({
      batches <- covariance_display_batches()
      validate(need(length(batches) > 0, "Select at least one eligible batch to display."))
      covariance_check_plot(covariance_pca_check(), type = "pca", batches = batches)
    })
    output$covariance_pca <- renderPlot({ covariance_pca_plot() }, height = function() {
      max(300, 140 + 50 * length(covariance_display_batches()))
    })
    covariance_sample_data <- reactive({
      checked <- current_covariance_check()
      data <- merge(checked$samples, checked$to_pooled, by = "batch", all.x = TRUE, sort = FALSE)
      data[match(covariance_display_batches(), data$batch), , drop = FALSE]
    })
    covariance_pairwise_data <- reactive({
      data <- current_covariance_check()$pairwise
      batches <- covariance_display_batches()
      data[data$batch_1 %in% batches & data$batch_2 %in% batches, , drop = FALSE]
    })
    covariance_sample_rows <- reactive({
      data <- covariance_sample_data()
      data.frame(Batch = data$batch, `Off-diagonal RMS` = data$off_diagonal_rms,
        `Spectral norm` = data$spectral, `Largest absolute difference` = data$max_abs,
        `Frobenius distance` = data$frobenius,
        `Complete rows (n)` = data$rows_complete, `Features (p)` = data$feature_count,
        `Matrix rank` = data$matrix_rank, Status = data$status, check.names = FALSE)
    })
    covariance_pairwise_rows <- reactive({
      data <- covariance_pairwise_data()
      data.frame(`Batch 1` = data$batch_1, `Batch 2` = data$batch_2,
        `Off-diagonal RMS` = data$off_diagonal_rms, `Spectral norm` = data$spectral,
        `Largest absolute difference` = data$max_abs, `Frobenius distance` = data$frobenius,
        check.names = FALSE)
    })
    covariance_distance_table <- function(data, empty_message) {
      table <- DT::datatable(data, rownames = FALSE,
        options = list(dom = "tip", paging = nrow(data) > 10, pageLength = 10,
          scrollX = TRUE, order = list(list(match("Off-diagonal RMS", names(data)) - 1L, "desc")),
          language = list(emptyTable = empty_message)))
      columns <- intersect(c("Off-diagonal RMS", "Spectral norm", "Largest absolute difference", "Frobenius distance"), names(data))
      DT::formatRound(table, columns, digits = 3)
    }
    output$covariance_sample_table <- DT::renderDT({
      covariance_distance_table(covariance_sample_rows(), "Select at least one eligible batch to display.")
    })
    output$covariance_pairwise_table <- DT::renderDT({
      covariance_distance_table(covariance_pairwise_rows(), "Select at least two eligible batches to compare pairs.")
    })
    scale_check_s <- reactiveVal(NULL)
    scale_check_spec <- reactive({
      list(location = input$combatls_formula, scale = input$combatls_sigma_formula,
           candidates = input$scale_candidates, exploratory = input$scale_screen_formula,
           reference = input$ref_bat_select)
    })
    scale_check_ready <- reactive({
      saved <- scale_check_s()
      identical(input$com_type, "combatls") && !is.null(saved) &&
        identical(saved$input_spec, scale_check_spec())
    })
    output$scale_check_ready <- function(...) { scale_check_ready() }
    outputOptions(output, "scale_check_ready", suspendWhenHidden = FALSE)
    output$scale_check_prompt <- renderUI({
      req(identical(input$com_type, "combatls"))
      saved <- scale_check_s()
      if (is.null(saved))
        return(helpText("Run Model Check to generate pre-harmonization diagnostics."))
      if (!scale_check_ready())
        return(helpText("Model settings have changed. Run Model Check again."))
      NULL
    })
    current_scale_check <- reactive({
      req(scale_check_ready())
      scale_check_s()
    })

    observeEvent(input$scale_model_check, {
      req(combatls_available, identical(input$com_type, "combatls"))
      scale_check_s(NULL)
      tryCatch({
        location <- trimws(if (is.null(input$combatls_formula)) "" else input$combatls_formula)
        if (!nzchar(location)) stop("Enter a location formula before running Model Check.", call. = FALSE)
        scale <- trimws(if (is.null(input$combatls_sigma_formula)) "" else input$combatls_sigma_formula)
        exploratory <- trimws(if (is.null(input$scale_screen_formula)) "" else input$scale_screen_formula)
        checked <- withProgress(message = "Checking GAMLSS scale models...", value = 0.5, {
          combatls_model_check(result = result, formula = stats::as.formula(location),
            sigma.formula = stats::as.formula(if (nzchar(scale)) scale else "~ 1"),
            screen.formula = if (nzchar(exploratory)) stats::as.formula(exploratory) else NULL,
            candidates = if (is.null(input$scale_candidates)) character() else input$scale_candidates,
            ref.batch = if (is.null(input$ref_bat_select) || input$ref_bat_select == "None") NULL else input$ref_bat_select)
        })
        checked$input_spec <- scale_check_spec()
        scale_check_s(checked)
        showNotification("Model check complete. Review screening, centiles, and fitting status before harmonization.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })
    output$scale_check_status <- renderText({
      checked <- current_scale_check()
      diagnostics <- checked$diagnostics
      issues <- sum(!diagnostics$valid | nzchar(diagnostics$message), na.rm = TRUE)
      paste("Fitting status:", sum(diagnostics$valid), "of", nrow(diagnostics),
            "fits converged and identifiable (including screening comparisons);",
            issues, "fits reported warnings or errors. Exploratory fits assume independent observations.")
    })
    output$scale_formula_suggestion <- renderText({
      checked <- current_scale_check()
      if (is.null(checked$suggested.formula)) return("No formula draft is available from valid comparisons.")
      paste(gsub("q < 0.05", "BH-adjusted p-value < 0.05", checked$suggestion.rule, fixed = TRUE), "Formula for manual review:", paste(deparse(checked$suggested.formula), collapse = " "))
    })
    observeEvent(input$scale_accept_suggestion, {
      checked <- current_scale_check()
      req(!is.null(checked$suggested.formula))
      updateTextInput(session, "combatls_sigma_formula", value = paste(deparse(checked$suggested.formula), collapse = " "))
      showNotification("Formula draft copied. Review or edit it, then rerun Model Check.", type = "message")
    })
    output$scale_screen_heatmap <- renderPlot({
      checked <- current_scale_check()
      validate(need(nrow(checked$screening) > 0, "Select candidate scale covariates to screen."))
      combatls_check_plot(checked, "screening",
        show.y.labels = !identical(input$scale_heatmap_y_labels, FALSE)) + .combatls_plot_text_theme()
    })
    scale_screen_tables <- reactive({ .combatls_screening_display(current_scale_check()) })
    output$scale_screen_summary <- DT::renderDT({
      DT::datatable(scale_screen_tables()$summary, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    scale_association_rows <- reactive({
      .combatls_filter_associations(scale_screen_tables()$details,
        covariate = input$scale_table_covariate,
        significant_only = input$scale_significant_only)
    })
    output$scale_screen_tests <- DT::renderDT({
      .combatls_associations_table(scale_association_rows())
    })
    observe({
      checked <- current_scale_check()
      req(input$scale_curve_x)
      choices <- .combatls_centile_feature_order(checked, input$scale_curve_x)
      selected <- isolate(input$scale_centile_feature)
      if (is.null(selected) || !selected %in% choices) selected <- choices[1L]
      updateSelectInput(session, "scale_centile_feature", choices = choices, selected = selected)
    })
    output$scale_centile_feature_note <- renderText({
      checked <- current_scale_check()
      req(input$scale_curve_x)
      if (!input$scale_curve_x %in% checked$candidates)
        return("No scale screening for this covariate; features retain their original order.")
      NULL
    })
    scale_centile_spec <- reactive({
      checked <- current_scale_check()
      req(input$scale_centile_feature, input$scale_curve_x)
      if (!isTRUE(input$scale_compare_centiles)) req(input$scale_centile_model)
      list(check = checked, feature = input$scale_centile_feature, x = input$scale_curve_x,
        group = if (is.null(input$scale_curve_group) || input$scale_curve_group == "None") NULL else input$scale_curve_group,
        model = if (isTRUE(input$scale_compare_centiles)) "both" else input$scale_centile_model)
    })
    scale_centile_plot_cached <- reactive({
      spec <- scale_centile_spec()
      # GAMLSS smooth prediction inspects sys.calls(). Keep the fitted object
      # out of the call itself: do.call() embeds it and makes pb() very slow.
      tryCatch(combatls_check_plot(spec$check, type = "centiles", feature = spec$feature,
                 x = spec$x, group = spec$group, model = spec$model),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$scale_centile_profile <- renderText({
      spec <- scale_centile_spec()
      .combatls_profile_summary(spec$check, spec$x, spec$group)
    })
    scale_observation_bins <- reactive({
      spec <- scale_centile_spec()
      tryCatch(.combatls_observation_bins(spec$check, spec$feature, spec$x, spec$group),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$scale_centiles <- renderPlot({
      plot <- scale_centile_plot_cached()
      if (isTRUE(input$scale_show_points)) {
        spec <- scale_centile_spec()
        plot <- .combatls_add_observations(plot, spec$check, spec$feature, spec$x, spec$group)
      }
      grid::grid.draw(.combatls_centiles_with_counts(plot, scale_observation_bins()))
    })
    scale_fit_data <- reactive({
      spec <- scale_centile_spec()
      .combatls_fit_view_data(spec$check, spec$feature, scale_observation_bins())
    })
    output$scale_fit_status <- renderText({
      status <- scale_fit_data()$status
      messages <- status[nzchar(status$message), , drop = FALSE]
      if (!nrow(messages)) return(NULL)
      paste(paste0(messages$scale_model, ": ", messages$message), collapse = " ")
    })
    output$scale_fit_diagnostics <- renderPlot({
      view <- if (identical(input$scale_fit_view, "qq")) "qq" else "residuals"
      .combatls_fit_view_plot(scale_fit_data(), view)
    }, height = function() {
      groups <- length(unique(scale_observation_bins()$bins$curve_group))
      if (identical(input$scale_fit_view, "qq")) max(300, 210 * groups)
      else max(460, 260 * groups + 160)
    })
    scale_variability <- reactive({
      spec <- scale_centile_spec()
      tryCatch(.combatls_centile_variability(spec$check, spec$feature, spec$x,
                 spec$group, model = spec$model),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$scale_variability <- DT::renderDT({
      spec <- scale_centile_spec()
      DT::datatable(.combatls_variability_display(scale_variability(), spec$x, spec$group),
        rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    })

    ############### Data Overview #############################
    output$explore_bar <- shiny::renderUI({
      if(input$data_view == "Exploratory Analysis"){
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Controlling Visualizations",
            selectInput("single_feature", "Select one feature to investigate", choices = features, selected = features[1]),
            selectInput("single_cov", "Select one covariate to investigate", choices = covariates, selected = covariates[1]),
            radioButtons("num_var_control_batch", "Select whether to separate by batch", choices = c("Yes", "No"), selected = "No"),
            shiny::uiOutput("batch_sep_control"),
            shiny::uiOutput("cov_visual_control")))
      }
    })

    output$cov_visual_control <- shiny::renderUI({
      if (!is.null(covariates)){
        if(input$single_cov %in% num_var){
          fluidRow(
            shinydashboard::box(
              width = 12,
              title = "Controlling Visualizations",
              radioButtons("num_var_control", "Select the type of smooth method", choices = c("lm", "loess", "glm", "gam"), selected = "lm"),
              sliderInput("se", "Select transparency", min = 0, max = 1, value = 0.2, step = 0.1)))
        }else if(input$single_cov %in% char_var){
          fluidRow(
            shinydashboard::box(
              width = 12,
              title = "Controlling Visualizations",
              radioButtons("char_var_control", "Select the type of plot to display", choices = c("boxplot", "boxplot with points", "density plot"), selected = "boxplot")))
        }
      }
    })

    output$batch_sep_control <- shiny::renderUI({
      if(input$num_var_control_batch == "Yes"){
        checkboxGroupInput("overview_batch_select", "Select batch levels to include:", choices = levels(df[[batch]]), selected = levels(df[[batch]]))
      }
    })

    output$data_ui <- shiny::renderUI({
      if(input$data_view == "Exploratory Analysis"){
        fluidRow(
          column(width = 6,
                 shinydashboard::box(
                   width = NULL,
                   title = "Batch vs Feature",
                   shiny::plotOutput("batch_vi"))),
          column(width = 6,
                 shinydashboard::box(
                   width = NULL,
                   title = "Covariate vs Feature",
                   shiny::plotOutput("cov_vi"))))
      }else{shiny::htmlOutput("data_rm_explain")}
    })

    output$data_rm_explain <- shiny::renderUI({
      data_rm <- info$summary_df %>% filter(.data[["remove"]] == "removed")
      if(nrow(data_rm) == 0){
        HTML(print("Batch levels that contain less than 3 observations are dropped: <strong>no batch level is dropped</strong>."))
      }else{
        HTML(paste0("Batch levels that contain less than 3 observations are dropped: <strong>", nrow(data_rm), " levels are dropped, corresponding to ", sum(data_rm$count), " observations</strong>."))
      }
    })

    output$batch_vi <- shiny::renderPlot({
      if(input$num_var_control_batch == "No"){
        combat_plot_gen(result, f = input$single_feature, batch_control = "No", plot_name = "batch_density")
      }else{
        combat_plot_gen(result, f = input$single_feature, batch_control = "Yes", batch_level = input$overview_batch_select, plot_name = "batch_density")
      }
    })

    output$cov_vi <- shiny::renderPlot({
      if (!is.null(covariates)){
        if(input$single_cov %in% num_var){
          if(input$num_var_control_batch == "No"){
            combat_plot_gen(result, f = input$single_feature, batch_control = "No", plot_name = "cov_feature", c = input$single_cov, smooth_method = input$num_var_control, alpha = input$se)
          }else{
            combat_plot_gen(result, f = input$single_feature, batch_control = "Yes", batch_level = input$overview_batch_select, plot_name = "cov_feature", c = input$single_cov, smooth_method = input$num_var_control, alpha = input$se)
          }
        }else if(input$single_cov %in% char_var){
          if(input$char_var_control == "boxplot"){
            if(input$num_var_control_batch == "No"){
              combat_plot_gen(result, f = input$single_feature, batch_control = "No", plot_name = "cov_feature", c = input$single_cov, char_plot_type = "boxplot")

            }else{
              combat_plot_gen(result, f = input$single_feature, batch_control = "Yes", batch_level = input$overview_batch_select, plot_name = "cov_feature", c = input$single_cov, char_plot_type = "boxplot")
            }
          }else if(input$char_var_control == "boxplot with points"){
            if(input$num_var_control_batch == "No"){
              combat_plot_gen(result, f = input$single_feature, batch_control = "No", plot_name = "cov_feature", c = input$single_cov, char_plot_type = "boxplot with points")
            }else{
              combat_plot_gen(result, f = input$single_feature, batch_control = "Yes", batch_level = input$overview_batch_select, plot_name = "cov_feature", c = input$single_cov, char_plot_type = "boxplot with points")
            }
          }else if(input$char_var_control == "density plot"){
            combat_plot_gen(result, f = input$single_feature, batch_control = "No", plot_name = "cov_feature", c = input$single_cov, char_plot_type = "density plot")
          }
        }
      }
    })


    output$data_usage_explain <- shiny::renderUI({
      HTML(paste0("Below is a preview of the data used for batch effect evaluation (and harmonization). Please review the dataset carefully to ensure correct identification of features, covariates and batch variable. <br> ",
                  "<strong>Note</strong>: The corresponding color themes for each type of variable are as follows: <br> ",
                  "<strong>Covariates</strong> - <strong><span style='color: pink;'>pink</span></strong>; ",
                  "<strong>Features</strong> - <strong><span style='color: lightyellow;'>lightyellow</span></strong>; ",
                  "<strong>Batch</strong> - <strong><span style='color: lightblue;'>lightblue</span></strong>."))
    })

    output$data_frame <- DT::renderDT({
      if(input$data_view == "Exploratory Analysis"){
        combat_table_gen(result, table_name = "exploratory_analysis", f = input$single_feature)
      }else{
        combat_table_gen(result, table_name = "data_overview")
      }
    })

    ############### Summary #############################
    output$cov_status <- shiny::renderUI({
      if (!is.null(covariates)){
        selectInput("cov", "Select covariate", choices = covariates, selected = covariates[1])
      }
    })

    output$plot_text_status <- shiny::renderUI({
      if(input$type == "Plot"){
        radioButtons("text_status", "Select whether to display numeric labels on the plot", choices = c("Yes", "No"), selected = "No")
      }
    })

    output$output <- shiny::renderUI({
      if (input$type == "Plot") {
        plotOutput("plot")
      } else if (input$type == "Table") {
        DT::DTOutput("table")
      }
    })
    output$plot <- shiny::renderPlot({
      if(input$text_status == "No"){combat_plot_gen(result, plot_name = "batch_summary")}else{combat_plot_gen(result, plot_name = "batch_summary", text_status = "Yes")}
    })
    output$table <- DT::renderDT({
      combat_table_gen(result, table_name = "summary_df")
    })
    output$cov_output <- shiny::renderUI({
      if (is.null(covariates)){
        textOutput("cov_text")
      }else if (input$type == "Plot") {
        plotOutput("cov_plot")
      } else if (input$type == "Table") {
        DT::DTOutput("cov_table")
      }
    })

    output$cov_text <- shiny::renderText({
      "No covariate is preserved"
    })

    output$cov_plot <- shiny::renderPlot({
      if (!is.null(covariates)){
        if(input$text_status == "No"){combat_plot_gen(result, plot_name = "cov_distribution", c = input$cov)}else{combat_plot_gen(result, plot_name = "cov_distribution", c = input$cov, text_status = "Yes")}
      }
    })
    output$cov_table <-  DT::renderDT({
      combat_table_gen(result, table_name = "cov_table", c = input$cov)
    })

    ############### Residual Plot #############################
    output$resid_all_control <- shiny::renderUI({
      if(input$resid_all == "No"){
        checkboxGroupInput("resid_batch_select", "Select batch levels to include:", choices = levels(df[[batch]]), selected = levels(df[[batch]]))
      }
    })
    output$resid_label_control <- shiny::renderUI({
      if(input$resid_label == "Yes"){
        sliderInput("label_angle", "Customize the angle of the label", min = 0, max = 90, value = 0)
      }
    })

    output$res_add_explain <- shiny::renderUI({
      HTML(print("A <strong>noticeable deviation of the mean from zero</strong> in the additive-residual box plot indicates the presence of an additive batch effect"))
    })

    output$res_add <- shiny::renderPlot({
      if(input$resid_color == "No"){
        if(input$resid_all == "Yes"){
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_add", color = "No", label = "No")
            }else{
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_add", color = "No", label = "Yes", angle = input$label_angle)
          }
        }else{
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_add", color = "No", label = "No")
          }else{
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_add", color = "No", label = "Yes", angle = input$label_angle)
          }
        }
      }else{
        if(input$resid_all == "Yes"){
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_add", color = "Yes", label = "No")
          }else{
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_add", color = "Yes", label = "Yes", angle = input$label_angle)
          }
        }else{
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_add", color = "Yes", label = "No")
          }else{
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_add", color = "Yes", label = "Yes", angle = input$label_angle)
          }
        }
      }
    })

    output$res_ml_explain <- shiny::renderUI({
      HTML(print("A <strong>substantial variation</strong> in the multiplicative-residual box plot demonstrates a potential multiplicative batch effect."))
    })

    output$res_ml <- shiny::renderPlot({
      if(input$resid_color == "No"){
        if(input$resid_all == "Yes"){
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_mul", color = "No", label = "No")
          }else{
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_mul", color = "No", label = "Yes", angle = input$label_angle)
          }
        }else{
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_mul", color = "No", label = "No")
          }else{
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_mul", color = "No", label = "Yes", angle = input$label_angle)
          }
        }
      }else{
        if(input$resid_all == "Yes"){
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_mul", color = "Yes", label = "No")
          }else{
            combat_plot_gen(result, f = input$feature, batch_control = "No", plot_name = "resid_mul", color = "Yes", label = "Yes", angle = input$label_angle)
          }
        }else{
          if(input$resid_label == "No"){
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_mul", color = "Yes", label = "No")
          }else{
            combat_plot_gen(result, f = input$feature, batch_control = "Yes", batch_level = input$resid_batch_select, plot_name = "resid_mul", color = "Yes", label = "Yes", angle = input$label_angle)
          }
        }
      }
    })

    ############### Dimensionality Reduction #############################
    output$pc_variance <- DT::renderDT({
      combat_table_gen(result, table_name = "pc_variance", PC1 = input$PC1, PC2 = input$PC2)
    })

    output$mdmr_control <- renderUI({
      if(!is.null(result$mdmr.summary)){
        tagList(list(
          uiOutput("mdmr_note"),
          DT::DTOutput("test_batch_mdmr"),
          uiOutput("mdmr_sig_text")
        ))
      }else{
        uiOutput("mdmr_skip")
      }
    })

    output$mdmr_skip <- renderUI({
      HTML("The MDMR test has been skipped.")
    })

    output$pca_all_control <- shiny::renderUI({
      if(input$pca_all == "No"){
        checkboxGroupInput("pca_batch_select", "Select batch levels to include:", choices = levels(df[[batch]]), selected = levels(df[[batch]]))
      }
    })

    output$pca <- shiny::renderPlot({
      if(input$pca_all == "Yes"){
        if(input$pca_label == "No"){combat_plot_gen(result, batch_control = "No", plot_name = "pca", PC1 = input$PC1, PC2 = input$PC2, label = "No")}else{
          combat_plot_gen(result, batch_control = "No", plot_name = "pca", PC1 = input$PC1, PC2 = input$PC2, label = "Yes")
        }
      }else{
        if(input$pca_label == "No"){combat_plot_gen(result, batch_control = "Yes", batch_level = input$pca_batch_select, plot_name = "pca", PC1 = input$PC1, PC2 = input$PC2, label = "No")}else{
          combat_plot_gen(result, batch_control = "Yes", batch_level = input$pca_batch_select, plot_name = "pca", PC1 = input$PC1, PC2 = input$PC2, label = "Yes")
        }
      }
    })
    output$tsne <- shiny::renderPlot({
      if(input$pca_all == "Yes"){
        if(input$pca_label == "No"){combat_plot_gen(result, batch_control = "No", plot_name = "tsne", label = "No")}else{
          combat_plot_gen(result, batch_control = "No", plot_name = "tsne", label = "Yes")
        }
      }else{
        if(input$pca_label == "No"){combat_plot_gen(result, batch_control = "Yes", batch_level = input$pca_batch_select, plot_name = "tsne", label = "No")}else{
          combat_plot_gen(result, batch_control = "Yes", batch_level = input$pca_batch_select, plot_name = "tsne", label = "Yes")
        }
      }
    })

    ############### Harmonization if needed #############################
    output$com_type_note <- renderUI({
      if(input$com_type == "comfam"){
        HTML(print("<strong>Note</strong>: Correcting Batch Effects <strong>(ComBat Family)</strong> <br><br>"))
      }else if (identical(input$com_type, "combatls")) {
        HTML("<strong>ComBatLS</strong>: preserve covariate effects in location and scale using GAMLSS formulas.<br><br>")
      }else{
        HTML(print("<strong>Note</strong>: Correcting Covariance Batch Effects <strong>(CovBat Family)</strong> <br><br>"))
      }
    })

    output$com_model_note <- renderUI({
      if (identical(input$com_type, "combatls")) return(NULL)
      if(input$com_model == "lm"){
        HTML(print("<strong>Note</strong>: a method designed for batch effect correction in cross-sectional data with linear covariate effects. <strong>(Original ComBat)</strong> <br><br>"))
      }else if(input$com_model == "lmer"){
        HTML(print("<strong>Note</strong>: a method accounts for intra-subject correlation in longitudinal data by incorporating random effects into the model. <strong>(Longitudinal ComBat)</strong> <br><br>"))
      }else if(input$com_model == "gam"){
        HTML(print("<strong>Note</strong>: a method allows for preservation of non-linear covariate effects through use of the generalized additive model. <strong>(ComBat-GAM)</strong> <br><br>"))
      }
    })

    output$smooth_select <- renderUI({
      if (identical(input$com_type, "combatls")) return(NULL)
      if(input$com_model == "gam"){
        checkboxGroupInput("smooth", "Select smooth terms:", choices = result$info$cov_shiny, selected = info$smooth_orig)
      }
    })

    output$random_select <- renderUI({
      if (identical(input$com_type, "combatls")) return(NULL)
      if(input$com_model == "lmer"){
        selectInput("random", "Select the random effect when considering longitudinal combat", choices = colnames(info$df), selected = colnames(info$df)[1])
      }
    })

    output$interaction_note <- renderUI({
      if (identical(input$com_type, "combatls")) return(NULL)
      HTML(print("eg: covariate1*covariate2,covariate3*covariate4 <br><br>"))
    })

    output$smooth_int_type_control <- renderUI({
      if (identical(input$com_type, "combatls")) return(NULL)
      if(input$com_model == "gam"){
        textInput("smooth_int_type", "Enter the types of potential interaction terms:", value = paste0(info$smooth_int_type, collapse = ","))
      }
    })

    output$smooth_int_type_control_note <- renderUI({
      if (identical(input$com_type, "combatls")) return(NULL)
      if(input$com_model == "gam"){
        HTML(paste0("eg: linear,factor-smooth <br><br>",
                    "<strong>Note</strong>: The detailed explanation of the interaction type can be found below <br><br>",
                    "<strong>linear</strong>: linear interaction terms <br>",
                    "<strong>categorical-continuous</strong>: categorical-continuous interactions (s(covariate1, by = categorical_covariate)) <br>",
                    "<strong>factor-smooth</strong>: includes categorical variable as part of the smooth (s(covariate1,categorical_covariate, bs = 'fs')) <br>",
                    "<strong>tensor</strong>: represents interactions with different scales (ti(covariate1,covariate2)) <br>",
                    "<strong>smooth-smooth</strong>: represents interaction between smoothed variables (s(covariate1,covariate2)) <br><br>"))
      }
    })

    output$eb_explain <- shiny::renderUI({
      HTML(paste0("<br>",
                  "This section aims to check the <strong>prior distribution assumption</strong> of the L/S model batch parameters. <br>",
                  "<br>",
                  "If the Empirical Bayes (EB) method is used, we expect to see: <br>",
                  "<ul>
                          <li>An <strong>estimated density distribution of the empirical values</strong> for both the location parameter gamma hat and the scale parameter delta hat (dotted line)</li>
                          <li>The <strong>EB-based prior distribution</strong> for both the location parameter gamma hat and the scale parameter delta hat (solid line)</li>
                    </ul>",
                  "If not, we expect to see: <br>",
                  "<ul>
                          <li>An <strong>estimated density distribution of the empirical values</strong> for both the location parameter gamma hat and the scale parameter delta hat (solid line)</li>
                    </ul>",
                  "If <strong>All</strong> batches are selected, line plots are colored by <strong>batch level</strong>."))
    })

    output$eb_check_description <- renderUI({
      HTML(paste0("Check if the EB assumption is met appropriately. <br>",
                  "<br>"))
    })

    observeEvent(input$eb_check_button,{
      req(!identical(input$com_type, "combatls"))
      if(length(input$interaction) > 0 & input$interaction != ""){
        interaction_enco <- sapply(strsplit(input$interaction, ",")[[1]], function(x) gsub("\\*", "\\,", x), USE.NAMES = FALSE)
        if(input$com_model == "gam"){
          smooth_int_type_enco <- strsplit(input$smooth_int_type, ",")[[1]]
        }else{smooth_int_type_enco <- NULL}
      }else{
        interaction_enco <- NULL
        smooth_int_type_enco <- NULL}
      eb <- ifelse(input$eb_control == "Yes", TRUE, FALSE)
      if(input$com_model == "gam"){
        smooth <- input$smooth
      }else{smooth <- NULL}
      if(input$com_model == "lmer"){
        random <- input$random
      }else{random <- NULL}

      if(input$ref_bat_select == "None"){
        ref_bat <- NULL
      }else{ref_bat <- input$ref_bat_select}

      msg <- sprintf('Start EB assumption check...')
      withProgress(message = msg, value = 0, {
        setProgress(0.5, 'Generating EB distribution...')
        eb_df <- combat_harm(eb_check = TRUE, result, type = input$com_model, random = input$random, smooth = input$smooth, interaction = interaction_enco, smooth_int_type = smooth_int_type_enco, family = input$com_type, ref.batch = ref_bat)
        setProgress(1, 'Complete!')
      })

      output$eb_location <- shiny::renderPlot({
        if(eb){
          if(input$batch_selection == "All"){
            combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "No", plot_name = "eb_location")
          }else{
            combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "Yes", batch_level = input$batch_selection, plot_name = "eb_location")
          }
        }else{
          if(input$batch_selection == "All"){
            combat_plot_gen(result, eb = FALSE, eb_df = eb_df, batch_control = "No", plot_name = "eb_location")
          }else{
            combat_plot_gen(result, eb = FALSE, eb_df = eb_df, batch_control = "Yes", batch_level = input$batch_selection, plot_name = "eb_location")
          }
        }
      })


      output$eb_scale <- shiny::renderPlot({
        if(eb){
          if(input$batch_selection == "All"){
            combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "No", plot_name = "eb_scale")
          }else{
            combat_plot_gen(result, eb = TRUE, eb_df = eb_df, batch_control = "Yes", batch_level = input$batch_selection, plot_name = "eb_scale")
          }
        }else{
          if(input$batch_selection == "All"){
            combat_plot_gen(result, eb = FALSE, eb_df = eb_df, batch_control = "No", plot_name = "eb_scale")
          }else{
            combat_plot_gen(result, eb = FALSE, eb_df = eb_df, batch_control = "Yes", batch_level = input$batch_selection, plot_name = "eb_scale")
          }
        }
      })
    })

    output$non_eb <- renderUI({
      if(input$eb_control == "No"){
        HTML(paste0("<strong>Note:</strong> The EB method is not selected! Only the empirical distribution of the location and scale parameters will be displayed. <br>", "<br>"))
      }else if(length(result$info$features) == 1){
        HTML(paste0("<strong>Note:</strong> The EB method is skipped for one feature dataset! Only the empirical distribution of the location and scale parameters will be displayed. <br>", "<br>"))
      }else{
        HTML(paste0("<strong>Note:</strong> When the number of features is small, the empirical and prior distributions may not visually align. <br>", "<br>"))
      }
    })

    observeEvent(input$ComBat,{
      combat_result_s(NULL)
      combat_result <- tryCatch({
        eb <- identical(input$eb_control, "Yes")
        ref_bat <- if (is.null(input$ref_bat_select) || input$ref_bat_select == "None") NULL else input$ref_bat_select
        harm_args <- list(result = result, family = input$com_type, eb = eb, ref.batch = ref_bat)
        if (identical(input$com_type, "combatls")) {
          if (!combatls_available) stop("ComBatLS is unavailable in this session.", call. = FALSE)
          location_text <- trimws(if (is.null(input$combatls_formula)) "" else input$combatls_formula)
          if (!nzchar(location_text)) stop("Enter a location formula, for example y ~ pb(AGE) + SEX.", call. = FALSE)
          scale_text <- trimws(if (is.null(input$combatls_sigma_formula)) "" else input$combatls_sigma_formula)
          harm_args$robust.LS <- isTRUE(input$combatls_robust)
          harm_args$formula <- stats::as.formula(location_text)
          harm_args$sigma.formula <- stats::as.formula(if (nzchar(scale_text)) scale_text else "~ 1")
        } else {
          interaction_text <- if (is.null(input$interaction)) "" else input$interaction
          interaction_enco <- if (nzchar(interaction_text)) gsub("*", ",", strsplit(interaction_text, ",")[[1]], fixed = TRUE) else NULL
          harm_args$type <- input$com_model
          harm_args$random <- if (input$com_model == "lmer") input$random else NULL
          harm_args$smooth <- if (input$com_model == "gam") input$smooth else NULL
          harm_args$interaction <- interaction_enco
          harm_args$smooth_int_type <- if (input$com_model == "gam" && !is.null(interaction_enco)) strsplit(input$smooth_int_type, ",")[[1]] else NULL
        }
        withProgress(message = "Harmonizing...", value = 0.5, {
          fitted <- do.call(combat_harm, harm_args)
          if (isTRUE(nzchar(input$save_path))) write.csv(fitted$harmonized_df, input$save_path, row.names = FALSE)
          setProgress(1, "Complete!")
          fitted
        })
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      })
      if (is.null(combat_result)) return()
      combat_result_s(combat_result)
      showNotification("Harmonization Completed", type = "message")
      output$output_msg <- renderPrint({
        if (isTRUE(nzchar(input$save_path))) paste("DataFrame saved to:", input$save_path)
        else "Harmonization completed. Enter a data path and run again to save the dataframe."
      })
    })

    observeEvent(input$ComBat_model,{
      model_save_path <- input$model_save_path
      combat_result <- req(combat_result_s())
      msg <- sprintf('Saving ComBat Model')
      withProgress(message = msg, value = 0, {
        setProgress(0.5, 'Saving ...')
        harm_model <- combat_result$combat.object
        if(combat_result$com_family %in% c("comfam", "combatls")){
          harm_model$ComBat.model$dat.combat <- NULL
        }else{
          harm_model$ComBat.model$dat.covbat <- NULL
        }
        saveRDS(harm_model, file = model_save_path)
        setProgress(1, 'Complete!')
      })
      showNotification('ComBat Model Successfully Saved', type = "message")

      output$output_msg_model <- renderPrint({
        paste("ComBat model saved to:", input$model_save_path)
      })
    })


    ############### Statistical Tests #############################

    output$mul_adjustment <- renderUI({
      HTML(paste0("<br>",
                  "<br>",
                  "All p-values have been adjusted by the <strong><span style='color: purple;'>Bonferroni</span></strong> method."))
    })

    output$test_batch_explain <- shiny::renderUI({
      if(input$test_batch == "ANOVA"){
        HTML(print("<strong>Note</strong>: The one-way ANOVA test is a statistical technique used to assess whether there are significant differences among the means of three or more groups. It requires meeting several assumptions to obtain reliable results."))
      }else if(input$test_batch == "Kruskal-Wallis"){
        HTML(print("<strong>Note</strong>: The Kruskal-Wallis test is a non-parametric statistical test used to compare the medians of two or more groups, which serves as an alternative to the ANOVA test when the assumption of normality or equal variance is not met."))
      }else if(input$test_batch == "Kenward-Roger (liner mixed model)"){
        HTML(print("<strong>Note</strong>: The Kenward-Roger(KR) test is commonly employed in the context of linear mixed-effects models to estimate the degrees of freedom for hypothesis testing."))
      }
    })

    output$test_variance_explain <- shiny::renderUI({
      if(input$test_variance == "Fligner-Killeen"){
        HTML(print("<strong>Note</strong>: The Fligner-Killeen (FK) test is a non-parametric alternative to Levene's and Bartlett's tests for assessing the homogeneity of variances. It doesn't rely on the assumption of normality."))
      }else if(input$test_variance== "Levene's Test"){
        HTML(print("<strong>Note</strong>: The Levene's test is a parametric test used to assess the equality of variances across multiple groups. It relies on the assumption of normality."))
      }else if(input$test_variance == "Bartlett's Test"){
        HTML(print("<strong>Note</strong>: The Bartlett's test is also a parametric test used for the same purpose as the Levene's test. Compared to the Levene's test, it is even more sensitive to departures from normality."))
      }
    })

    output$test_batch_ui <- shiny::renderUI({
      fluidRow(
        column(width = 12,
               shinydashboard::box(
                 width = NULL,
                 DT::DTOutput("test_batch_table"))))
    })

    output$test_variance_ui <- shiny::renderUI({
      fluidRow(
        column(width = 12,
               shinydashboard::box(
                 width = NULL,
                 DT::DTOutput("test_variance"))))
    })

    output$test_batch_mdmr <- DT::renderDT({
      combat_table_gen(result, table_name = "mdmr")
    })

    output$mdmr_sig_text <- renderUI({
      if(is.na(result$mdmr.summary$sig[2])){
        HTML("There's no significant global batch effect based on the MDMR test.")
      }else{HTML("There's a <strong>significant global batch effect</strong> based on the MDMR test.")}
    })

    output$mdmr_note <- renderUI({
      HTML(paste0("<strong>Note</strong>: The Multivariate Distance Matrix Regression (MDMR) is utilized to evaluate the overall presence of batch effects within the dataset. <br>",
                  "<br>"))
    })


    observeEvent(input$Diagnosis,{
      diag_save_path <- input$diag_save_path

      # Set up harmonization progress notification
      msg <- sprintf('Export diagnosis result')
      withProgress(message = msg, value = 0, {
        setProgress(0.5, 'Exporting...')
        if(length(diag_save_path) > 0 & diag_save_path != ""){
          diag_save(diag_save_path, result)
        }
        setProgress(1, 'Complete!')
      })

      showNotification('Export Successful', type = "message")

      output$output_msg_diag <- renderPrint({
        paste("DataFrame saved to:", input$diag_save_path)
      })

    })


    output$test_batch_table <- DT::renderDT({
      if(input$test_batch == "Kenward-Roger (liner mixed model)"){
        combat_table_gen(result, table_name = "kenward_roger", significant_only = input$batch_significant_only)
      }else if(input$test_batch== "ANOVA"){
        combat_table_gen(result, table_name = "anova", significant_only = input$batch_significant_only)
      }else if(input$test_batch == "Kruskal-Wallis"){
        combat_table_gen(result, table_name = "kruskal_wallis", significant_only = input$batch_significant_only)
      }
    })

    output$test_variance <- DT::renderDT({
      if(input$test_variance == "Fligner-Killeen"){
        combat_table_gen(result, table_name = "fligner_killeen", significant_only = input$variance_significant_only)
      }else if(input$test_variance == "Levene's Test"){
        combat_table_gen(result, table_name = "levenes", significant_only = input$variance_significant_only)
      }else if(input$test_variance == "Bartlett's Test"){
        combat_table_gen(result, table_name = "bartletts", significant_only = input$variance_significant_only)
      }
    })

    output$sig_pct_batch <- shiny::renderText({
      if(input$test_batch == "Kenward-Roger (liner mixed model)"){
        if(type == "lmer"){
          n <- nrow(result$kr_test_df)
          pct <- 100 * (n - sum(is.na(result$kr_test_df$sig)))/n
          HTML(paste0("The percentage of significant features is: <strong>", round(pct,2), "%</strong>."))}else{
            HTML("The Kenward-Roger test is a modification of the degrees of freedom in linear mixed models. Not appropriate for the current type of model.")
          }
      }else if(input$test_batch == "ANOVA"){
        n <- nrow(result$anova_test_df)
        pct <- 100 * (n - sum(is.na(result$anova_test_df$sig)))/n
        HTML(paste0("The percentage of significant features is: <strong>", round(pct,2), "%</strong>."))
      }else if(input$test_batch == "Kruskal-Wallis"){
        n <- nrow(result$kw_test_df)
        pct <- 100 * (n - sum(is.na(result$kw_test_df$sig)))/n
        HTML(paste0("The percentage of significant features is: <strong>", round(pct,2), "</strong>%."))
      }
    })
    output$sig_pct_variance <- shiny::renderText({
      if(input$test_variance == "Fligner-Killeen"){
        n <- nrow(result$fk_test_df)
        pct <- 100 * (n - sum(is.na(result$fk_test_df$sig)))/n
        HTML(paste0("The percentage of significant features is: <strong>", round(pct,2), "</strong>%."))
      }else if(input$test_variance == "Levene's Test"){
        n <- nrow(result$lv_test_df)
        pct <- 100 * (n - sum(is.na(result$lv_test_df$sig)))/n
        HTML(paste0("The percentage of significant features is: <strong>", round(pct,2), "</strong>%."))
      }else if(input$test_variance == "Bartlett's Test"){
        n <- nrow(result$bl_test_df)
        if(n != 0){
          pct <- 100 * (n - sum(is.na(result$bl_test_df$sig)))/n
          HTML(paste0("The percentage of significant features is: <strong>", round(pct,2), "</strong>%."))}else{
            HTML("Bartlett's Test failed due to less than 2 observations in each group.")
          }
      }
    })
  }

  shinyApp(ui = ui, server = server, enableBookmarking = "url")
}


#' Generate Diagnostic Plots for Batch Effect Analysis
#'
#' This function generates a variety of diagnostic plots for analyzing batch effects and their relationships with features and covariates.
#' Depending on the specified plot type, it can create density plots, box plots, residual plots, PCA plots, T-SNE plots, and empirical Bayes diagnostic plots.
#'
#' @param result A list derived from `visual_prep()` that contains datasets and statistical test results for Shiny visualization.
#' @param f A string specifying the feature of interest for visualization.
#' @param batch_control A string indicating whether to include batch-specific controls. Defaults to `"No"`.
#' @param batch_level A vector specifying the batch levels to include in the plot. Used only when `batch_control` is not `"No"`.
#' @param plot_name A string specifying the type of plot to generate. Options include `"batch_density"`, `"cov_feature"`, `"batch_summary"`, `"cov_distribution"`, `"resid_add"`, `"resid_mul"`, `"pca"`, `"tsne"`, `"eb_location"`, and `"eb_scale"`.
#' @param c A string specifying the covariate of interest for `"cov_feature"` or `"cov_distribution"` plots.
#' @param smooth_method A string specifying the smoothing method for trend lines. Defaults to `"lm"` (linear model).
#' @param alpha A numeric value between 0 and 1 controlling the transparency of trend lines. Defaults to `0.2`.
#' @param char_plot_type A string specifying the type of plot for categorical covariates. Options include `"boxplot"`, `"boxplot with points"`, and `"density plot"`. Defaults to `"boxplot"`.
#' @param text_status A string indicating whether to display text annotations in the plot. Defaults to `"No"`.
#' @param color A string indicating whether to use color coding in plots. Defaults to `"No"`.
#' @param label A string indicating whether to include axis labels in the plot. Defaults to `"No"`.
#' @param angle A numeric value specifying the angle of x-axis labels. Defaults to `0`.
#' @param PC1 A string specifying the first principal component for PCA plots.
#' @param PC2 A string specifying the second principal component for PCA plots.
#' @param eb A logical value indicating whether to include empirical Bayes prior information in the plot. Defaults to `TRUE`.
#' @param eb_df A data frame containing empirical Bayes information for generating `eb_location` and `eb_scale` plots.
#'
#' @return A ggplot object representing the specified diagnostic plot.
#'
#' @details
#' The function dynamically generates plots based on the `plot_name` parameter:
#' - `"batch_density"`: Density plots of features by batch levels.
#' - `"cov_feature"`: Covariate vs. feature plots with optional batch adjustments.
#' - `"batch_summary"`: Bar plots summarizing batch-level distributions.
#' - `"cov_distribution"`: Covariate distributions stratified by batch.
#' - `"resid_add"`: Additive residual box plots.
#' - `"resid_mul"`: Multiplicative residual box plots.
#' - `"pca"`: Principal Component Analysis (PCA) plots.
#' - `"tsne"`: T-SNE plots for dimensionality reduction.
#' - `"eb_location"`: Empirical Bayes location parameter density plots.
#' - `"eb_scale"`: Empirical Bayes scale parameter density plots.
#'
#' @examples
#' if(interactive()){
#'  result <- visual_prep(type = "lm", features = "thickness.left.cuneus",
#'  batch = "manufac", covariates = "AGE", df = adni[1:100, ], mdmr = FALSE, cores = 1)
#'  combat_plot_gen(result, f = "thickness.left.cuneus", plot_name = "batch_density")
#'  combat_plot_gen(result, f = "thickness.left.cuneus", c = "AGE", plot_name = "cov_feature")
#' }
#'
#' @export

combat_plot_gen <- function(result, f = NULL, batch_control = "No", batch_level = NULL, plot_name, c = NULL, smooth_method = "lm", alpha = 0.2, char_plot_type = "boxplot", text_status = "No",
                            color = "No", label = "No", angle = 0, PC1 = NULL, PC2 = NULL, eb = TRUE, eb_df = NULL){
  info <- result$info
  type <- info$type
  df <- info$df
  batch <- info$batch
  features <- info$features
  covariates <- info$cov_shiny
  char_var <- info$char_var
  num_var <- setdiff(covariates, char_var)
  if(plot_name == "batch_density"){
    ## batch level density plot
    if(batch_control == "No"){
      ggplot(df, aes(x = .data[[f]])) +
        geom_density(fill = "blue", alpha = 0.3) +
        labs(x = f) +
        theme(
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
        )
    }else{
      overview_sub_df <- df %>% filter(.data[[batch]] %in% batch_level)
      ggplot(overview_sub_df, aes(x = .data[[f]], fill = .data[[batch]])) +
        geom_density(alpha = 0.3) +
        labs(x = f, fill = batch) +
        theme(
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
        )
    }
  }else if(plot_name == "cov_feature"){
    ## covariate vs feature plot
    if (!is.null(covariates)){
      if(c %in% num_var){
        # Dynamic Shiny controls can be unset during the first plot render.
        if (length(alpha) == 0L) alpha <- 0.2
        if(batch_control == "No"){
          ggplot(df, aes(x = .data[[c]], y = .data[[f]])) +
            geom_point() +
            geom_smooth(method = smooth_method, alpha = as.numeric(alpha)) +
            labs(x = c, y = f) +
            theme(
              axis.title.x = element_text(size = 12, face = "bold"),
              axis.title.y = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
            )
        }else{
          overview_sub_df <- df %>% filter(.data[[batch]] %in% batch_level)
          ggplot(overview_sub_df, aes(x = .data[[c]], y = .data[[f]], color = .data[[batch]])) +
            geom_point() +
            geom_smooth(method = smooth_method, aes(fill = .data[[batch]]), alpha = as.numeric(alpha)) +
            labs(x = c, y = f, color = batch, fill = batch) +
            theme(
              axis.title.x = element_text(size = 12, face = "bold"),
              axis.title.y = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
            )
        }
      }else if(c %in% char_var){
        if(char_plot_type == "boxplot"){
          if(batch_control == "No"){
            ggplot(df, aes(x = .data[[c]], y = .data[[f]], fill = .data[[c]])) +
              geom_boxplot() +
              scale_fill_brewer(palette="Pastel1") +
              labs(x = c, y = f, fill = c) +
              theme(
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold"),
              )

          }else{
            overview_sub_df <- df %>% filter(.data[[batch]] %in% batch_level)
            ggplot(overview_sub_df, aes(x = .data[[c]], y = .data[[f]], fill = .data[[batch]])) +
              geom_boxplot() +
              scale_fill_brewer(palette="Pastel1") +
              labs(x = c, y = f, fill = batch) +
              theme(
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold"),
              )
          }
        }else if(char_plot_type == "boxplot with points"){
          if(batch_control == "No"){
            ggplot(df, aes(x = .data[[c]], y = .data[[f]], fill = .data[[c]])) +
              geom_boxplot() +
              geom_jitter(aes(shape = .data[[c]])) +
              scale_fill_brewer(palette="Pastel1") +
              labs(x = c, y = f, fill = c, shape = c) +
              theme(
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold"),
              )
          }else{
            overview_sub_df <- df %>% filter(.data[[batch]] %in% batch_level)
            ggplot(overview_sub_df, aes(x = .data[[c]], y = .data[[f]], fill = .data[[batch]])) +
              geom_boxplot() +
              geom_jitter(aes(shape = .data[[c]])) +
              scale_fill_brewer(palette="Pastel1") +
              labs(x = c, y = f, fill = batch, shape = c) +
              theme(
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold"),
              )
          }
        }else if(char_plot_type == "density plot"){
          ggplot(df, aes(x = .data[[f]], fill = .data[[c]])) +
            geom_density(alpha = 0.3) +
            labs(x = f, fill = c) +
            theme(
              axis.title.x = element_text(size = 12, face = "bold"),
              axis.title.y = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
            )
        }
      }
    }

  }else if(plot_name == "batch_summary"){
    ## batch level distribution plot
    add_plot <- ggplot(info$summary_df %>% filter(remove == "kept"), aes(x = .data[["count"]], y = .data[[batch]])) +
      geom_bar(stat = "identity", fill = "aquamarine") +
      labs(x = "Count", y = "Batch") +
      theme(
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.ticks.y = element_blank())
    if(text_status == "No"){add_plot}else{add_plot + geom_text(aes(label = .data[["count"]]), hjust = -0.2, position = position_dodge(0.9), size = 5, colour = "black")}
  }else if(plot_name == "cov_distribution"){
    ## covariate distribution
    if (!is.null(covariates)){
      if(c %in% num_var){
        ggplot(df, aes(x = .data[[c]], y = reorder(as.factor(.data[[batch]]), .data[[c]], Fun = median), fill = .data[[batch]])) +
          geom_boxplot(alpha = 0.3) +
          labs(x = c, y = "Batch", fill = "Covariate") +
          theme(
            legend.position = "none",
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.ticks.y = element_blank())
      }else if(c %in% char_var){
        df_c <- df %>% group_by(.data[[batch]], .data[[c]]) %>% dplyr::tally() %>% mutate(percentage = .data[["n"]]/sum(.data[["n"]]))
        colnames(df_c) <- c(batch, c, "n", "percentage")
        add_plot <- ggplot(df_c, aes(y = as.factor(.data[[batch]]), x = .data[["n"]], fill = .data[[c]])) +
          geom_bar(stat="identity", position ="fill") +
          scale_fill_brewer(palette = "Pastel1") +
          labs(x = "Percentage", y = "Batch", fill = c) +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.ticks.y = element_blank())
        if(text_status == "No"){add_plot}else{add_plot + geom_text(aes(label = paste0(sprintf("%1.1f", .data[["percentage"]]*100),"%")), position = position_fill(vjust=0.5), colour="black", size = 5)}
      }
    }
  }else if(plot_name == "resid_add"){
    ## additive residual plot
    add_mean <- result$residual_add_df %>% group_by(.data[[batch]]) %>% summarize(across(dplyr::all_of(features), median, .names = "mean_{.col}")) %>% ungroup()
    colnames(add_mean) <- c(batch, colnames(add_mean)[-1])
    result$residual_add_df <- result$residual_add_df %>% left_join(add_mean, by = c(batch))
    if(color == "No"){
      if(batch_control == "No"){
        add_plot <- ggplot(result$residual_add_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]])) +
          geom_boxplot() +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else{
        sub_plot_df <- result$residual_add_df %>% filter(.data[[batch]] %in% batch_level)
        add_plot <- ggplot(sub_plot_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]])) +
          geom_boxplot() +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }

      if(label == "No"){
        add_plot +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
      }else{
        add_plot +
          theme(axis.text.x = element_text(angle = angle, hjust = 0.5, size = 12, face = "bold"),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold")
          )
      }

    }else{

      if(batch_control == "No"){
        add_plot <- ggplot(result$residual_add_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]], fill = .data[[batch]])) +
          geom_boxplot(alpha = 0.3) +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual")
      }else{
        sub_plot_df <- result$residual_add_df %>% filter(.data[[batch]] %in% batch_level)
        add_plot <- ggplot(sub_plot_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]], fill = .data[[batch]])) +
          geom_boxplot(alpha = 0.3) +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }

      if(label == "No"){
        add_plot +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                legend.position = "none")
      }else{
        add_plot +
          theme(axis.text.x = element_text(angle = angle, hjust = 0.5, size = 12, face = "bold"),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold"),
                legend.position = "none"
          )
      }

    }
  }else if(plot_name == "resid_mul"){
    ## multiplicative residual plot
    add_mean <- result$residual_add_df %>% group_by(.data[[batch]]) %>% summarize(across(dplyr::all_of(features), median, .names = "mean_{.col}")) %>% ungroup()
    colnames(add_mean) <- c(batch, colnames(add_mean)[-1])
    result$residual_ml_df <- result$residual_ml_df %>% left_join(add_mean, by = c(batch))
    if(color == "No"){

      if(batch_control == "No"){
        mul_plot <- ggplot(result$residual_ml_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]])) +
          geom_boxplot() +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else{
        sub_plot_df <- result$residual_ml_df %>% filter(.data[[batch]] %in% batch_level)
        mul_plot <- ggplot(sub_plot_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]])) +
          geom_boxplot() +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }
      if(label == "No"){
        mul_plot +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
      }else{
        mul_plot +
          theme(axis.text.x = element_text(angle = angle, hjust = 0.5, size = 12, face = "bold"),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold")
          )
      }
    }else{
      if(batch_control == "No"){
        mul_plot <- ggplot(result$residual_ml_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]], fill = .data[[batch]])) +
          geom_boxplot(alpha = 0.3) +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else{
        sub_plot_df <- result$residual_ml_df %>% filter(.data[[batch]] %in% batch_level)
        mul_plot <- ggplot(sub_plot_df, aes(x = reorder(as.factor(.data[[batch]]), .data[[paste0("mean_",f)]]), y = .data[[f]], fill = .data[[batch]])) +
          geom_boxplot(alpha = 0.3) +
          geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
          labs(x = "Batch", y = "Residual") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }

      if(label == "No"){
        mul_plot +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                legend.position = "none")
      }else{
        mul_plot +
          theme(axis.text.x = element_text(angle = angle, hjust = 0.5, size = 12, face = "bold"),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 12, face = "bold"),
                legend.position = "none"
          )
      }

    }
  }else if(plot_name == "pca"){
    ## PCA plot
    if(batch_control == "No"){
      pca_plot_base <- ggplot(result$pca_df, aes(x = .data[[PC1]], y = .data[[PC2]], color = .data[[batch]])) +
        geom_point() +
        labs(x = PC1, y = PC2, color = "Batch") +
        theme(
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
        )
      if(label == "No"){pca_plot_base + guides(color = "none")}else{pca_plot_base}
    }else{
      sub_pca_df <- result$pca_df %>% filter(.data[[batch]] %in% batch_level)
      pca_plot_base <- ggplot(sub_pca_df, aes(x = .data[[PC1]], y = .data[[PC2]], color = .data[[batch]])) +
        geom_point() +
        labs(x = PC1, y = PC2, color = "Batch") +
        theme(
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
        )
      if(label == "No"){pca_plot_base + guides(color = "none")}else{pca_plot_base}
    }
  }else if(plot_name == "tsne"){
    ## T-SNE plot
    if(batch_control == "No"){
      tsne_plot_base <- ggplot(result$tsne_df, aes(x = .data[["cor_1"]], y = .data[["cor_2"]], color = .data[[batch]])) +
        geom_point() +
        labs(x = "Dim 1", y = "Dim 2", color = "Batch") +
        theme(
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
        )
      if(label == "No"){tsne_plot_base + guides(color = "none")}else{tsne_plot_base}
    }else{
      sub_tsne_df <- result$tsne_df %>% filter(.data[[batch]] %in% batch_level)
      tsne_plot_base <- ggplot(sub_tsne_df, aes(x = .data[["cor_1"]], y = .data[["cor_2"]], color = .data[[batch]])) +
        geom_point() +
        labs(x = "Dim 1", y = "Dim 2", color = "Batch") +
        theme(
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
        )
      if(label == "No"){tsne_plot_base + guides(color = "none")}else{tsne_plot_base}
    }
  }else if(plot_name == "eb_location"){
    ## eb_location plot
    if(eb){
      min_x <- eb_df %>% filter(grepl("^gamma_*", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% min()
      max_x <- eb_df %>% filter(grepl("^gamma_*", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% max()
      if(batch_control == "No"){
        ggplot(eb_df %>% filter(grepl("^gamma_*", .data[["type"]]), .data[["batch"]] != "reference") %>% mutate(type = case_when(.data[["type"]] == "gamma_prior" ~ "EB prior",
                                                                                                                                 .data[["type"]] == "gamma_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], color = .data[["batch"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Gamma", y = "Density", color = "Batch", linetype = "Estimate Type") +
          guides(color = "none") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else{
        ggplot(eb_df %>% filter(grepl("^gamma_*", .data[["type"]]), .data[["batch"]] == batch_level) %>% mutate(type = case_when(.data[["type"]] == "gamma_prior" ~ "EB prior",
                                                                                                                                 .data[["type"]] == "gamma_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Gamma", y = "Density", linetype = "Estimate Type") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }
    }else{
      min_x <- eb_df %>% filter(grepl("gamma_hat", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% min()
      max_x <- eb_df %>% filter(grepl("gamma_hat", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% max()
      if(batch_control == "No"){
        ggplot(eb_df %>% filter(grepl("gamma_hat", .data[["type"]]), .data[["batch"]] != "reference") %>% mutate(type = case_when(.data[["type"]] == "gamma_prior" ~ "EB prior",
                                                                                                                                  .data[["type"]] == "gamma_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], color = .data[["batch"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Gamma", y = "Density", color = "Batch", linetype = "Estimate Type") +
          guides(color = "none") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else{
        ggplot(eb_df %>% filter(grepl("gamma_hat", .data[["type"]]), .data[["batch"]] == batch_level) %>% mutate(type = case_when(.data[["type"]] == "gamma_prior" ~ "EB prior",
                                                                                                                                  .data[["type"]] == "gamma_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Gamma", y = "Density", linetype = "Estimate Type") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }
    }
  }else if(plot_name == "eb_scale"){
    ## eb_scale plot
    if(eb){
      min_x <- eb_df %>% filter(grepl("^delta_*", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% min()
      max_x <- eb_df %>% filter(grepl("^delta_*", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% max()
      if(batch_control == "No"){
        ggplot(eb_df %>% filter(grepl("^delta_*", .data[["type"]]), .data[["batch"]] != "reference") %>% mutate(type = case_when(.data[["type"]] == "delta_prior" ~ "EB prior",
                                                                                                                                 .data[["type"]] == "delta_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], color = .data[["batch"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Delta", y = "Density", color = "Batch", linetype = "Estimate Type") +
          guides(color = "none") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else{
        ggplot(eb_df %>% filter(grepl("^delta_*", .data[["type"]]), .data[["batch"]] == batch_level) %>% mutate(type = case_when(.data[["type"]] == "delta_prior" ~ "EB prior",
                                                                                                                                 .data[["type"]] == "delta_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Delta", y = "Density", linetype = "Estimate Type") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }
    }else{
      if(batch_control == "No"){
        min_x <- eb_df %>% filter(grepl("delta_hat", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% min()
        max_x <- eb_df %>% filter(grepl("delta_hat", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% max()
        ggplot(eb_df %>% filter(grepl("delta_hat", .data[["type"]]), .data[["batch"]] != "reference") %>% mutate(type = case_when(.data[["type"]] == "delta_prior" ~ "EB prior",
                                                                                                                                  .data[["type"]] == "delta_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], color = .data[["batch"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Delta", y = "Density", color = "Batch", linetype = "Estimate Type") +
          guides(color = "none") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else{
        min_x <- eb_df %>% filter(grepl("delta_hat", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% min()
        max_x <- eb_df %>% filter(grepl("delta_hat", .data[["type"]])) %>% pull(.data[["eb_values"]]) %>% max()
        ggplot(eb_df %>% filter(grepl("delta_hat", .data[["type"]]), .data[["batch"]] == batch_level) %>% mutate(type = case_when(.data[["type"]] == "delta_prior" ~ "EB prior",
                                                                                                                                  .data[["type"]] == "delta_hat" ~ "Emprical values")), aes(x = .data[["eb_values"]], linetype = .data[["type"]])) +
          geom_density() +
          xlim(min_x, max_x) +
          labs(x = "Delta", y = "Density", linetype = "Estimate Type") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }
    }
  }

}


#' Generate Diagnostic Tables for Batch Effect Analysis
#'
#' This function generates a variety of tables to summarize data and results from batch effect analyses.
#' Depending on the specified table name, it can create data overview tables, exploratory analysis summaries,
#' statistical test results, PCA summaries, and covariate distributions.
#'
#' @param result A list derived from `visual_prep()` that contains datasets and statistical test results for Shiny visualization.
#' @param table_name A string specifying the type of table to generate. Options include:
#'   - `"data_overview"`: Overview of the dataset, including covariates, features, and batch information.
#'   - `"exploratory_analysis"`: Summary of the selected feature and covariates.
#'   - `"summary_df"`: Summary of batch-level distributions, including batch removal status.
#'   - `"cov_table"`: Covariate summary table, displaying distributions for numeric or categorical covariates.
#'   - `"pc_variance"`: Variance explained by specified principal components.
#'   - `"mdmr"`: Results from the Multivariate Distance Matrix Regression (MDMR) analysis.
#'   - `"kenward_roger"`: Results from Kenward-Roger tests.
#'   - `"anova"`: Results from ANOVA tests.
#'   - `"kruskal_wallis"`: Results from Kruskal-Wallis tests.
#'   - `"fligner_killeen"`: Results from Fligner-Killeen tests.
#'   - `"levenes"`: Results from Levene's tests.
#'   - `"bartletts"`: Results from Bartlett's tests.
#' @param f A string specifying the feature of interest for tables requiring a specific feature. Default is `NULL`.
#' @param c A string specifying the covariate of interest for tables requiring a specific covariate. Default is `NULL`.
#' @param PC1 A string specifying the first principal component for PCA variance tables. Default is `NULL`.
#' @param PC2 A string specifying the second principal component for PCA variance tables. Default is `NULL`.
#' @param significant_only For feature-level batch-effect test tables, show only
#'   Bonferroni-adjusted p-values below 0.05 (default FALSE). Uses the stored
#'   full-precision adjusted p-values, or significance symbols for older results
#'   without numeric values. Does not recalculate tests or multiple-testing
#'   adjustment and does not affect other table types.
#'
#' @return A `DT::datatable` object containing the requested table.
#'
#' @details
#' The function dynamically generates tables based on the `table_name` parameter.
#'
#' @examples
#' if(interactive()){
#'  result <- visual_prep(type = "lm", features = "thickness.left.cuneus",
#'  batch = "manufac", covariates = "AGE", df = adni[1:100, ], mdmr = FALSE, cores = 1)
#'  combat_table_gen(result, table_name = "cov_table", c = "AGE")
#'  combat_table_gen(result, table_name = "pc_variance", PC1 = "PC1", PC2 = "PC2")
#'  }
#'
#' @export


combat_table_gen <- function(result, table_name, f = NULL, c = NULL, PC1 = NULL, PC2 = NULL,
                             significant_only = FALSE){
  test_tables <- c(kenward_roger = "kr_test_df", anova = "anova_test_df",
    kruskal_wallis = "kw_test_df", fligner_killeen = "fk_test_df",
    levenes = "lv_test_df", bartletts = "bl_test_df")
  if (isTRUE(significant_only) && table_name %in% names(test_tables)) {
    key <- test_tables[[table_name]]
    tests <- result[[key]]
    if (nrow(tests) > 0L) {
      # visual_prep() stores Bonferroni-adjusted values in p.value.raw before
      # formatting p.value; use that precision rather than the display strings.
      significant <- if (is.numeric(tests$p.value.raw))
        is.finite(tests$p.value.raw) & tests$p.value.raw < .05 else
        tests$sig %in% c("*", "**", "***")
      result[[key]] <- tests[significant, , drop = FALSE]
    }
  }
  info <- result$info
  type <- info$type
  df <- info$df
  batch <- info$batch
  features <- info$features
  covariates <- info$cov_shiny
  char_var <- info$char_var
  num_var <- setdiff(covariates, char_var)
  other <- setdiff(colnames(df), c(batch, covariates, features))
  df_show <- df[c(batch, covariates, features, other)]
  if(table_name == "data_overview"){
    df_show %>% DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                    targets = "_all")))) %>% formatStyle(
                                                                      covariates,
                                                                      backgroundColor = "pink"
                                                                    ) %>% formatStyle(
                                                                      features,
                                                                      backgroundColor = "lightyellow"
                                                                    ) %>% formatStyle(
                                                                      batch,
                                                                      backgroundColor = "lightblue"
                                                                    )
  }else if(table_name == "exploratory_analysis"){
    df_show %>% dplyr::select(all_of(c(batch, covariates, f))) %>% DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                                                                       targets = "_all")))) %>% formatStyle(
                                                                                                                         covariates,
                                                                                                                         backgroundColor = "pink"
                                                                                                                       ) %>% formatStyle(
                                                                                                                         f,
                                                                                                                         backgroundColor = "lightyellow"
                                                                                                                       ) %>% formatStyle(
                                                                                                                         batch,
                                                                                                                         backgroundColor = "lightblue"
                                                                                                                       )
  }else if(table_name == "summary_df"){
    info$summary_df %>% mutate(`percentage (%)` = sprintf("%.3f", .data[["percentage (%)"]])) %>% arrange(desc(.data[["remove"]])) %>%
      DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                          targets = "_all")))) %>% formatStyle(
                                                            'remove',
                                                            target = 'row',
                                                            backgroundColor = styleEqual(c("removed"), "lightyellow")
                                                          )
  }else if(table_name == "cov_table"){
    if (!is.null(covariates)){
      if(c %in% num_var){
        cov_summary_table <- df %>% group_by(.data[[batch]]) %>% summarize(min = min(.data[[c]]), mean = mean(.data[[c]]), max = max(.data[[c]]))
        colnames(cov_summary_table) <- c(batch, "min", "mean", "max")
        cov_summary_table <- cov_summary_table %>% mutate(mean = sprintf("%.3f", .data[["mean"]]),
                                                          min = sprintf("%.3f", .data[["min"]]),
                                                          max = sprintf("%.3f", .data[["max"]]))
        cov_summary_table %>% DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                                  targets = "_all"))))
      }else if(c %in% char_var){
        cov_summary_table <- df %>% group_by(.data[[batch]], .data[[c]]) %>% dplyr::tally() %>% mutate(percentage = 100*.data[["n"]]/sum(.data[["n"]]))
        colnames(cov_summary_table) <- c(batch, c, "n", "percentage (%)")
        cov_summary_table %>% mutate(`percentage (%)` = sprintf("%.3f", .data[["percentage (%)"]])) %>% DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                                                                                                            targets = "_all"))))
      }
    }

  }else if(table_name == "pc_variance"){
    pca_table <- result$pca_summary %>% filter(.data[["Principal_Component"]] %in% c(PC1, PC2)) %>% dplyr::select(dplyr::all_of(c("Principal_Component", "Variance_Explained")))
    sum_total_variance <- sum(pca_table$Variance_Explained)
    pca_table %>% add_row(Principal_Component = "Total", Variance_Explained = sum_total_variance) %>% mutate(Variance_Explained = sprintf("%.3f", .data[["Variance_Explained"]])) %>%
      DT::datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  }else if(table_name == "mdmr"){
    result$mdmr.summary %>% DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                                targets = "_all")))) %>% formatStyle(columns = c("p.value"),color = styleEqual(result$red, "red")) %>% formatStyle(
                                                                                  'sig',
                                                                                  target = 'row',
                                                                                  backgroundColor = styleEqual(c("*", "**", "***"), "lightyellow"))
  }else if(table_name == "kenward_roger"){
    if(type == "lmer"){
      result$kr_test_df %>% dplyr::select(dplyr::all_of(c("feature", "p.value", "sig"))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% formatStyle(columns = c("p.value"),color = styleEqual(result$red, "red")) %>%
        formatStyle('sig', target = 'row', backgroundColor = styleEqual(c("*", "**", "***"), "lightyellow"))}else{result$kr_test_df %>% DT::datatable()}
  }else if(table_name == "anova"){
    result$anova_test_df %>% dplyr::select(dplyr::all_of(c("feature", "p.value", "sig"))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% formatStyle(columns = c("p.value"),color = styleEqual(result$red, "red")) %>% formatStyle(
      'sig',
      target = 'row',
      backgroundColor = styleEqual(c("*", "**", "***"), "lightyellow"))
  }else if(table_name == "kruskal_wallis"){
    result$kw_test_df %>% dplyr::select(dplyr::all_of(c("feature", "p.value", "sig"))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                                                                                                    targets = "_all")))) %>% formatStyle(columns = c("p.value"),color = styleEqual(result$red, "red")) %>% formatStyle(
                                                                                                                                                      'sig',
                                                                                                                                                      target = 'row',
                                                                                                                                                      backgroundColor = styleEqual(c("*", "**", "***"), "lightyellow"))
  }else if(table_name == "fligner_killeen"){
    result$fk_test_df %>% dplyr::select(dplyr::all_of(c("feature", "p.value", "sig"))) %>% datatable(extensions = 'Buttons', options = list(columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                            targets = "_all")))) %>% formatStyle(columns = c("p.value"),color = styleEqual(result$red, "red")) %>% formatStyle(
                                                                                                                                                                              'sig',
                                                                                                                                                                              target = 'row',
                                                                                                                                                                              backgroundColor = styleEqual(c("*", "**", "***"), "lightyellow"))
  }else if(table_name == "levenes"){
    result$lv_test_df %>% dplyr::select(dplyr::all_of(c("feature", "p.value", "sig"))) %>% datatable(extensions = 'Buttons', options = list(columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                            targets = "_all")))) %>% formatStyle(columns = c("p.value"),color = styleEqual(result$red, "red")) %>% formatStyle(
                                                                                                                                                                              'sig',
                                                                                                                                                                              target = 'row',
                                                                                                                                                                              backgroundColor = styleEqual(c("*", "**", "***"), "lightyellow"))
  }else if(table_name == "bartletts"){
    if(all(c("feature", "p.value", "sig") %in% names(result$bl_test_df))){
      result$bl_test_df %>% dplyr::select(dplyr::all_of(c("feature", "p.value", "sig"))) %>% datatable(extensions = 'Buttons', options = list(columnDefs = list(list(className = 'dt-center',
                                                                                                                                                                              targets = "_all")))) %>% formatStyle(columns = c("p.value"),color = styleEqual(result$red, "red")) %>% formatStyle(
                                                                                                                                                                                'sig',
                                                                                                                                                                                target = 'row',
                                                                                                                                                                                backgroundColor = styleEqual(c("*", "**", "***"), "lightyellow"))}else{
                                                                                                                                                                                  result$bl_test_df %>% DT::datatable()
                                                                                                                                                                                }

  }
}

