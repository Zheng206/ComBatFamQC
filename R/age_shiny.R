#' Lifespan Age Trends
#'
#' Provide estimated lifespan age trends of neuroimaging-derived brain structures through shiny app.
#'
#' @param age_list A list containing all ROIs' true volumes, age trend estimates, and the fitted GAMLSS model.
#' @param features A vector of roi names.
#' @param quantile_type A vector of quantile types (e.g., `c("quantile_25", "median", "quantile_75")`)
#' @param use_plotly A boolean variable that indicates whether to display the age plot using the `plotly` package.
#'
#' @importFrom gamlss gamlss gamlss.control predictAll getQuantile ps pb
#' @importFrom gamlss.dist BCT NO
#' @importFrom utils head
#' @importFrom stats setNames
#'
#' @return This function does not return a value. It launches a Shiny app.
#'
#' @export
#'
#' @details
#' When this function is called, it starts a Shiny application in the
#' user's default web browser. Execution is blocked until the app is closed.
#'
#' @examples
#' features <- colnames(age_df)[c(6:56)]
#' sub_df <- age_df[,c(features[1], "age", "sex", "ICV_baseline")] |> na.omit()
#' colnames(sub_df) <- c("y", "age", "sex", "icv")
#' age_list <- list("Volume_1" = age_list_gen(sub_df = sub_df))
#' quantile_type <- c("quantile_25", "median", "quantile_75")
#' if(interactive()){
#'   age_shiny(age_list = age_list, features = "Volume_1", quantile_type = quantile_type)
#' }


age_shiny <- function(age_list, features, quantile_type, use_plotly = TRUE){
  quantile_type <- quantile_type
  plotly_package <- requireNamespace("plotly", quietly = TRUE)
  use_plotly <- use_plotly && plotly_package
  ui <- function(request) {
    fluidPage(
      theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
      titlePanel("LIFESPAN Age Trends"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            shinydashboard::box(
              width = NULL,
              title = "Age Trend View Control",
              selectInput("features", "Select ROI", choices = features, selected = features[1]),
              radioButtons("sex", "Sex control", choices = c("Female", "Male", "Female vs. Male (Only for visualization)"), selected = "Female"),
              radioButtons("quantile", "Select the quantile level", choices = c(quantile_type, "customization"), selected = quantile_type[1]),
              uiOutput("cus_quantile"))),
          fluidRow(
            shinydashboard::box(
              title = "Age Trend Table Export",
              width = NULL,
              textInput("age_save_path", "Save age trend table to:"),
              actionButton("Export", "Export Age Trend Table"),
              verbatimTextOutput("output_msg_age"))),
          fluidRow(
            shinydashboard::box(
              width = NULL,
              title = "GAMLSS Model Export",
              textInput("gamlss_save_path", "Save GAMLSS Model to:"),
              actionButton("gamlss_model", "Save GAMLSS Model"),
              verbatimTextOutput("output_msg_gamlss")
            )
          )
          ),
        mainPanel(
          tabsetPanel(
            tabPanel("Age Trend",
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Age Trend Plots",
                         uiOutput("plotly_check"))),
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "Age Trend Table",
                         DT::DTOutput("agetable")))
                     )
          )
        )
      )
    )
  }

  server <- function(input, output, session) {
    cus_result_s <- reactiveVal(NULL)
    output$cus_quantile <- renderUI({
      if(input$quantile == "customization"){
        sliderInput(
          inputId = "quantile_selection",
          label = "Select a quantile for the age trend",
          value = 0.75,
          min = 0.01,
          max = 0.99,
          step = 0.01
        )
      }
    })

    output$plotly_check <- renderUI({
      if(use_plotly){
        plotly::plotlyOutput("ageplot")
      }else{shiny::plotOutput("ageplot")}
    })

    observeEvent(input$Export,{
      age_save_path <- input$age_save_path

      # Set up harmonization progress notification
      msg <- sprintf('Export age trend table')
      withProgress(message = msg, value = 0, {
        setProgress(0.5, 'Exporting...')
        if(length(age_save_path) > 0 & age_save_path != ""){
          age_save(age_save_path, age_list)
        }
        setProgress(1, 'Complete!')
      })

      showNotification('Export Successful', type = "message")

      output$output_msg_age <- renderPrint({
        paste("Age trend table saved to:", input$age_save_path)
      })

    })

    observeEvent(input$gamlss_model,{
      model_save_path <- input$gamlss_save_path
      msg <- sprintf('Saving GAMLSS Model')
      withProgress(message = msg, value = 0, {
        setProgress(0.5, 'Saving ...')
        gamlss_model <- lapply(seq_len(length(age_list)), function(i){
          g_model <- age_list[[i]]$model
          return(g_model)})
        names(gamlss_model) <- names(age_list)

        saveRDS(gamlss_model, file = model_save_path)
        setProgress(1, 'Complete!')
      })
      showNotification('GAMLSS Model Successfully Saved', type = "message")

      output$output_msg_gamlss <- renderPrint({
        paste("GAMLSS model saved to:", input$gamlss_save_path)
      })
    })

    output$ageplot <- if(use_plotly){
      plotly::renderPlotly({
        base_plot <- age_trend_plot(age_list, f = input$features, s = "none", q = input$quantile, use_plotly = TRUE)
        if(input$quantile != "customization"){
          if(input$sex == "Female"){
            age_trend_plot(age_list, f = input$features, s = "F", q = input$quantile, use_plotly = TRUE)
          }else if(input$sex == "Male"){
            age_trend_plot(age_list, f = input$features, s = "M", q = input$quantile, use_plotly = TRUE)
          }else if(input$sex == "Female vs. Male (Only for visualization)"){
            age_trend_plot(age_list, f = input$features, s = "F vs M", q = input$quantile, use_plotly = TRUE)
          }
        }else{
          cus_list <- cus_result_gen(age_list, customized_q = input$quantile_selection, f = input$features)
          cus_result_s(cus_list$cus_result)
          if(input$sex == "Female"){
            age_trend_plot(age_list, f = input$features, s = "F", q = "customization", cus_list = cus_list, use_plotly = TRUE)
          }else if(input$sex == "Male"){
            age_trend_plot(age_list, f = input$features, s = "M", q = "customization", cus_list = cus_list, use_plotly = TRUE)
          }else if(input$sex == "Female vs. Male (Only for visualization)"){
            age_trend_plot(age_list, f = input$features, s = "F vs M", q = "customization", cus_list = cus_list, use_plotly = TRUE)
          }
        }
      })
    }else{
      renderPlot({
        base_plot <- age_trend_plot(age_list, f = input$features, s = "none", q = input$quantile, use_plotly = FALSE)
        if(input$quantile != "customization"){
          if(input$sex == "Female"){
            age_trend_plot(age_list, f = input$features, s = "F", q = input$quantile, use_plotly = FALSE)
          }else if(input$sex == "Male"){
            age_trend_plot(age_list, f = input$features, s = "M", q = input$quantile, use_plotly = FALSE)
          }else if(input$sex == "Female vs. Male (Only for visualization)"){
            age_trend_plot(age_list, f = input$features, s = "F vs M", q = input$quantile, use_plotly = FALSE)
          }
        }else{
          cus_list <- cus_result_gen(age_list, customized_q = input$quantile_selection, f = input$features)
          cus_result_s(cus_list$cus_result)
          if(input$sex == "Female"){
            age_trend_plot(age_list, f = input$features, s = "F", q = "customization", cus_list = cus_list, use_plotly = FALSE)
          }else if(input$sex == "Male"){
            age_trend_plot(age_list, f = input$features, s = "M", q = "customization", cus_list = cus_list, use_plotly = FALSE)
          }else if(input$sex == "Female vs. Male (Only for visualization)"){
            age_trend_plot(age_list, f = input$features, s = "F vs M", q = "customization", cus_list = cus_list, use_plotly = FALSE)
          }
        }
      })
    }

    output$agetable <- DT::renderDT({
      result <- age_list[[input$features]]
      if(input$quantile != "customization"){
        if(input$sex == "Female"){
          age_table_gen(result = result, q = input$quantile, s = "F")
        }else if(input$sex == "Male"){
          age_table_gen(result = result, q = input$quantile, s = "M")
        }else if(input$sex == "Female vs. Male (Only for visualization)"){
          age_table_gen(result = result, q = input$quantile, s = "F vs M")
        }
      }else{
        cus_result <- cus_result_s()
        if(input$sex == "Female"){
          age_table_gen(result = cus_result, q = paste0("quantile_", 100*input$quantile_selection), s = "F")
        }else if(input$sex == "Male"){
          age_table_gen(result = cus_result, q = paste0("quantile_", 100*input$quantile_selection), s = "M")
        }else if(input$sex == "Female vs. Male (Only for visualization)"){
          age_table_gen(result = cus_result, q = paste0("quantile_", 100*input$quantile_selection), s = "F vs M")
        }

      }
    })
  }
  shinyApp(ui = ui, server = server)
}

#' Age Trend Estimates Generation
#'
#' A GAMLSS model using a Box-Cox t distribution was fitted separately to rois of interest,
#' to establish normative reference ranges as a function of age for the volume of a specific roi.
#'
#' @param sub_df A two-column dataset that contains age and roi volume related information. column y: roi volumes, column age: age.
#' @param lq The lower bound quantile. eg: 0.25, 0.05
#' @param hq The upper bound quantile. eg: 0.75, 0.95
#' @param mu An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the mu formula.
#' "smooth": y ~ pb(age), "linear": y ~ age, "default": y ~ 1.
#' @param sigma An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the sigma formula.
#' "smooth": ~ pb(age), "linear": ~ age, "default": ~ 1.
#' @param nu An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the nu formula.
#' "smooth": ~ pb(age), "linear": ~ age, "default": ~ 1.
#' @param tau An indicator of whether to smooth age variable, include it as a linear term or only include the intercept in the tau formula.
#' "smooth": ~ pb(age), "linear": ~ age, "default": ~ 1.
#'
#' @return `age_list_gen` returns a list containing the following components:
#' \item{true_df}{a dataframe contains the true age and ROI volume information}
#' \item{predicted_df_sex}{a dataframe contains the estimated age trend adjusting sex and icv}
#' \item{model}{the fitted GAMLSS model}
#'
#' @export
#'
#' @examples
#' features <- colnames(age_df)[c(6:56)]
#' sub_df <- age_df[,c(features[1], "age", "sex", "ICV_baseline")] |> na.omit()
#' colnames(sub_df) <- c("y", "age", "sex", "icv")
#' age_list_gen(sub_df = sub_df)


age_list_gen <- function(sub_df, lq = 0.25, hq = 0.75, mu = "smooth", sigma = "smooth", nu= "default", tau = "default"){

  feature <- colnames(sub_df)[1]
  if(mu == "smooth") {
    mu_form_sex <- as.formula(paste0(feature, " ~ pb(age) + sex + icv"))
    }else if(mu == "linear"){
    mu_form_sex <- as.formula(paste0(feature, " ~ age + sex + icv"))
    }else if(mu == "default"){
      mu_form_sex <- as.formula(paste0(feature, " ~ sex + icv"))
    }

  if(sigma == "smooth") {
    sig_form <- as.formula("~ pb(age)")
  }else if(sigma == "linear"){
    sig_form <- as.formula("~ age")
  }else if(sigma == "default"){
    sig_form <- as.formula("~ 1")
  }

  if(nu == "smooth") {
    nu_form <- as.formula("~ pb(age)")
  }else if(nu == "linear"){
    nu_form <- as.formula("~ age")
  }else if(nu == "default"){
    nu_form <- as.formula("~ 1")
  }

  if(tau == "smooth") {
    tau_form <- as.formula("~ pb(age)")
  }else if(tau == "linear"){
    tau_form <- as.formula("~ age")
  }else if(tau == "default"){
    tau_form <- as.formula("~ 1")
  }

  mdl_sex <- gamlss(mu_form_sex,
               sigma.formula=sig_form,
               nu.formula=nu_form,
               tau.formula=tau_form,
               family=NO(),
               data = sub_df,
               control = gamlss.control(n.cyc = 100))

  # predict hypothetical data
  min_age <- min(sub_df[["age"]])
  max_age <- max(sub_df[["age"]])
  age_test <- seq(from = min_age, to = max_age,length.out = 1000)
  mean_icv <- mean(sub_df$icv)

  quantiles <- c(lq, 0.5, hq)

  age_df_sex <- lapply(c("F", "M"), function(x){
    predictions_quantiles_female <- matrix(data=0,ncol=3,nrow=1000)
    for (i in 1:length(quantiles)){
      Qua <- getQuantileRefactored(obj = mdl_sex, quantile = quantiles[i], term="age", fixed.at=list(sex = x, icv = mean_icv), data = sub_df)[[1]]
      predictions_quantiles_female[,i] <- Qua(age_test)
    }
    colnames(predictions_quantiles_female) <- c(paste0("quantile_", 100*lq), "median", paste0("quantile_", 100*hq))
    age_df_sex <- data.frame(cbind(age = age_test, predictions_quantiles_female)) %>%
      pivot_longer(colnames(predictions_quantiles_female), names_to = "type", values_to = "prediction") %>% mutate(sex = x)
  }) %>% bind_rows()

  return_list <- list("true_df" = sub_df, "predicted_df_sex" = age_df_sex, "model" = mdl_sex)
  return(return_list)
}

#' Generate Predicted Quantiles for Age Trends
#'
#' This function computes predicted quantiles for a specified feature and demographic group based on a GAMLSS model.
#' The function interpolates predictions over a range of ages while accounting for fixed covariates.
#'
#' @param age_list A list containing all ROIs' true volumes, age trend estimates, and the fitted GAMLSS model.
#' @param feature A string specifying the feature of interest within the `age_list`.
#' @param q A numeric value between 0 and 1 representing the quantile to predict (e.g., `0.5` for the median).
#' @param s A string indicating the gender of the group for which the predictions are generated (e.g., `"F"` for female, `"M"` for male).
#'
#' @return A data frame containing columns for age, quantile type, prediction, and sex.
#'
#' @details
#' This function uses a GAMLSS model to generate predictions for a specified quantile and demographic group.
#' The predictions are computed over a sequence of ages (`age_test`) that spans the observed age range in the data.
#' The function adjusts for fixed covariates such as `icv` by using their mean values from the input data.
#'
#' @export
#'
#' @examples
#' # Initialize result to NULL for safety
#' age_list <- NULL
#'
#' # Check if the previous results file exists and load it
#' if (file.exists("./tests/testthat/previous-results/age_list.rds")) {
#'   age_list <- readRDS("./tests/testthat/previous-results/age_list.rds")
#' }
#'
#' # Use the result if it is available
#' if (!is.null(age_list)) {
#'   customize_percentile(age_list, feature = "Volume_1", q = 0.5, s = "F")
#' } else {
#'   message("Age list is NULL. Please ensure the file exists and is accessible.")
#' }

customize_percentile <- function(age_list, feature, q = 0.75, s = "F"){
  mdl_sex <- age_list[[feature]]$model
  sub_df <- age_list[[feature]]$true_df
  min_age <- min(sub_df[["age"]])
  max_age <- max(sub_df[["age"]])
  age_test <- seq(from = min_age, to = max_age,length.out = 1000)
  mean_icv <- mean(sub_df$icv)
  Qua <- getQuantileRefactored(obj = mdl_sex, quantile = q, term="age", fixed.at=list(sex = s, icv = mean_icv), data = sub_df)[[1]]
  predictions_quantiles_female <- Qua(age_test)
  age_df_sex <- data.frame(cbind(age = age_test, type = paste0("quantile_", 100*q), prediction = predictions_quantiles_female, sex = s)) %>%
    mutate(age = as.numeric(.data[["age"]]), prediction = as.numeric(.data[["prediction"]]))
  return(age_df_sex)
}


#' Generate Customized Predicted Quantiles List
#'
#' This function computes customized predicted quantiles for a specified feature across both female and male groups
#' using a GAMLSS model. The resulting list object is structured for visualization purposes.
#'
#' @param age_list A list containing all ROIs' true volumes, age trend estimates, and the fitted GAMLSS model.
#' @param customized_q A numeric value between 0 and 1 representing the quantile to predict (e.g., `0.75` for the 75th percentile).
#' @param f A string specifying the feature of interest within the `age_list`.
#'
#' @return A list containing the customized quantile value used for predictions and a data frame with columns for age, quantile type, prediction, and sex.
#'
#' @details
#' This function utilizes the GAMLSS model to compute predictions for the specified quantile across demographic groups.
#' The results include predictions for both the specified quantile and the median, enabling enhanced visualization.
#'
#' @export
#'
#' @examples
#' # Initialize result to NULL for safety
#' age_list <- NULL
#'
#' # Check if the previous results file exists and load it
#' if (file.exists("./tests/testthat/previous-results/age_list.rds")) {
#'   age_list <- readRDS("./tests/testthat/previous-results/age_list.rds")
#' }
#'
#' # Use the result if it is available
#' if (!is.null(age_list)) {
#'   cus_result_gen(age_list, customized_q = 0.75, f = "Volume_1")
#' } else {
#'   message("Age list is NULL. Please ensure the file exists and is accessible.")
#' }

cus_result_gen <- function(age_list, customized_q = 0.75, f){
  result <- age_list[[f]]
  cus_result <- lapply(c("F", "M"), function(x) customize_percentile(age_list, f, customized_q, x)) |> bind_rows()
  cus_result <- rbind(cus_result, result$predicted_df_sex %>% filter(.data[["type"]] == "median"))
  return(list("customized_q" = customized_q, "cus_result" = cus_result))
}


#' Generate Age Trend Plot
#'
#' This function creates an age trend plot for a specified feature and demographic group based on GAMLSS model predictions.
#' The function supports both static plots using `ggplot2` and interactive plots using `plotly`.
#'
#' @param age_list A list containing all ROIs' true volumes, age trend estimates, and the fitted GAMLSS model.
#' @param f A string specifying the feature of interest within the `age_list`.
#' @param s A string indicating the demographic group for visualization: `"F"` for female, `"M"` for male, `"F vs M"` for gender comparison, or `"none"` for base plot (true data) visualization.
#' @param q A string specifying the quantile type (e.g., `"quantile_75"`, `"median"`, or `"customization"`).
#' @param cus_list A list object containing customized quantile predictions generated by the `cus_result_gen` function.
#' @param use_plotly A boolean indicating whether to create an interactive plot using `plotly` (default: `TRUE`).
#'
#' @return A plot object:
#' \itemize{
#'   \item If \code{use_plotly = TRUE}, returns a `plotly` interactive plot.
#'   \item If \code{use_plotly = FALSE}, returns a `ggplot2` static plot.
#' }
#'
#' @details
#' The function overlays true data points with predicted quantile trends for the specified feature and demographic group.
#' It supports customization for quantile visualization and uses precomputed results from the `cus_result_gen` function
#' for enhanced flexibility.
#'
#' @export
#'
#' @examples
#' # Initialize result to NULL for safety
#' age_list <- NULL
#'
#' # Check if the previous results file exists and load it
#' if (file.exists("./tests/testthat/previous-results/age_list.rds")) {
#'   age_list <- readRDS("./tests/testthat/previous-results/age_list.rds")
#' }
#'
#' # Use the result if it is available
#' if (!is.null(age_list)) {
#'   customized_results <- cus_result_gen(age_list, customized_q = 0.75, f = "Volume_1")
#'   age_trend_plot(
#'     age_list = age_list,
#'     f = "Volume_1",
#'     s = "F",
#'     q = "customization",
#'     cus_list = customized_results,
#'     use_plotly = TRUE
#'   )
#' } else {
#'   message("Age list is NULL. Please ensure the file exists and is accessible.")
#' }

age_trend_plot <- function(age_list, f, s = "none", q = "median", cus_list = NULL, use_plotly = TRUE){
  result <- age_list[[f]]
  pal <- c("coral", "olivedrab")
  pal <- setNames(pal, c("F", "M"))
  if(use_plotly){
    base_plot <- plotly::plot_ly(colors = pal) %>%
      plotly::add_trace(
        data = result$true_df,
        x = ~.data[["age"]],
        y = ~.data[[f]],
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "rgba(70, 130, 150, 0.5)",
          size = 6
        ),
        hoverinfo = "text",
        text = ~paste("Age: ", round(.data[["age"]], 2), "<br>Volume: ", round(.data[[f]],2)),
        name = "True Data"
      ) %>% plotly::layout(
        xaxis = list(title = "Age", titlefont = list(size = 14, color = "black")),
        yaxis = list(title = "ROI Volume", titlefont = list(size = 14, color = "black")),
        legend = list(title = list(text = "Legend"), font = list(size = 12)),
        margin = list(l = 50, r = 50, b = 50, t = 50),
        font = list(family = "Arial", size = 12)
      )
    if(q != "customization"){
      if(s == "none"){
        base_plot
      }else if(s == "F"){
        base_plot %>%
          plotly::add_trace(
            data = result$predicted_df_sex %>% filter(.data[["sex"]] == "F"),
            x = ~.data[["age"]],
            y = ~.data[["prediction"]],
            type = "scatter",
            mode = "lines",
            line = list(color = "coral"),
            linetype = ~.data[["type"]],
            hoverinfo = "text",
            text = ~paste("Age: ", round(.data[["age"]], 2), "<br>Volume: ", round(.data[["prediction"]], 2))
          )
      }else if(s == "M"){
        base_plot %>%
          plotly::add_trace(
            data = result$predicted_df_sex %>% filter(.data[["sex"]] == "M"),
            x = ~.data[["age"]],
            y = ~.data[["prediction"]],
            type = "scatter",
            mode = "lines",
            line = list(color = "olivedrab"),
            linetype = ~.data[["type"]],
            hoverinfo = "text",
            text = ~paste("Age: ", round(.data[["age"]], 2), "<br>Volume: ", round(.data[["prediction"]], 2))
          )
      }else if(s == "F vs M"){
        base_plot %>%
          plotly::add_trace(
            data = result$predicted_df_sex %>% filter(.data[["type"]] == q),
            x = ~.data[["age"]],
            y = ~.data[["prediction"]],
            type = "scatter",
            mode = "lines",
            color = ~.data[["sex"]],
            hoverinfo = "text",
            text = ~paste("Age: ", round(.data[["age"]], 2), "<br>Volume: ", round(.data[["prediction"]], 2))
          )
      }
    }else{
      cus_result <- cus_list$cus_result
      customized_q <- cus_list$customized_q
      if(s == "none"){
        base_plot
      }else if(s == "F"){
        base_plot %>%
          plotly::add_trace(
            data = cus_result %>% filter(.data[["sex"]] == "F"),
            x = ~.data[["age"]],
            y = ~.data[["prediction"]],
            type = "scatter",
            mode = "lines",
            line = list(color = "coral"),
            linetype = ~.data[["type"]],
            hoverinfo = "text",
            text = ~paste("Age: ", round(.data[["age"]], 2), "<br>Volume: ", round(.data[["prediction"]], 2))
          )
      }else if(s == "M"){
        base_plot %>%
          plotly::add_trace(
            data = cus_result %>% filter(.data[["sex"]] == "M"),
            x = ~.data[["age"]],
            y = ~.data[["prediction"]],
            type = "scatter",
            mode = "lines",
            line = list(color = "olivedrab"),
            linetype = ~.data[["type"]],
            hoverinfo = "text",
            text = ~paste("Age: ", round(.data[["age"]], 2), "<br>Volume: ", round(.data[["prediction"]], 2))
          )
      }else if(s == "F vs M"){
        base_plot %>%
          plotly::add_trace(
            data = cus_result %>% filter(.data[["type"]] == paste0("quantile_", 100*customized_q)),
            x = ~.data[["age"]],
            y = ~.data[["prediction"]],
            type = "scatter",
            mode = "lines",
            color = ~.data[["sex"]],
            hoverinfo = "text",
            text = ~paste("Age: ", round(.data[["age"]], 2), "<br>Volume: ", round(.data[["prediction"]], 2))
          )
      }
    }
  }else{
    base_plot <- ggplot(result$true_df, aes(x = .data[["age"]], y = .data[[f]])) +
      geom_point(color = "cornflowerblue", alpha = 0.6) +
      labs(x = "Age", y = "ROI Volume") +
      theme(
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
      )
    if(q != "customization"){
      if(s == "none"){
        base_plot
      }else if(s == "F"){
        base_plot + geom_line(data = result$predicted_df_sex %>% filter(.data[["sex"]] == "F"), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], linetype = .data[["type"]]), color = "coral")
      }else if(s == "M"){
        base_plot + geom_line(data = result$predicted_df_sex %>% filter(.data[["sex"]] == "M"), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], linetype = .data[["type"]]), color = "olivedrab")
      }else if(s == "F vs M"){
        base_plot + geom_line(data = result$predicted_df_sex %>% filter(.data[["type"]] == q), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], color = .data[["sex"]])) +
          scale_color_manual(
            values = c("F" = "coral", "M" = "olivedrab"),
            name = "Sex"
          )
      }
    }else{
      cus_result <- cus_list$cus_result
      customized_q <- cus_list$customized_q
      if(s == "none"){
        base_plot
      }else if(s == "F"){
        base_plot + geom_line(data = cus_result %>% filter(.data[["sex"]] == "F"), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], linetype = .data[["type"]]), color = "coral")
      }else if(s == "M"){
        base_plot + geom_line(data = cus_result %>% filter(.data[["sex"]] == "M"), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], linetype = .data[["type"]]), color = "olivedrab")
      }else if(s == "F vs M"){
        base_plot + geom_line(data = cus_result %>% filter(.data[["type"]] == paste0("quantile_", 100*customized_q)), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], color = .data[["sex"]]))
      }
    }
  }
}


#' Generate Age Trend Summary Table
#'
#' This function generates a summary table of age trend predictions for a specified quantile and demographic group.
#' The table includes the predicted volume values, percentage changes between age intervals, and other details
#' for either females, males, or a comparison between both genders.
#'
#' @param result A data object containing prediction results and metadata.
#' @param q A string specifying the quantile of interest (e.g., `"quantile_50"` for the median).
#' @param s A string indicating the demographic group for which the summary table is generated:
#'   \itemize{
#'     \item `"F"` for female
#'     \item `"M"` for male
#'     \item `"F vs M"` for comparison between females and males
#'   }
#'
#' @return A `datatable` object (from the `DT` package) containing the age trend summary table with the following columns:
#'   \itemize{
#'     \item `Age`: The age values at regular intervals (rounded to the nearest 10).
#'     \item `Percentile.Volume`: The predicted volume values for the specified quantile (only for females or males).
#'     \item `PercentageChange (%)`: The percentage change in volume between consecutive age intervals (only for females or males).
#'     \item `Percentile.Volume_F`: The predicted volume values for females (when comparing genders).
#'     \item `Percentile.Volume_M`: The predicted volume values for males (when comparing genders).
#'     \item `PercentageChange_F (%)`: The percentage change for females (when comparing genders).
#'     \item `PercentageChange_M (%)`: The percentage change for males (when comparing genders).
#'   }
#'
#' @details
#' The function processes the input data to filter predictions based on the specified quantile and demographic group.
#' It calculates percentage changes in predicted volume values for easier interpretation of trends. For gender comparisons
#' (`"F vs M"`), it generates side-by-side columns for females and males.
#'
#' The output table is formatted using the `DT` package with additional features, such as CSV and Excel export options.
#'
#' @examples
#' # Initialize result to NULL for safety
#' age_list <- NULL
#'
#' # Check if the previous results file exists and load it
#' if (file.exists("./tests/testthat/previous-results/age_list.rds")) {
#'   age_list <- readRDS("./tests/testthat/previous-results/age_list.rds")
#' }
#'
#' # Use the result if it is available
#' if (!is.null(age_list)) {
#'   result <- age_list[[1]]
#'   age_table_gen(result, q = "median", s = "F")
#' } else {
#'   message("Age list is NULL. Please ensure the file exists and is accessible.")
#' }
#'
#' @export


age_table_gen <- function(result, q = "median", s = "F"){
  if(s == "F"){
    if("predicted_df_sex" %in% names(result)){
      age_table <- result$predicted_df_sex %>% filter(.data[["type"]] == q, .data[["sex"]] == "F") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume" = "prediction", "Age" = "age")
    }else{
      age_table <- result %>% filter(.data[["type"]] == q, .data[["sex"]] == "F") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume" = "prediction", "Age" = "age")
    }
    min_age <- floor(min(age_table$Age)/10)*10
    max_age <- floor(max(age_table$Age)/10)*10
    age_table <- lapply(seq(min_age, max_age, 10), function(x){
      sub_age <- age_table %>% filter(.data[["Age"]] >= x) %>% head(1)
      return(sub_age)
    }) %>% bind_rows()
    age_table[["PercentageChange (%)"]] <- c(NA, 100*diff(age_table$Percentile.Volume)/na.omit(lag(age_table$Percentile.Volume)))
    age_table <- age_table %>% mutate(Age = sprintf("%.0f", .data[["Age"]]),
                                      Percentile.Volume = sprintf("%.3f", .data[["Percentile.Volume"]]),
                                      `PercentageChange (%)` = sprintf("%.3f", .data[["PercentageChange (%)"]]))
  }else if(s == "M"){
    if("predicted_df_sex" %in% names(result)){
      age_table <- result$predicted_df_sex %>% filter(.data[["type"]] == q, .data[["sex"]] == "M") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume" = "prediction", "Age" = "age")
    }else{
      age_table <- result %>% filter(.data[["type"]] == q, .data[["sex"]] == "M") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume" = "prediction", "Age" = "age")
    }
    min_age <- floor(min(age_table$Age)/10)*10
    max_age <- floor(max(age_table$Age)/10)*10
    age_table <- lapply(seq(min_age, max_age, 10), function(x){
      sub_age <- age_table %>% filter(.data[["Age"]] >= x) %>% head(1)
      return(sub_age)
    }) %>% bind_rows()
    age_table[["PercentageChange (%)"]] <- c(NA, 100*diff(age_table$Percentile.Volume)/na.omit(lag(age_table$Percentile.Volume)))
    age_table <- age_table %>% mutate(Age = sprintf("%.0f", .data[["Age"]]),
                                      Percentile.Volume = sprintf("%.3f", .data[["Percentile.Volume"]]),
                                      `PercentageChange (%)` = sprintf("%.3f", .data[["PercentageChange (%)"]]))
  }else if(s == "F vs M"){
    if("predicted_df_sex" %in% names(result)){
      age_table_F <- result$predicted_df_sex %>% filter(.data[["type"]] == q, .data[["sex"]] == "F") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume_F" = "prediction", "Age" = "age")
      age_table_M <- result$predicted_df_sex %>% filter(.data[["type"]] == q, .data[["sex"]] == "M") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume_M" = "prediction", "Age" = "age")
    }else{
      age_table_F <- result %>% filter(.data[["type"]] == q, .data[["sex"]] == "F") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume_F" = "prediction", "Age" = "age")
      age_table_M <- result %>% filter(.data[["type"]] == q, .data[["sex"]] == "M") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume_M" = "prediction", "Age" = "age")
    }

    min_age <- floor(min(age_table_F$Age)/10)*10
    max_age <- floor(max(age_table_F$Age)/10)*10
    age_table_F <- lapply(seq(min_age, max_age, 10), function(x){
      sub_age <- age_table_F %>% filter(.data[["Age"]] >= x) %>% head(1)
      return(sub_age)
    }) %>% bind_rows()
    age_table_M <- lapply(seq(min_age, max_age, 10), function(x){
      sub_age <- age_table_M %>% filter(.data[["Age"]] >= x) %>% head(1)
      return(sub_age)
    }) %>% bind_rows()
    age_table <- cbind(age_table_F, age_table_M[c("Percentile.Volume_M")])
    age_table[["PercentageChange_F (%)"]] <- c(NA, 100*diff(age_table$Percentile.Volume_F)/na.omit(lag(age_table$Percentile.Volume_F)))
    age_table[["PercentageChange_M (%)"]] <- c(NA, 100*diff(age_table$Percentile.Volume_M)/na.omit(lag(age_table$Percentile.Volume_M)))
    age_table <- age_table %>% mutate(Age = sprintf("%.0f", .data[["Age"]]),
                                      Percentile.Volume_F = sprintf("%.3f", .data[["Percentile.Volume_F"]]),
                                      Percentile.Volume_M = sprintf("%.3f", .data[["Percentile.Volume_M"]]),
                                      `PercentageChange_F (%)` = sprintf("%.3f", .data[["PercentageChange_F (%)"]]),
                                      `PercentageChange_M (%)` = sprintf("%.3f", .data[["PercentageChange_M (%)"]])
    )
  }

  age_table_dt <- age_table %>% DT::datatable(options = list(dom = 'Bfrtip', buttons = c('csv', 'excel'), columnDefs = list(list(className = 'dt-center', targets = "_all"))), extensions = 'Buttons')
  return(age_table_dt)
}
