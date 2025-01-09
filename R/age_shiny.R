#' Lifespan Age Trends
#'
#' Provide estimated lifespan age trends of neuroimaging-derived brain structures through shiny app.
#'
#' @param age_list A list containing all ROIs' true volumes, age trend estimates, and the fitted GAMLSS model.
#' @param features A vector of roi names.
#' @param quantile_type A vector of quantile types.
#'
#' @importFrom gamlss gamlss gamlss.control predictAll getQuantile ps pb
#' @importFrom gamlss.dist BCT NO
#' @importFrom utils head
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
#' \dontrun{
#' age_shiny(age_list = age_list, features = "Volume_1", quantile_type = quantile_type)
#' }


age_shiny <- function(age_list, features, quantile_type){
  quantile_type <- quantile_type
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
              radioButtons("quantile", "Select the quantile level", choices = quantile_type, selected = quantile_type[1]))),
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
                         shiny::plotOutput("ageplot"))),
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

    output$ageplot <- shiny::renderPlot({
      result <- age_list[[input$features]]
      if(input$sex == "Female"){
        ggplot(result$true_df, aes(x = .data[["age"]], y = .data[[input$features]])) +
          geom_point(color = "steelblue") +
          geom_line(data = result$predicted_df_sex %>% filter(.data[["sex"]] == "F"), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], linetype = .data[["type"]]), color = "red") +
          labs(x = "Age", y = "ROI Volume") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else if(input$sex == "Male"){
        ggplot(result$true_df, aes(x = .data[["age"]], y = .data[[input$features]])) +
          geom_point(color = "steelblue") +
          geom_line(data = result$predicted_df_sex %>% filter(.data[["sex"]] == "M"), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], linetype = .data[["type"]]), color = "purple") +
          labs(x = "Age", y = "ROI Volume") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }else if(input$sex == "Female vs. Male (Only for visualization)"){
        ggplot(result$true_df, aes(x = .data[["age"]], y = .data[[input$features]])) +
          geom_point(color = "steelblue") +
          geom_line(data = result$predicted_df_sex %>% filter(.data[["type"]] == "median"), mapping = aes(x = .data[["age"]], y = .data[["prediction"]], color = .data[["sex"]])) +
          labs(x = "Age", y = "ROI Volume") +
          theme(
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
          )
      }
    })
    output$agetable <- DT::renderDT({
      result <- age_list[[input$features]]
      if(input$sex == "Female"){
        age_table <- result$predicted_df_sex %>% filter(.data[["type"]] == input$quantile, .data[["sex"]] == "F") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume" = "prediction", "Age" = "age")
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
      }else if(input$sex == "Male"){
        age_table <- result$predicted_df_sex %>% filter(.data[["type"]] == input$quantile, .data[["sex"]] == "M") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume" = "prediction", "Age" = "age")
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
      }else if(input$sex == "Female vs. Male (Only for visualization)"){
        age_table_F <- result$predicted_df_sex %>% filter(.data[["type"]] == input$quantile, .data[["sex"]] == "F") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume_F" = "prediction", "Age" = "age")
        age_table_M <- result$predicted_df_sex %>% filter(.data[["type"]] == input$quantile, .data[["sex"]] == "M") %>% dplyr::select(all_of(c("age", "prediction"))) %>% rename("Percentile.Volume_M" = "prediction", "Age" = "age")
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
      age_table %>% DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                            targets = "_all"))))
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
#' \item{true_df}{a dataframe contains the true age and roi volume infomation}
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
    #mu_form = as.formula("y ~ pb(age)")
    mu_form_sex <- as.formula(paste0(feature, " ~ pb(age) + sex + icv"))
    }else if(mu == "linear"){
    #mu_form = as.formula("y ~ age")
    mu_form_sex <- as.formula(paste0(feature, " ~ age + sex + icv"))
    }else if(mu == "default"){
      #mu_form = as.formula("y ~ 1")
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

  #mdl = gamlss(mu_form,
  #             sigma.formula=sig_form,
  #             nu.formula=nu_form,
  #             tau.formula=tau_form,
  #             family=BCT(),
  #             data = sub_df,
  #             control = gamlss.control(n.cyc = 100))

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
  #y_test = matrix(data=mean(sub_df[["y"]]),nrow=1000,ncol=1)
  #data_test = data.frame(cbind(y_test, age_test))
  #colnames(data_test) = c("y", "age")

  #params = predictAll(object = mdl,data = sub_df, newdata=data_test,
  #                    output='matrix',type="response",
  #                    y.value="median",what=c("mu", "sigma", "nu", "tau"))
  quantiles <- c(lq, 0.5, hq)
  #predictions_quantiles = matrix(data=0,ncol=3,nrow=1000)
  #for (i in 1:length(quantiles)){
  #  Qua <- getQuantile(obj = mdl, quantile = quantiles[i], term="age", fixed.at=list())
  #  predictions_quantiles[,i] = Qua(age_test)
  #}
  #colnames(predictions_quantiles) = c(paste0("quantile_", 100*lq), "median", paste0("quantile_", 100*hq))
  #age_df = data.frame(cbind(age = age_test, predictions_quantiles)) %>%
  #  pivot_longer(colnames(predictions_quantiles), names_to = "type", values_to = "prediction")
  #
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





