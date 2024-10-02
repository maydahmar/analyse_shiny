#app

# app.R

# Charger les librairies
library(shiny)
library(shinydashboard)  # Pour un look moderne, si nécessaire
library(dplyr)
library(ggplot2)
library(caret)          # Pour le machine learning

# Charger les fonctions
source("R/functions.R")
source("R/exploratory_analysis.R")
source("R/modeling.R")
source("R/resampling.R")
source("R/hyperparameter_tuning.R")

# Charger les interfaces utilisateur
source("ui/exploratory_ui.R")
source("ui/modeling_ui.R")
source("ui/resampling_ui.R")
source("ui/conclusions_ui.R")

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse et Prédiction de Churn"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Sélectionner un jeu de données :", 
                  choices = c("Credit Fraud", "Bank Marketing", "Employee Attrition")),
      actionButton("run_analysis", "Lancer l'Analyse")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Exploration des Données", 
                 uiOutput("exploratory_ui")),
        
        tabPanel("Modélisation",
                 uiOutput("modeling_ui")),
        
        tabPanel("Sur-échantillonnage/Sous-échantillonnage",
                 uiOutput("resampling_ui")),
        
        tabPanel("Conclusions",
                 uiOutput("conclusions_ui"))
      )
    )
  )
)

# Définir la logique serveur
server <- function(input, output, session) {
  
  data <- reactive({
    dataset <- switch(input$dataset,
                      "Credit Fraud" = load_credit_fraud(),
                      "Bank Marketing" = load_bank_marketing(),
                      "Employee Attrition" = load_employee_attrition())
    dataset
  })
  
  observeEvent(input$run_analysis, {
    # Pour l'UI d'analyse exploratoire
    output$exploratory_ui <- renderUI({
      tagList(
        plotOutput("missing_values_plot"),
        plotOutput("churn_distribution_plot"),
        plotOutput("categorical_churn_plot"),
        plotOutput("numerical_churn_plot"),
        plotOutput("correlation_matrix_plot")
      )
    })
    
    # Pour l'UI de modélisation
    output$modeling_ui <- renderUI({
      tagList(
        plotOutput("model_auc_plot"),
        tableOutput("model_summary")
      )
    })
    
    # Pour l'UI de resampling
    output$resampling_ui <- renderUI({
      tagList(
        plotOutput("sampling_auc_plot"),
        tableOutput("sampling_summary")
      )
    })
    
    # Pour l'UI des conclusions
    output$conclusions_ui <- renderUI({
      tagList(
        verbatimTextOutput("conclusions_text")
      )
    })
    
    output$missing_values_plot <- renderPlot({
      plot_missing_values(data())
    })
    
    output$churn_distribution_plot <- renderPlot({
      plot_churn_distribution(data())
    })
    
    output$categorical_churn_plot <- renderPlot({
      plot_categorical_churn(data())
    })
    
    output$numerical_churn_plot <- renderPlot({
      plot_numerical_churn(data())
    })
    
    output$correlation_matrix_plot <- renderPlot({
      plot_correlation_matrix(data())
    })
  })
  
  output$model_auc_plot <- renderPlot({
    model_results <- train_models(data())
    plot_auc(model_results)
  })
  
  output$model_summary <- renderTable({
    model_results <- train_models(data())
    summary_table(model_results)
  })
  
  output$sampling_auc_plot <- renderPlot({
    sampling_results <- apply_sampling_methods(data())
    plot_sampling_auc(sampling_results)
  })
  
  output$sampling_summary <- renderTable({
    sampling_results <- apply_sampling_methods(data())
    summary_sampling_table(sampling_results)
  })
  
  output$conclusions_text <- renderText({
    # Insérer ici le texte des conclusions basées sur les résultats
    "Conclusions: Ici, vous pouvez résumer les résultats et les recommandations."
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
