modeling_ui <- function() {
  fluidPage(
    useShinyjs(),  # Pour la gestion d'éléments interactifs et cachés
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")  # Ajouter du CSS si nécessaire
    ),
    
    titlePanel("Model Analysis and Predictions"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Model Selection"),
        
        # Choix du modèle à entraîner
        selectInput("model_choice", "Choose a Model",
                    choices = c("Decision Tree", "Logistic Regression", "SVM Linear", "SVM Non Linear"),
                    selected = "Decision Tree"),
        
        actionButton("train_model", "Train Model", icon = icon("play"))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Model Performance",
                   fluidRow(
                     box(title = "Model Accuracy", width = 12, status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("model_accuracy")),
                     box(title = "Model AUC", width = 12, status = "info", solidHeader = TRUE,
                         verbatimTextOutput("model_auc")),
                     box(title = "Confusion Matrix", width = 12, status = "info", solidHeader = TRUE,
                         plotOutput("confusion_matrix"))
                   ),
                   fluidRow(
                     box(title = "ROC Curve", width = 12, status = "warning", solidHeader = TRUE,
                         plotOutput("roc_curve")),
                     box(title = "Feature Importance", width = 12, status = "success", solidHeader = TRUE,
                         plotOutput("feature_importance"))
                   )
          ),
          # Nouvel onglet pour les résultats du grid search
          tabPanel("Grid Search Results",
                   fluidRow(
                     box(title = "Grid Search", width = 12, status = "primary", solidHeader = TRUE,
                         plotOutput("grid_results"))
                   )
          )
        )
      )
    )
  )
}
