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
                    choices = c("Decision Tree", "Logistic Regression", "SVM"), 
                    selected = "Decision Tree"),
        
        conditionalPanel(
          condition = "input.model_choice == 'Decision Tree'",
          sliderInput("tree_depth", "Max Depth of Tree", min = 1, max = 20, value = 5),
          sliderInput("min_samples_split", "Min Samples Split", min = 2, max = 10, value = 2)
        ),
        
        conditionalPanel(
          condition = "input.model_choice == 'SVM'",
          selectInput("svm_kernel", "Kernel Type", 
                      choices = c("Linear", "Radial", "Polynomial", "Sigmoid"), 
                      selected = "Radial"),
          numericInput("svm_cost", "Cost", value = 1, min = 0.01, max = 10, step = 0.1),
          numericInput("svm_gamma", "Gamma (for Radial)", value = 0.1, min = 0.01, max = 1, step = 0.01)
        ),
        
        actionButton("train_model", "Train Model", icon = icon("play"))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Model Performance",
                   fluidRow(
                     box(title = "Model Accuracy", width = 12, status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("model_accuracy")),
                     box(title = "Confusion Matrix", width = 12, status = "info", solidHeader = TRUE,
                         tableOutput("confusion_matrix"))
                   ),
                   fluidRow(
                     box(title = "ROC Curve", width = 12, status = "warning", solidHeader = TRUE,
                         plotOutput("roc_curve")),
                     box(title = "Feature Importance", width = 12, status = "success", solidHeader = TRUE,
                         plotOutput("feature_importance"))
                   )
          ),
          tabPanel("Hyperparameter Tuning",
                   fluidRow(
                     box(title = "Optimal Hyperparameters", width = 12, status = "info", solidHeader = TRUE,
                         verbatimTextOutput("best_hyperparameters"))
                   )
          )
        )
      )
    )
  )
}
