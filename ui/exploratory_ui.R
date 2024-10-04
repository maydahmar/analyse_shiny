exploratory_ui <- function() {
  fluidPage(
    useShinyjs(),  # Charger shinyjs pour pouvoir utiliser les fonctions de navigation
    actionButton("go_back", "Go Back Home"),  # Bouton pour revenir à la page d'accueil
    titlePanel("Exploratory Data Analysis"),
    
    # Afficher le tableau du dataset
    fluidRow(
      box(title = "Tableau du Dataset", width = 12, status = "primary", solidHeader = TRUE,
          DT::dataTableOutput("dataset_table"))
    ),
    
    # Afficher les dimensions, les valeurs manquantes, et les attributs constants
    fluidRow(
      box(title = "Informations sur le Dataset", width = 6, status = "info", solidHeader = TRUE,
          tableOutput("dataset_info"))
    ),
    
    # Afficher la proportion de churn
    fluidRow(
      box(title = "Proportion de Churn", width = 6, status = "info", solidHeader = TRUE,
          plotOutput("churn_plot"))
    ),
    
    
    
  
    
  )
  
  
}
