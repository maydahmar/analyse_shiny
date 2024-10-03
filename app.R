# Charger les fichiers R pour l'interface et les fonctions
source("R/exploratory_analysis.R")
source("ui/exploratory_ui.R")
library(shiny)
library(shinydashboard)
library(shinyjs)


# Définir l'interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Profil de données"),
  dashboardSidebar(
    id = "tabs",
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      
      
      # Choix du dataset
      selectInput(
        inputId = "dataset_choice",
        label = "Choisir un dataset",
        choices = list(
          "Credit Fraud" = "credit_fraud.csv",
          "Bank Marketing" = "bank_marketing.csv",
          "Employee Attrition" = "employee_attrition.csv"
        ),
        selected = "credit_fraud.csv"
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),  # Activer shinyjs
    div(id = "home-content",
        includeHTML("www/index.html")
    ),
    div(id = "exploratory-content",
        exploratory_ui(),  # Appel à l'UI spécifique de l'analyse exploratoire
        style = "display: none;"  # Cacher cette section au départ
    )
  )
  
)

# Définir le serveur
server <- function(input, output, session) {
  
  # Charger dynamiquement le dataset sélectionné
  selected_dataset <- reactive({
    file_path <- file.path("data", input$dataset_choice)
    read.csv(file_path, stringsAsFactors = FALSE)
  })
  
  # Appel du serveur spécifique de l'analyse exploratoire
  callModule(exploratory_analysis, "exploratory", selected_dataset = selected_dataset)
  
  # Observer les changements de 'navigate_to' pour afficher l'interface Exploratory Analysis
  observeEvent(input$navigate_to, {
    if (input$navigate_to == 'exploratory') {
      shinyjs::hide("home-content")  # Cacher la section accueil
      shinyjs::show("exploratory-content")  # Afficher la section Exploratory Analysis
    }
  })
  
  # Observer le clic sur le bouton de retour pour revenir à la page d'accueil
  observeEvent(input$go_back, {
    shinyjs::hide("exploratory_ui")
    shinyjs::show("home-content")
  })
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)
