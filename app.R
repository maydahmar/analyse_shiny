# Charger les fichiers R pour l'interface et les fonctions
source("R/exploratory_analysis.R")
source("ui/exploratory_ui.R")
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)  # Charger readxl pour lire les fichiers Excel

# Définir l'interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Profil de données"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      
      # Choix du dataset
      selectInput(
        inputId = "dataset_choice",
        label = "Choisir un dataset",
        choices = list(
          "Credit Fraud" = "creditcard.csv",
          "Bank Marketing" = "bank-additional-full.csv",
          "Employee Attrition" = "whole data.csv"
        ),
        selected = "creditcard.csv"
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
    
    
    # Vérifier le type de fichier et le charger correctement
    if (grepl("\\.csv$", file_path)) {
      # Lire les premières lignes pour détecter le séparateur
      header_line <- readLines(file_path, n = 1)
      
      # Vérifier quel séparateur semble être utilisé
      sep_guess <- if (grepl(";", header_line)) {
        ";"
      } else if (grepl(",", header_line)) {
        ","
      } else if (grepl("\\t", header_line)) {
        "\t"
      } else {
        stop("Impossible de détecter le séparateur du fichier CSV")
      }
      
      # Charger le fichier avec le bon séparateur
      read.csv(file_path, stringsAsFactors = FALSE, sep = sep_guess)
    } else if (grepl("\\.xlsx$", file_path)) {
      read_excel(file_path)
    } else {
      stop("Format de fichier non pris en charge")
    }
  })
  
  # Appel du serveur spécifique de l'analyse exploratoire
  exploratory_analysis(input, output, session, selected_dataset = selected_dataset)
  
  # Observer les changements de 'navigate_to' pour afficher l'interface Exploratory Analysis
  observeEvent(input$navigate_to, {
    if (input$navigate_to == 'exploratory') {
      shinyjs::hide("home-content")  # Cacher la section accueil
      shinyjs::show("exploratory-content")  # Afficher la section Exploratory Analysis
    }
  })
  
  # Observer le clic sur le bouton de retour pour revenir à la page d'accueil
  observeEvent(input$go_back, {
    shinyjs::hide("exploratory-content")  # Cacher la section Exploratory Analysis
    shinyjs::show("home-content")  # Afficher la section accueil
  })
  
  # Réinitialiser l'input navigate_to à chaque fois que la page d'accueil est affichée
  observeEvent(input$go_back, {
    updateTextInput(session, "navigate_to", value = "")
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)