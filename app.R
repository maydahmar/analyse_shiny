# Charger les fichiers R pour l'interface et les fonctions
source("R/exploratory_analysis.R")
source("ui/exploratory_ui.R")
source("R/modeling.R")
source("ui/modeling_ui.R")
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)  # Charger readxl pour lire les fichiers Excel

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Profil de données"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  # Ajout d'un ID pour le système d'onglets
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
        selected = "bank-additional-full.csv"
      ),
      
      # Ajouter le menu item pour Exploratory Analysis
      menuItem("Exploratory Analysis", icon = icon("chart-bar"), tabName = "exploratory"),
      
      # Nouveau lien vers Model Analysis
      menuItem("Model Analysis", icon = icon("cogs"), tabName = "modeling")
    )
  ),
  
  # Corps de la page
  dashboardBody(
    useShinyjs(),  # Activer shinyjs
    tabItems(
      tabItem(tabName = "home", 
              div(id = "home-content", 
                  includeHTML("www/index.html"),  # Le contenu original, y compris les images, actualités, etc.
                  
                  # Ajouter des boutons de navigation après le contenu existant
                  h2("Explorez les sections suivantes"),
                  p("Vous pouvez cliquer sur les boutons ci-dessous pour accéder directement aux sections :"),
                  actionButton("go_to_exploratory", "Aller à l'Exploratory Analysis"),
                  actionButton("go_to_modeling", "Aller à l'Analyse des Modèles")
              )
      ),
      tabItem(tabName = "exploratory", 
              div(id = "exploratory-content", exploratory_ui())),
      tabItem(tabName = "modeling", 
              div(id = "modeling-content", modeling_ui()))
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Charger dynamiquement le dataset sélectionné chaque fois que 'dataset_choice' change
  selected_dataset <- reactive({
    file_path <- file.path("data", input$dataset_choice)
    
    # Vérifier le type de fichier et le charger correctement
    if (grepl("\\.csv$", file_path)) {
      header_line <- readLines(file_path, n = 1)
      sep_guess <- if (grepl(";", header_line)) ";" else if (grepl(",", header_line)) "," else if (grepl("\\t", header_line)) "\t" else stop("Impossible de détecter le séparateur du fichier CSV")
      read.csv(file_path, stringsAsFactors = FALSE, sep = sep_guess)
    } else if (grepl("\\.xlsx$", file_path)) {
      read_excel(file_path)
    } else {
      stop("Format de fichier non pris en charge")
    }
  })

  
  # Appel du serveur spécifique de l'analyse exploratoire avec les données chargées
  exploratory_analysis(input, output, session, selected_dataset = selected_dataset)
  
  # Utiliser `updateTabItems()` pour basculer d'onglet lorsque les boutons sont cliqués
  observeEvent(input$go_to_exploratory, {
    updateTabItems(session, "tabs", "exploratory")
  })
  
  observeEvent(input$go_to_modeling, {
    updateTabItems(session, "tabs", "modeling")
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
