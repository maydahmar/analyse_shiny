# Charger les fichiers R pour l'interface et les fonctions
source("R/exploratory_analysis.R")
source("ui/exploratory_ui.R")
source("R/modeling.R")
source("ui/modeling_ui.R")
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
        selected = "bank-additional-full.csv"
      ), 
      
      # Ajouter le menu item pour Exploratory Analysis
      menuItem("Exploratory Analysis", icon = icon("chart-bar"), tabName = "exploratory"),
      
      # Nouveau lien vers Model Analysis
      menuItem("Model Analysis", icon = icon("cogs"), tabName = "modeling")
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
    ),
    # Nouvelle section pour l'analyse des modèles
    div(id = "modeling-content",
        modeling_ui(),  # Appel à l'UI spécifique de l'analyse de modèles
        style = "display: none;"  # Cacher cette section au départ
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  
  # Observer la sélection du dataset pour afficher l'onglet correspondant
  observe({
    if (input$dataset_choice == "bank-additional-full.csv") {
      shinyjs::show(selector = 'a[data-value="Rapport Bank Marketing"]')
      shinyjs::hide(selector = 'a[data-value="Rapport Employee Attrition"]')
    } else if (input$dataset_choice == "whole data.csv") {
      shinyjs::show(selector = 'a[data-value="Rapport Employee Attrition"]')
      shinyjs::hide(selector = 'a[data-value="Rapport Bank Marketing"]')
    } else {
      shinyjs::hide(selector = 'a[data-value="Rapport Bank Marketing"]')
      shinyjs::hide(selector = 'a[data-value="Rapport Employee Attrition"]')
    }
  })
  
  
  
  
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
  
  
  # Observer les changements de 'navigate_to' pour afficher l'interface Exploratory Analysis
  observeEvent(input$navigate_to, {
    if (input$navigate_to == 'exploratory') {
      shinyjs::hide("home-content")  # Cacher la section accueil
      shinyjs::show("exploratory-content")  # Afficher la section Exploratory Analysis
      shinyjs::hide("modeling-content") 
      # Réinitialiser la valeur de 'navigate_to' après avoir affiché l'Exploratory Analysis
      session$sendCustomMessage(type = 'resetNavigate', message = list())
    }else if (input$navigate_to == 'modeling') {
      shinyjs::hide("home-content")  # Cacher la section accueil
      shinyjs::hide("exploratory-content")  # Afficher la section Exploratory Analysis
      shinyjs::show("modeling-content") 
      # Réinitialiser la valeur de 'navigate_to' après avoir affiché l'Exploratory Analysis
      session$sendCustomMessage(type = 'resetNavigate', message = list())
    }
  })
  
  # Observer le clic sur l'item du menu latéral
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "exploratory") {
      shinyjs::hide("home-content")  # Cacher la section accueil
      shinyjs::show("exploratory-content")  # Afficher la section Exploratory Analysis
      shinyjs::hide("modeling-content") 
    } else if (input$sidebarItemExpanded == "modeling") {
      shinyjs::hide("home-content")  # Cacher la section d'accueil
      shinyjs::hide("exploratory-content")  # Cacher la section Exploratory Analysis
      shinyjs::show("modeling-content")  # Afficher la section Modeling
    } else if (input$sidebarItemExpanded == "home") {
      shinyjs::show("home-content")  # Afficher la section d'accueil
      shinyjs::hide("exploratory-content")  # Cacher la section Exploratory Analysis
      shinyjs::hide("modeling-content")  # Cacher la section Modeling
    }
  })
  
  
  # Observer le clic sur le bouton de retour pour revenir à la page d'accueil
  observeEvent(input$go_back, {
    shinyjs::hide("exploratory-content")  # Cacher la section Exploratory Analysis
    shinyjs::show("home-content")  # Afficher la section accueil
    shinyjs::hide("modeling-content") 
    # Réinitialiser la valeur de 'navigate_to'
    session$sendCustomMessage(type = 'resetNavigate', message = list())
  })
  
  # Observer le clic sur le raccourci Exploratory Analysis dans la barre latérale
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "exploratory") {
      shinyjs::hide("home-content")  # Cacher la section accueil
      shinyjs::show("exploratory-content")  # Afficher la section Exploratory Analysis
      shinyjs::hide("modeling-content") 
    } else if (input$sidebarItemExpanded == "modeling") {
      shinyjs::hide("home-content")  # Cacher la section d'accueil
      shinyjs::hide("exploratory-content")  # Cacher la section Exploratory Analysis
      shinyjs::show("modeling-content")  # Afficher la section Modeling
    }
  })

  
  
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)