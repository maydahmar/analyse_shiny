library(shiny)
library(shinydashboard)

# Charger les fichiers R pour l'interface et les fonctions
source("R/exploratory_analysis.R")
source("ui/exploratory_ui.R")
source("ui/modeling_ui.R")
source("ui/resampling_ui.R")
source("ui/conclusions_ui.R")

# Définir l'interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Profil de données"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      
      # Liste déroulante pour choisir le dataset
      selectInput(
        inputId = "dataset_choice",
        label = "Select dataset",
        choices = list(
          "Credit Fraud" = "bank-additional-full.csv",
          "Bank Marketing" = "creditcard.csv",
          "Employee Attrition" = "data_dictionary.csv"
        ),
        selected = "bank-additional-full.csv"
      )
    )
  ),
  
  dashboardBody(
    # Inclure le fichier HTML
    includeHTML("www/index.html")
  )
)


# Définir le serveur
server <- function(input, output, session) {
  
  # Charger dynamiquement le dataset sélectionné
  selected_dataset <- reactive({
    file_path <- file.path("data", input$dataset_choice) # Chemin vers le fichier
    read.csv(file_path, stringsAsFactors = FALSE) # Charger le fichier CSV
  })
  
  # Exemple : observer la sélection et afficher un message (peut être modifié selon les besoins)
  observe({
    dataset <- selected_dataset()
    cat("Dataset chargé :", input$dataset_choice, "\n")
  })
  
  exploratory_server(input, output, session)
  # modeling_server(input, output, session)
  # resampling_server(input, output, session)
  # conclusions_server(input, output, session)
}


# Lancer l'application
shinyApp(ui = ui, server = server)
