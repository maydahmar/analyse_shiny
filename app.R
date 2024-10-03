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
      menuItem("Exploratory Analysis", tabName = "exploratory", icon = icon("chart-bar")),
      menuItem("Modeling", tabName = "modeling", icon = icon("cogs")),
      menuItem("Resampling", tabName = "resampling", icon = icon("sync")),
      menuItem("Conclusions", tabName = "conclusions", icon = icon("check-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # Lien vers le fichier CSS
    ),
    tabItems(
      # Page d'accueil
      tabItem(tabName = "home",
              h1("Welcome to the Data Profile"),
              fluidRow(
                box(
                  title = "About this tool", width = 12, status = "primary",
                  p("This tool allows you to explore different datasets related to Credit Fraud, Bank Marketing, and Employee Attrition.")
                )
              ),
              fluidRow(
                box(
                  title = "Explore Datasets", width = 3, status = "info",
                  actionButton("explore_btn", "Go to Exploratory Analysis", icon = icon("chart-bar"))
                ),
                box(
                  title = "Modeling", width = 3, status = "info",
                  actionButton("model_btn", "Go to Modeling", icon = icon("cogs"))
                ),
                box(
                  title = "Resampling", width = 3, status = "info",
                  actionButton("resample_btn", "Go to Resampling", icon = icon("sync"))
                ),
                box(
                  title = "Conclusions", width = 3, status = "info",
                  actionButton("conclude_btn", "Go to Conclusions", icon = icon("check-circle"))
                )
              )
      ),
      exploratory_ui(),
      modeling_ui(),
      resampling_ui(),
      conclusions_ui()
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  exploratory_server(input, output, session)
  #modeling_server(input, output, session)
  #resampling_server(input, output, session)
  #conclusions_server(input, output, session)
  
  # Navigation entre les pages
  observeEvent(input$explore_btn, {
    updateTabItems(session, "exploratory")
  })
  
  observeEvent(input$model_btn, {
    updateTabItems(session, "modeling")
  })
  
  observeEvent(input$resample_btn, {
    updateTabItems(session, "resampling")
  })
  
  observeEvent(input$conclude_btn, {
    updateTabItems(session, "conclusions")
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
