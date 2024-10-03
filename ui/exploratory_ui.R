exploratory_ui <- function() {
  tabItem(
    tabName = "exploratory",
    fluidPage(
      h2("Exploratory Analysis"),
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset", "Choisir un dataset:",
                      choices = c("Credit fraud", "Bank marketing", "Employee attrition", "Bank marketing full")),
          actionButton("load_data", "Charger les données"),
          hr(),
          selectInput("variable", "Choisir une variable:", choices = NULL),
          selectInput("plot_type", "Type de graphique:", 
                      choices = c("Histogramme", "Barplot", "Boxplot")),
          hr()
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Résumé des données", tableOutput("data_summary")),
            tabPanel("Graphique de churn", plotOutput("churn_plot")),
            tabPanel("Corrélations", plotOutput("correlation_plot"))
          )
        )
      )
    )
  )
}
