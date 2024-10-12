exploratory_ui <- function() {
  fluidPage(
    useShinyjs(), # Charger shinyjs pour pouvoir utiliser les fonctions de navigation
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")  # Lien vers le fichier CSS
    ),
    
    tags$div(id = "loading-message", style = "display:none;", "Loading..."),
    
    actionButton("go_back", "Go Back Home"),  # Bouton pour revenir à la page d'accueil
    titlePanel("Exploratory Data Analysis"),
    
    # Définir les onglets
    tabsetPanel(
      # Onglet Data Info
      tabPanel("Data Info",
               fluidRow(
                 box(title = "Dataset Table", width = 12, status = "primary", solidHeader = TRUE,
                     DT::dataTableOutput("dataset_table"))
               ),
               fluidRow(
                 box(title = "Dataset Information", width = 6, status = "info", solidHeader = TRUE,
                     tableOutput("dataset_info"))
               ),
               fluidRow(
                 box(title = "Column Types", width = 6, status = "info", solidHeader = TRUE,
                     tableOutput("column_types"))
               ),
               fluidRow(
                 box(title = "Churn Proportion", width = 6, status = "info", solidHeader = TRUE,
                     plotOutput("churn_plot"))
               )
      ),
      
      # Onglet Numeric Features
      tabPanel("Numeric Features",
               fluidRow(
                 box(title = "Select a Numeric Variable", width = 12, status = "warning", solidHeader = TRUE,
                     selectInput("numericFeature", "Choose a Numeric Variable", choices = NULL),
                     tableOutput("featureStats"),
                     plotOutput("featureHist")
                 )
               ),
               fluidRow(
                 box(title = "Churn & Non-Churn Distribution for Numeric Variables", width = 12, status = "warning", solidHeader = TRUE,
                     selectInput("selected_numeric_var", "Choose a Numeric Variable", choices = NULL),
                     plotOutput("numeric_churn_histogram")
                 )
               ),
               fluidRow(
                 box(title = "Correlation Matrix of Numeric Variables", width = 12, status = "info", solidHeader = TRUE,
                     plotOutput("correlation_matrix")
                 )
               )
      ),
      
      # Onglet Categorical Features
      tabPanel("Categorical Features",
               fluidRow(
                 box(title = "Select a Categorical Variable", width = 12, status = "warning", solidHeader = TRUE,
                     selectInput("categoricalFeature", "Choose a Categorical Variable", choices = NULL),
                     plotOutput("catFeaturePlot")
                 )
               ),
               fluidRow(
                 box(title = "Churn vs Non-Churn Proportion for Categorical Variables", width = 12, status = "warning", solidHeader = TRUE,
                     plotOutput("cat_churn_plot")
                 )
               ),
               fluidRow(
                 box(title = "Calculate Cramér's V Coefficient", width = 12, status = "warning", solidHeader = TRUE,
                     selectInput("catVar1", "Choose the First Categorical Variable", choices = NULL),
                     selectInput("catVar2", "Choose the Second Categorical Variable", choices = NULL),
                     verbatimTextOutput("cramersV")
                 )
               )
      ),
      
      tabPanel("Rapport (Conclusion)",
               fluidRow(
                 box(title = "Analyse approfondie du dataset 'Bank Marketing'", width = 12, status = "success", solidHeader = TRUE,
                     uiOutput("conclusion_text")  # Utiliser uiOutput au lieu de verbatimTextOutput
                 )
               )
      )
      
    )
  )
}
