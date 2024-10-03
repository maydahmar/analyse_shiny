exploratory_ui <- function() {
  fluidPage(
    useShinyjs(),  # Charger shinyjs pour pouvoir utiliser les fonctions de navigation
    actionButton("go_back", "go back home"),  # Bouton pour revenir à la page d'accueil
    titlePanel("Exploratory Data Analysis"),
    fluidRow(
      box(title = "Dimensions et valeurs manquantes", width = 6, status = "primary", solidHeader = TRUE,
          tableOutput("dataset_summary")),
      box(title = "Proportion de churn", width = 6, status = "primary", solidHeader = TRUE,
          plotOutput("churn_plot"))
    ),
    fluidRow(
      box(title = "Variables catégorielles", width = 12, status = "info", solidHeader = TRUE,
          plotOutput("cat_var_plot"))
    ),
    fluidRow(
      box(title = "Variables numériques - Churn vs. Non-Churn", width = 12, status = "info", solidHeader = TRUE,
          plotOutput("num_var_plot"))
    ),
    fluidRow(
      box(title = "Matrice de corrélation", width = 12, status = "danger", solidHeader = TRUE,
          plotOutput("corr_matrix"))
    )
  )
}
