library(ggplot2)
library(dplyr)
library(ggcorrplot)

# Charger et préparer les données en fonction de la sélection
load_dataset <- function(dataset_name) {
  if (dataset_name == "Credit fraud") {
    return(read.csv("data/credit_fraud.csv"))
  } else if (dataset_name == "Bank marketing") {
    return(read.csv("data/bank_marketing.csv"))
  } else if (dataset_name == "Employee attrition") {
    return(read.csv("data/employee_attrition.csv"))
  } else if (dataset_name == "Bank marketing full") {
    return(read.csv("/mnt/data/bank-additional-full.csv")) # Fichier chargé
  }
}

# Fonction pour créer un résumé du dataset
summarize_data <- function(data) {
  summary(data)
}

# Fonction pour créer un graphique de churn
plot_churn <- function(data, variable) {
  ggplot(data, aes_string(x = variable, fill = "Churn")) +
    geom_bar(position = "fill") +
    labs(title = paste("Distribution de", variable, "par churn"), y = "Proportion")
}

# Fonction pour créer une matrice de corrélation
plot_correlation <- function(data) {
  correlation_matrix <- cor(select_if(data, is.numeric))
  ggcorrplot(correlation_matrix, method = "circle")
}

# Module serveur pour l'analyse exploratoire
exploratory_server <- function(input, output, session) {
  
  # Réagir au chargement des données
  dataset <- eventReactive(input$load_data, {
    data <- load_dataset(input$dataset)
    updateSelectInput(session, "variable", choices = names(data))
    return(data)
  })
  
  # Résumé des données
  output$data_summary <- renderTable({
    summarize_data(dataset())
  })
  
  # Graphique de churn
  output$churn_plot <- renderPlot({
    req(input$variable)
    plot_churn(dataset(), input$variable)
  })
  
  # Matrice de corrélation
  output$correlation_plot <- renderPlot({
    plot_correlation(dataset())
  })
}
