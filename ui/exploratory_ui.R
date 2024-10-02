# ui/exploratory_ui.R

# UI pour l'analyse exploratoire
exploratory_ui <- function() {
  tagList(
    plotOutput("missing_values_plot"),
    plotOutput("churn_distribution_plot"),
    plotOutput("categorical_churn_plot"),
    plotOutput("numerical_churn_plot"),
    plotOutput("correlation_matrix_plot")
  )
}
