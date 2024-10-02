# ui/modeling_ui.R

# UI pour la modélisation
modeling_ui <- function() {
  tagList(
    plotOutput("model_auc_plot"),
    tableOutput("model_summary")
  )
}
