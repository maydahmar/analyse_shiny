# ui/resampling_ui.R

# UI pour le sur-échantillonnage/sous-échantillonnage
resampling_ui <- function() {
  tagList(
    plotOutput("sampling_auc_plot"),
    tableOutput("sampling_summary")
  )
}
