# ui/conclusions_ui.R

# UI pour les conclusions
conclusions_ui <- function() {
  tagList(
    verbatimTextOutput("conclusions_text")
  )
}
