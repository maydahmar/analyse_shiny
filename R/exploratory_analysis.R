# R/exploratory_analysis.R

# R/exploratory_analysis.R
exploratory_analysis <- function(input, output, session, selected_dataset) {
  output$exploratory_test <- renderText({
    "Exploratory UI Loaded!" # Message de test
  })
  
  # Résumé des dimensions et valeurs manquantes
  output$dataset_summary <- renderTable({
    dataset <- selected_dataset()
    summary <- data.frame(
      "Dimension" = c("Rows", "Columns"),
      "Count" = c(nrow(dataset), ncol(dataset))
    )
    summary$`Missing Values` <- c(sum(is.na(dataset)), NA)
    return(summary)
  })
  
  # Proportion de churn
  output$churn_plot <- renderPlot({
    dataset <- selected_dataset()
    prop_table <- prop.table(table(dataset$churn)) # Remplace "churn" par le nom de la colonne cible
    barplot(prop_table, main = "Proportion de Churn", col = c("blue", "red"))
  })
  
  # Variables catégorielles vs. Churn
  output$cat_var_plot <- renderPlot({
    dataset <- selected_dataset()
    cat_vars <- sapply(dataset, is.factor) # Identifier les variables catégorielles
    for (var in names(cat_vars[cat_vars])) {
      table_data <- table(dataset[[var]], dataset$churn) # Remplace "churn" par le nom de la colonne cible
      barplot(prop.table(table_data, margin = 2), main = paste("Proportion de", var, "vs. Churn"),
              col = c("blue", "red"), legend = TRUE)
    }
  })
  
  # Variables numériques - Churn vs. Non-Churn
  output$num_var_plot <- renderPlot({
    dataset <- selected_dataset()
    num_vars <- sapply(dataset, is.numeric) # Identifier les variables numériques
    for (var in names(num_vars[num_vars])) {
      hist(dataset[dataset$churn == 1, var], main = paste(var, "- Churn"),
           xlab = var, col = "red", breaks = 10, freq = FALSE)
      hist(dataset[dataset$churn == 0, var], main = paste(var, "- Non-Churn"),
           xlab = var, col = "blue", breaks = 10, freq = FALSE, add = TRUE)
    }
  })
  
  # Matrice de corrélation
  output$corr_matrix <- renderPlot({
    dataset <- selected_dataset()
    num_vars <- sapply(dataset, is.numeric)
    corr <- cor(dataset[, num_vars], use = "complete.obs")
    corrplot::corrplot(corr, method = "circle")
  })
}
