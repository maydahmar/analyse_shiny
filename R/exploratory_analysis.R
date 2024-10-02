# R/exploratory_analysis.R

library(ggplot2)
library(corrplot)  # Pour afficher la matrice de corrélation

# Fonction pour visualiser les valeurs manquantes
plot_missing_values <- function(data) {
  missing_values <- colSums(is.na(data))
  barplot(missing_values, main="Valeurs Manquantes par Variable", las=2, col="blue")
}

# Fonction pour afficher la distribution de churn
plot_churn_distribution <- function(data) {
  ggplot(data, aes(x = churn)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Distribution de Churn", x = "Churn", y = "Nombre d'Individus")
}

# Fonction pour visualiser le churn selon les variables catégorielles
plot_categorical_churn <- function(data) {
  cat_vars <- sapply(data, is.factor)
  for (var in names(data)[cat_vars]) {
    ggplot(data, aes_string(x = var, fill = "churn")) +
      geom_bar(position = "fill") +
      labs(title = paste("Churn par", var), y = "Proportion")
  }
}

# Fonction pour visualiser le churn selon les variables numériques
plot_numerical_churn <- function(data) {
  num_vars <- sapply(data, is.numeric)
  for (var in names(data)[num_vars]) {
    ggplot(data, aes_string(x = var, fill = "churn")) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
      labs(title = paste("Distribution de", var, "selon Churn"))
  }
}

# Fonction pour afficher la matrice de corrélation
plot_correlation_matrix <- function(data) {
  cor_matrix <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")
  corrplot(cor_matrix, method = "circle")
}
