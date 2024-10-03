# R/functions.R

# Charger les librairies nécessaires
library(dplyr)

# Fonction pour charger le jeu de données Credit Fraud
load_credit_fraud <- function() {
  read.csv("../data/credit_fraud.csv")
}

# Fonction pour charger le jeu de données Bank Marketing
load_bank_marketing <- function() {
  prin('ici')
  read.csv("../data/bank-additional-full.csv")
}

# Fonction pour charger le jeu de données Employee Attrition
load_employee_attrition <- function() {
  read.csv("data/employee_attrition.csv")
}

# Fonction pour convertir les colonnes qualitatives nominales en numériques
convert_nominal_to_numeric <- function(data, cols_to_convert) {
  for (col in cols_to_convert) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      # Conversion en facteur (si pas déjà)
      data[[col]] <- as.factor(data[[col]])
      # Remplacer les niveaux du facteur par des valeurs numériques
      data[[col]] <- as.numeric(data[[col]]) - 1  # Commencer à 0
    }
  }
  return(data)
}

# Fonction pour convertir les colonnes quantitatives continues en classes discrètes
convert_continuous_to_bins <- function(data, col_to_convert, num_bins = 5, start = NULL, end = NULL) {
  col_values <- data[[col_to_convert]]
  
  # Si des valeurs spécifiques de start et end sont données, on les utilise, sinon on prend les min et max de la colonne
  if (is.null(start)) {
    start <- min(col_values, na.rm = TRUE)
  }
  if (is.null(end)) {
    end <- max(col_values, na.rm = TRUE)
  }
  
  # Création des bornes d'intervalle en fonction du nombre de classes
  breaks <- seq(start, end, length.out = num_bins + 1)
  
  # Classification des valeurs dans les classes correspondantes
  data[[col_to_convert]] <- cut(col_values, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  return(data)
}

# Autres fonctions utilitaires peuvent être ajoutées ici
