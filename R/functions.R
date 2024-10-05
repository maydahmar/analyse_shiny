# R/functions.R

# Charger les librairies nécessaires
library(dplyr)

# Fonction pour charger le jeu de données Credit Fraud
load_credit_fraud <- function() {
  read.csv("../data/credit_fraud.csv")
}

# Fonction pour charger le jeu de données Bank Marketing
load_bank_marketing <- function() {
  read.csv("data/bank-additional-full.csv", sep=';')
}

# Fonction pour charger le jeu de données Employee Attrition
load_employee_attrition <- function() {
  read.csv("data/employee_attrition.csv", sep=';')
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

convert_continuous_to_bins <- function(data, col_to_convert, num_bins = 5, start = NULL, end = NULL) {
  col_values <- data[[col_to_convert]]
  
  # Si des valeurs spécifiques de start et end sont données, on les utilise, sinon on prend les min et max de la colonne
  if (is.null(start)) {
    start <- min(col_values, na.rm = TRUE)
  }
  if (is.null(end)) {
    end <- max(col_values, na.rm = TRUE)
  }
  
  # Vérification du nombre de catégories demandé
  if (num_bins < 3) {
    stop("Le nombre de catégories doit être au moins 3 pour gérer les valeurs en dehors des bornes.")
  }
  
  # Création des bornes pour les classes
  breaks <- seq(start, end, length.out = num_bins - 1)  # -1 car nous ajouterons manuellement les classes pour < start et > end
  
  # Ajout des bornes extrêmes
  breaks <- c(-Inf, breaks, Inf)
  
  
  # Classification des valeurs dans les classes correspondantes
  data[[col_to_convert]] <- cut(col_values, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  return(data)
}


#Fonction pour vérifier les valeurs manquantes dans un dataframe
check_missing_values <- function(data) {
  missing_summary <- sapply(data, function(x) sum(is.na(x)))  # Compter les NA par colonne
  
  if (sum(missing_summary) > 0) {
    cat("Il y a des valeurs manquantes dans les colonnes suivantes :\n")
    print(missing_summary[missing_summary > 0])  # Afficher les colonnes avec des NA et leur nombre
  } else {
    cat("Aucune valeur manquante dans le dataset.\n")
  }
}

# Fonction pour entraîner un SVM avec différents noyaux et retourner l'AUC
evaluate_svm_kernel <- function(train_data, test_data, y_train, y_test, kernel_type) {
  # Entraîner le modèle SVM avec un noyau donné
  svm_model <- svm(y_train ~ ., data = train_data, kernel = kernel_type, probability = TRUE)
  
  # Prédiction sur les données de test
  predictions_prob_svm <- predict(svm_model, newdata = test_data, probability = TRUE)
  
  # Extraire les probabilités pour la classe positive
  predictions_prob_svm_values <- attr(predictions_prob_svm, "probabilities")[, 2]
  
  # Calculer l'AUC pour le modèle SVM
  roc_curve_svm <- roc(test_data$y_test, predictions_prob_svm_values)
  auc_value_svm <- auc(roc_curve_svm)
  
  # Retourner l'AUC et le noyau testé
  return(list(kernel = kernel_type, auc = auc_value_svm))
}

# Autres fonctions utilitaires peuvent être ajoutées ici
