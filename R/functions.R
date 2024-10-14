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
  read.csv("data/whole data.csv", sep=',')
}

# Fonction pour convertir les colonnes qualitatives nominales en numériques
convert_nominal_to_numeric <- function(data, cols_to_convert) {
  for (col in cols_to_convert) {
    
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      
      # Conversion en facteur (si pas déjà)
      data[[col]] <- as.factor(data[[col]])
      
    }
  }
  return(data)
}

remove_constant_columns <- function(df) {
  # Identifie les colonnes avec une seule valeur unique (constantes)
  non_constant_columns <- df[, sapply(df, function(x) length(unique(x)) > 1)]
  
  # Retourner le data frame sans les colonnes constantes
  return(non_constant_columns)
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
  roc_curve_svm <- roc(y_test, predictions_prob_svm_values)  # Attention à utiliser y_test correctement
  auc_value_svm <- auc(roc_curve_svm)
  
  # Retourner un dataframe avec le noyau testé et son AUC
  return(data.frame(kernel = kernel_type, auc = auc_value_svm))
}





grid_search_svm <- function(train_data, test_data, y_train, y_test, kernel_type) {
  
  # Définir une grille d'hyperparamètres
  if (kernel_type == "linear") {
    grid <- expand.grid(C = c(0.1, 1, 10))  # Pas besoin de gamma pour noyau linéaire
  } else {
    grid <- expand.grid(C = c(0.1, 1, 10), gamma = c(0.01, 0.1, 1))
  }
  
  # Fonction pour ajuster le SVM avec des hyperparamètres spécifiques
  fit_svm <- function(C, gamma = NULL) {
    if (kernel_type == "linear") {
      svm(y_train ~ ., data = train_data, kernel = kernel_type, cost = C, probability = TRUE, max_iter = 10000)
    } else {
      svm(y_train ~ ., data = train_data, kernel = kernel_type, cost = C, gamma = gamma, probability = TRUE, max_iter = 10000)
    }
  }
  
  # Ajuster les modèles pour chaque combinaison d'hyperparamètres
  if (kernel_type == "linear") {
    grid <- grid %>%
      mutate(fit = map(C, fit_svm),  # map au lieu de pmap ici
             auc = map_dbl(fit, compute_auc, 
                           test_features = test_data %>% select(-y_test),  # Modifier pour exclure la colonne des labels
                           test_labels = y_test, 
                           model_type = paste("svm", kernel_type, sep = "_")))
  } else {
    grid <- grid %>%
      mutate(fit = pmap(list(C = C, gamma = gamma), fit_svm),
             auc = map_dbl(fit, compute_auc, 
                           test_features = test_data %>% select(-y_test),  # Même ajustement ici
                           test_labels = y_test, 
                           model_type = paste("svm", kernel_type, sep = "_")))
  }
  
  # Retourner la meilleure combinaison d'hyperparamètres
  best_svm_model <- grid %>%
    arrange(desc(auc)) %>%
    slice(1)
  
  # Retourner les hyperparamètres et le meilleur modèle
  return(list(
    model = best_svm_model$fit[[1]],    # Le meilleur modèle SVM
    auc = best_svm_model$auc,           # La meilleure AUC
    C = best_svm_model$C,               # Le meilleur paramètre C
    gamma = ifelse(kernel_type == "linear", NA, best_svm_model$gamma)  # Le paramètre gamma si applicable
  ))
}



# Ajout de la recherche du meilleur kernel pour SVM
best_svm_kernel <- function(train_data, test_data, y_train, y_test) {
  
  # Tester différents noyaux
  kernels <- c("linear", "radial", "polynomial", "sigmoid")
  results <- lapply(kernels, function(kernel) grid_search_svm(train_data, test_data, y_train, y_test, kernel))
  
  # Comparer les résultats et choisir le meilleur
  best_result <- results[[which.max(sapply(results, function(x) x$auc))]]
  print(paste("Meilleur noyau SVM:", best_result$kernel, "- AUC:", best_result$auc))
  
  return(best_result)
}

grid_search_logistic <- function(X_train, y_train, X_test, y_test) {   
  # Utiliser glmnet pour effectuer une régression logistique avec régularisation   
  # Convertir en matrices (glmnet requiert des matrices)   
  X_train_mat <- as.matrix(X_train)   
  X_test_mat <- as.matrix(X_test)   
  
  # Définir la grille d'hyperparamètres pour alpha (0 = Ridge, 1 = Lasso) et lambda (pénalité)   
  alpha_values <- c(0,0.5, 1)  # alpha = 0 pour Ridge, alpha = 1 pour Lasso   
  lambda_values <- 10^seq(-3, 3, length = 10)  # Valeurs possibles pour lambda
  
  # Créer une dataframe pour stocker les résultats   
  grid_results <- data.frame(alpha = numeric(), lambda = numeric(), auc = numeric())
  
  # Boucle pour parcourir toutes les combinaisons d'alpha et lambda   
  for (alpha in alpha_values) {
    for (lambda in lambda_values) {
      # Ajuster le modèle glmnet       
      logistic_model <- glmnet(X_train_mat, y_train, family = "binomial", alpha = alpha, lambda = lambda)
      
      # Prédire les probabilités pour les données de test       
      predictions_prob_logistic <- predict(logistic_model, newx = X_test_mat, type = "response", s = lambda)
      
      # Calculer l'AUC pour la régression logistique       
      roc_curve_logistic <- roc(y_test, predictions_prob_logistic)
      auc_value_logistic <- auc(roc_curve_logistic)
      
      # Enregistrer les résultats dans le dataframe       
      grid_results <- rbind(grid_results, data.frame(alpha = alpha, lambda = lambda, auc = auc_value_logistic))
    }
  }
  
  # Retourner le dataframe contenant toutes les combinaisons d'alpha, lambda et AUC
  return(grid_results)
}




# Autres fonctions utilitaires peuvent être ajoutées ici