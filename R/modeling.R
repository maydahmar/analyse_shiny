# R/modeling.R

library(caret)  # Pour l'entraînement des modèles
library(e1071)  # Pour SVM
library(rpart)  # Pour les arbres de décision
library(pROC)   # Pour calculer l'AUC

# Charger les fonctions depuis le fichier functions.R
source("R/functions.R")

# Fonction pour entraîner différents modèles de prédiction
prepare_data_bank <- function() {
  # Préparation des données
  
  # Charger les jeux de données
  #credit_fraud_data <- load_credit_fraud()
  bank_marketing_data <- load_bank_marketing()
  #employee_attrition_data <- load_employee_attrition()
  
  
  # Exemple de transformation des colonnes catégorielles en numériques pour Bank Marketing
  bank_marketing_data <- convert_nominal_to_numeric(bank_marketing_data, c("job", "marital", "education","default","housing","loan","poutcome","contact","month","day_of_week"))
  bank_marketing_data <- convert_continuous_to_bins(bank_marketing_data, c("age"),num_bins = 5, start = 32, end = 65)
  bank_marketing_data <- convert_continuous_to_bins(bank_marketing_data, c("duration"),num_bins = 5, start = 102, end = 644.5)
  
  # Transformer la variable 'y' de 'yes'/'no' en '1'/'0'
  bank_marketing_data$y <- ifelse(bank_marketing_data$y == "yes", 1, 0)
  
  split_index <- createDataPartition(bank_marketing_data$y, p = 0.8, list = FALSE)
  
  # Données d'entraînement
  train_data <- bank_marketing_data[split_index, ]
  
  # Données de test
  test_data <- bank_marketing_data[-split_index, ]
  
  # Séparez les variables explicatives (features) et la variable cible
  X_train <- train_data[ , -which(names(train_data) == "y")]  # Toutes les colonnes sauf y
  y_train <- train_data$y  # La variable cible
  
  X_test <- test_data[ , -which(names(test_data) == "y")]  # Toutes les colonnes sauf y
  y_test <- test_data$y  # La variable cible
  
  # Standardisation des données
  #X_train <- scale(X_train)  # Standardiser les données d'entraînement
  #X_test <- scale(X_test, center = attr(X_train, "scaled:center"), scale = attr(X_train, "scaled:scale"))  # Utiliser les mêmes paramètres pour le test
  
  
  # Retourner une liste contenant les deux datasets
  return(list(X_train= X_train, X_test = X_test, y_train = y_train, y_test = y_test))
}



train_bank <- function(train_test_data) {
  # Extraire les ensembles d'entraînement et de test
  X_train <- as.data.frame(train_test_data$X_train)
  y_train <- as.factor(train_test_data$y_train)
  
  X_test <- as.data.frame(train_test_data$X_test)
  y_test <- as.factor(train_test_data$y_test)
  
  # Créer un dataframe d'entraînement et de test
  train_data <- cbind(y_train, X_train)
  test_data <- cbind(y_test, X_test)
  
  # 1. Construire l'arbre de décision avec les paramètres par défaut
  decision_tree_model <- rpart(y_train ~ ., data = train_data, method = "class")
  
  
  # Prédiction sur les données de test
  predictions_prob_dt <- predict(decision_tree_model, newdata = test_data, type = "prob")[, 2]
  
  # Calculer l'AUC pour l'arbre de décision
  roc_curve_dt <- roc(test_data$y_test, predictions_prob_dt)
  auc_value_dt <- auc(roc_curve_dt)
  
  # Afficher l'AUC de l'arbre de décision
  print(paste("AUC de l'arbre de décision :", auc_value_dt))
  
  # 2. Construire le modèle de régression logistique
  logistic_model <- glm(y_train ~ ., data = train_data, family = binomial)
  
  # Résumé du modèle
  summary(logistic_model)
  
  # Prédiction sur les données de test
  predictions_prob_logistic <- predict(logistic_model, newdata = test_data, type = "response")
  
  # Convertir les probabilités en classes (0 ou 1) avec un seuil de 0.5
  predictions_logistic <- ifelse(predictions_prob_logistic > 0.5, 1, 0)
  
  # Calculer l'AUC pour la régression logistique
  roc_curve_logistic <- roc(test_data$y_test, predictions_logistic)
  auc_value_logistic <- auc(roc_curve_logistic)
  
  # Afficher l'AUC de la régression logistique
  print(paste("AUC de la régression logistique :", auc_value_logistic))
  
  
  
  
  
  # 3. Construire le modèle SVM sans noyau
  svm_model <- svm(y_train ~ ., data = train_data, probability = TRUE)
  
  # Prédiction sur les données de test
  predictions_prob_svm <- predict(svm_model, newdata = test_data, probability = TRUE)
 
  predictions_prob_svm_values <- attr(predictions_prob_svm, "probabilities")[, 2]  # Probabilité pour la classe positive
  
  # Calculer l'AUC pour le modèle SVM
  roc_curve_svm <- roc(test_data$y_test, predictions_prob_svm_values)
  auc_value_svm <- auc(roc_curve_svm)
  
  # Afficher l'AUC du modèle SVM sans noyau
  print(paste("AUC du modèle SVM sans noyau :", auc_value_svm))
  
  #Tester les différents noyaux
  kernels <- c("linear", "radial", "polynomial", "sigmoid")
  results <- lapply(kernels, function(k) evaluate_svm_kernel(train_data, test_data, y_train, y_test, k))
  
  # Afficher les résultats
  for (result in results) {
    print(paste("Kernel:", result$kernel, "- AUC:", result$auc))
  }
  
  # Trouver le noyau avec la meilleure AUC
  best_result <- results[[which.max(sapply(results, function(x) x$auc))]]
  print(paste("Meilleur kernel:", best_result$kernel, "- AUC:", best_result$auc))
  
}
# Fonction pour afficher l'AUC des modèles
plot_auc <- function(model_results) {
  
}

# Fonction pour créer un tableau récapitulatif des résultats des modèles
summary_table <- function(model_results) {
}

train_test = prepare_data_bank()
train_bank(train_test)

