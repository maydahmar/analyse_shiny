# R/modeling.R

library(caret)  # Pour l'entraînement des modèles
library(e1071)  # Pour SVM
library(rpart)  # Pour les arbres de décision
library(pROC)   # Pour calculer l'AUC
library(tidyverse)
library(glmnet)


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
  X_train <- scale(X_train)  # Standardiser les données d'entraînement
  X_test <- scale(X_test, center = attr(X_train, "scaled:center"), scale = attr(X_train, "scaled:scale"))  # Utiliser les mêmes paramètres pour le test
  
  
  # Retourner une liste contenant les deux datasets
  return(list(X_train= X_train, X_test = X_test, y_train = y_train, y_test = y_test))
}

prepare_data_attrition <- function() {
  # Charger les données et retirer les valeurs manquantes
  attrition <- load_employee_attrition()
  attrition <- na.omit(attrition)
  
  # Transformer la variable 'Attrition' de 'yes'/'no' en '1'/'0'
  attrition$Attrition <- ifelse(attrition$Attrition == "Yes", 1, 0)
  
  # Convertir certaines colonnes catégorielles en variables numériques (mais pas pour normalisation)
  attrition <- convert_nominal_to_numeric(attrition, c("BusinessTravel", "Department", "EducationField","Gender","JobRole","MaritalStatus","Over18"))
  
  # Supprimer les colonnes de type factor avec un seul niveau
  attrition <- attrition[, sapply(attrition, function(x) !(is.factor(x) && nlevels(x) < 2))]
  
  attrition <- remove_constant_columns(attrition)
  
  # Séparer les données en train et test
  split_index <- createDataPartition(attrition$Attrition, p = 0.8, list = FALSE)
  
  # Données d'entraînement
  train_data <- attrition[split_index, ]
  
  # Données de test
  test_data <- attrition[-split_index, ]
  
  # Séparer les variables explicatives (X) et la variable cible (y)
  X_train <- train_data[ , -which(names(train_data) == "Attrition")]  # Toutes les colonnes sauf Attrition
  y_train <- train_data$Attrition  # La variable cible
  
  X_test <- test_data[ , -which(names(test_data) == "Attrition")]  # Toutes les colonnes sauf Attrition
  y_test <- test_data$Attrition  # La variable cible
  
  # Identifier les colonnes numériques et catégorielles
  numeric_columns <- sapply(X_train, is.numeric)
  categorical_columns <- !numeric_columns  # Complément des colonnes numériques
  
  # Appliquer le scaling uniquement sur les colonnes numériques
  X_train_numeric <- scale(X_train[ , numeric_columns])  # Normaliser uniquement les colonnes numériques
  X_test_numeric <- scale(X_test[ , numeric_columns], 
                          center = attr(X_train_numeric, "scaled:center"), 
                          scale = attr(X_train_numeric, "scaled:scale"))  # Utiliser les mêmes paramètres pour le test
  
  # Réassembler les données avec les variables catégorielles non normalisées
  X_train <- cbind(X_train_numeric, X_train[ , categorical_columns])  # Réunir colonnes numériques normalisées + catégorielles
  X_test <- cbind(X_test_numeric, X_test[ , categorical_columns])     # Faire de même pour les données de test
  
  # Retourner une liste contenant les deux datasets
  return(list(X_train = X_train, X_test = X_test, y_train = y_train, y_test = y_test))
}

train_decision_tree <- function(train_data, test_data, y_train, y_test){
  # 1. Construire l'arbre de décision avec les paramètres par défaut
  decision_tree_model <- rpart(y_train ~ ., data = train_data, method = "class")
  
  # Prédiction sur les données de test
  predictions_prob_dt <- predict(decision_tree_model, newdata = test_data, type = "prob")[, 2]
  
  # Calculer l'AUC pour l'arbre de décision
  roc_curve_dt <- roc(test_data$y_test, predictions_prob_dt)
  auc_value_dt <- auc(roc_curve_dt)
  
  predictions <- ifelse(predictions_prob_dt > 0.5, 1, 0)
  accuracy <- sum(predictions == y_train) / length(y_train)
  confusion_matrix <- table(Predicted = predictions, Actual = y_train)
  
  
  return (list(roc_curve = roc_curve_dt,
              auc = auc_value_dt,
              accuracy = accuracy,
              confusion_matrix = confusion_matrix,
              tree = decision_tree_model
              ))
}

train_grid_tree <- function(train_data, test_data, y_train, y_test){
  # 2. Grid search pour l'arbre de décision
  best_tree <- grid_search_tree(train_data, test_data, y_train, y_test)
  
  # Afficher les meilleurs hyperparamètres et l'AUC
  print(paste("Meilleur minsplit :", best_tree$best_minsplit))
  print(paste("Meilleur maxdepth :", best_tree$best_maxdepth))
  print(paste("Meilleure AUC :", best_tree$best_auc))
}

train_glm <-function(train_data, test_data, y_train, y_test){
  # 3. Construire le modèle de régression logistique avec les paramètres par défaut
  logistic_model <- glm(y_train ~ ., data = train_data, family = binomial)

  # Calculer l'AUC pour la régression logistique
  predictions_prob_logistic <- predict(logistic_model, newdata = test_data, type = "response")
  roc_curve_logistic <- roc(test_data$y_test, predictions_prob_logistic)
  auc_value_logistic <- auc(roc_curve_logistic)
  
  predictions <- ifelse(predictions_prob_logistic > 0.5, 1, 0)
  accuracy <- sum(predictions == y_train) / length(y_train)
  confusion_matrix <- table(Predicted = predictions, Actual = y_train)
  
  
  return (list(roc_curve = roc_curve_dt,
               auc = auc_value_dt,
               accuracy = accuracy,
               confusion_matrix = confusion_matrix
  ))
}

train_grid_glm <-function(train_data, test_data, y_train, y_test){
  # 4. Construire la régression logistique avec pénalisation
  best_logistic <- grid_search_logistic(X_train, y_train, X_test, y_test)

  # Afficher les meilleurs hyperparamètres et l'AUC
  print(paste("Meilleur alpha :", best_logistic$alpha))
  print(paste("Meilleur lambda :", best_logistic$lambda))
  print(paste("Meilleure AUC :", best_logistic$auc))
}

train_svm_linear <- function(train_data, test_data, y_train, y_test){
  # 5. Construire le modèle SVM sans noyau
  svm_model <- svm(y_train ~ ., data = train_data, probability = TRUE)
  
  # Prédiction sur les données de test
  predictions_prob_svm <- predict(svm_model, newdata = test_data, probability = TRUE)
  
  predictions_prob_svm_values <- attr(predictions_prob_svm, "probabilities")[, 2]  # Probabilité pour la classe positive
  
  # Calculer l'AUC pour le modèle SVM
  roc_curve_svm <- roc(test_data$y_test, predictions_prob_svm_values)
  auc_value_svm <- auc(roc_curve_svm)
  
  predictions <- ifelse(predictions_prob_logistic > 0.5, 1, 0)
  accuracy <- sum(predictions == y_train) / length(y_train)
  confusion_matrix <- table(Predicted = predictions, Actual = y_train)
  
  return (list(roc_curve = roc_curve_dt,
               auc = auc_value_dt,
               accuracy = accuracy,
               confusion_matrix = confusion_matrix
  ))
  
}

train_grid_svm_linear <-  function(train_data, test_data, y_train, y_test){
  # 6. Grid search SVM sans noyau 
  best_svm <- grid_search_svm(train_data, test_data, y_train, y_test,'linear')
  
  # Afficher les meilleurs hyperparamètres et l'AUC
  print(paste("Meilleur C :", best_svm$C))
  print(paste("Meilleur gamma :", best_svm$gamma))
  print(paste("Meilleure AUC :", best_svm$auc))
}

train_svm_non_linear <- function(train_data, test_data, y_train, y_test){
  # 7. Recherche du meilleur noyau (polynomial)
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

#train_test = prepare_data_bank()
#train_bank_grid(train_test)
train_test <- prepare_data_attrition()
train_bank_grid(train_test)


# R/modeling_server.R

# Fonction serveur pour la section Modeling
modeling_server <- function(input, output, session, selected_dataset) {
  
  # Réagir lorsque le bouton "Train Model" est cliqué
  observeEvent(input$train_model, {
    
    dataset <- selected_dataset()
    
    # Vérifier quel dataset est sélectionné
    if (input$model_choice == "bank-additional-full.csv") {
      # Préparer les données pour le dataset Bank Marketing
      train_test_data <- prepare_data_bank()
      
    } else if (input$model_choice == "whole data.csv") {
      # Préparer les données pour le dataset Employee Attrition
      train_test_data <- prepare_data_attrition()
      
    } else if (input$model_choice == "creditcard.csv") {
      # Si tu as une fonction pour le dataset Credit Fraud
      #train_test_data <- prepare_data_credit_fraud()  # À adapter selon ton besoin
      
    } else {
      stop("Dataset non pris en charge")
    }
    
    # Extraire les ensembles d'entraînement et de test
    X_train <- as.data.frame(train_test_data$X_train)
    y_train <- as.factor(train_test_data$y_train)
    
    X_test <- as.data.frame(train_test_data$X_test)
    y_test <- as.factor(train_test_data$y_test)
    
    # Créer un dataframe d'entraînement et de test
    train_data <- cbind(y_train, X_train)
    test_data <- cbind(y_test, X_test)
    
    
    # Réagir selon le choix du modèle
    if (input$model_choice == "Decision Tree") {
      # Paramètres spécifiques à l'arbre de décision
      model <- train_decision_tree(train_data, test_data, y_train, y_test)
      
      
      roc_curve = model$roc_curve
      auc = model$auc
      accuracy = model$accuracy
      confusion_matrix = model$confusion_matrix
      tree = model$tree
      
      
      # Mise à jour des sorties
      output$model_accuracy <- renderText({ paste("Accuracy:", round(accuracy, 3)) })
      output$confusion_matrix <- renderTable({ confusion_matrix })
      
      # Optionnel : Importance des variables
      output$feature_importance <- renderPlot({
        varImpPlot(tree_model)
      })
      
    } else if (input$model_choice == "SVM Linear") {
      # Paramètres spécifiques au SVM
      model <- train_svm_linear(train_data, test_data, y_train, y_test)
      
      roc_curve = model$roc_curve
      auc = model$auc
      accuracy = model$accuracy
      confusion_matrix = model$confusion_matrix
      
      # Mise à jour des sorties
      output$model_accuracy <- renderText({ paste("Accuracy:", round(accuracy, 3)) })
      output$confusion_matrix <- renderTable({ confusion_matrix })
      
    } else if (input$model_choice == "Logistic Regression") {
      # Paramètres spécifiques à la régression logistique
      model <- train_glm(train_data, test_data, y_train, y_test)
      
      roc_curve = model$roc_curve
      auc = model$auc
      accuracy = model$accuracy
      confusion_matrix = model$confusion_matrix
      
      # Mise à jour des sorties
      output$model_accuracy <- renderText({ paste("Accuracy:", round(accuracy, 3)) })
      output$confusion_matrix <- renderTable({ confusion_matrix })
      
    }
    
  })
}
