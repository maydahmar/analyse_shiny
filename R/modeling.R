# R/modeling.R

library(caret)  # Pour l'entraînement des modèles
library(e1071)  # Pour SVM
library(rpart)  # Pour les arbres de décision
library(pROC)   # Pour calculer l'AUC
library(tidyverse)
library(glmnet)
# Load the required libraries
library(ggplot2)
library(reshape2)


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
  accuracy <- sum(predictions == y_test) / length(y_test)
  confusion_matrix <- table(Predicted = predictions, Actual = y_test)
  
  
  return (list(roc_curve = roc_curve_dt,
              auc = auc_value_dt,
              accuracy = accuracy,
              confusion_matrix = confusion_matrix,
              tree = decision_tree_model
              ))
}

train_grid_tree <- function(train_data, test_data, y_train, y_test) {
  # Définir les grilles d'hyperparamètres
  minsplit_values <- seq(2, 10, by = 2)
  maxdepth_values <- seq(1, 10, by = 2)
  
  # Créer un data.frame pour stocker les résultats
  results <- data.frame(minsplit = integer(),
                        maxdepth = integer(),
                        auc = numeric(),
                        accuracy = numeric())
  
  for (minsplit in minsplit_values) {
    for (maxdepth in maxdepth_values) {
      # Entraîner l'arbre de décision avec les hyperparamètres actuels
      tree_model <- rpart(y_train ~ ., data = train_data, 
                          method = "class", 
                          control = rpart.control(minsplit = minsplit, maxdepth = maxdepth))
      
      # Prédiction sur les données de test
      predictions_prob <- predict(tree_model, newdata = test_data, type = "prob")[, 2]
      predictions <- ifelse(predictions_prob > 0.5, 1, 0)
      
      # Calcul des performances
      roc_curve <- roc(y_test, predictions_prob)
      auc_value <- auc(roc_curve)
      accuracy <- sum(predictions == y_test) / length(y_test)
      
      # Stocker les résultats dans le data.frame
      results <- rbind(results, data.frame(minsplit = minsplit, maxdepth = maxdepth, 
                                           auc = auc_value, accuracy = accuracy))
    }
  }
  return(results)
}


train_glm <-function(train_data, test_data, y_train, y_test){
  # 3. Construire le modèle de régression logistique avec les paramètres par défaut
  logistic_model <- glm(y_train ~ ., data = train_data, family = binomial)

  # Calculer l'AUC pour la régression logistique
  predictions_prob_logistic <- predict(logistic_model, newdata = test_data, type = "response")
  roc_curve_logistic <- roc(test_data$y_test, predictions_prob_logistic)
  auc_value_logistic <- auc(roc_curve_logistic)
  
  predictions <- ifelse(predictions_prob_logistic > 0.5, 1, 0)
  accuracy <- sum(predictions == y_test) / length(y_test)
  confusion_matrix <- table(Predicted = predictions, Actual = y_test)
  
  
  return (list(roc_curve = roc_curve_logistic,
               auc = auc_value_logistic,
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

train_svm <- function(train_data, test_data, y_train, y_test, kernel){
  # 5. Construire le modèle SVM sans noyau
  svm_model <- svm(y_train ~ ., data = train_data, probability = TRUE, kernel = kernel)
  
  # Prédiction sur les données de test
  predictions_prob_svm <- predict(svm_model, newdata = test_data, probability = TRUE)
  
  predictions_prob_svm_values <- attr(predictions_prob_svm, "probabilities")[, 2]  # Probabilité pour la classe positive
  
  # Calculer l'AUC pour le modèle SVM
  roc_curve_svm <- roc(test_data$y_test, predictions_prob_svm_values)
  auc_value_svm <- auc(roc_curve_svm)
  
  predictions <- ifelse(predictions_prob_svm_values > 0.5, 1, 0)
  accuracy <- sum(predictions == y_test) / length(y_test)
  confusion_matrix <- table(Predicted = predictions, Actual = y_test)
  
  return (list(roc_curve = roc_curve_svm,
               auc = auc_value_svm,
               accuracy = accuracy,
               confusion_matrix = confusion_matrix
  ))
  
}

train_grid_svm_linear <- function(train_data, test_data, y_train, y_test) {
  # Grille de paramètres
  cost_values <- seq(0.1, 1, by = 0.2)
  
  # DataFrame pour stocker les résultats
  results <- data.frame(cost = numeric(),
                        auc = numeric(),
                        accuracy = numeric())
  
  for (cost in cost_values) {
    # Entraîner le modèle SVM avec les hyperparamètres actuels
    svm_model <- svm(y_train ~ ., data = train_data, 
                      kernel = "linear", cost = cost, probability = TRUE)
      
    # Prédictions
    predictions_prob <- predict(svm_model, newdata = test_data, probability = TRUE)
    predictions_prob_values <- attr(predictions_prob, "probabilities")[, 2]
    predictions <- ifelse(predictions_prob_values > 0.5, 1, 0)
      
    # Calcul des performances
    roc_curve <- roc(y_test, predictions_prob_values)
    auc_value <- auc(roc_curve)
    accuracy <- sum(predictions == y_test) / length(y_test)
      
    # Stocker les résultats
    results <- rbind(results, data.frame(cost = cost, auc = auc_value, accuracy = accuracy))
  }
  return(results)
}


train_svm_non_linear <- function(train_data, test_data, y_train, y_test) {
  # Liste des noyaux à tester
  kernels <- c("radial", "polynomial", "sigmoid")
  
  # Appliquer la fonction à chaque noyau et obtenir un dataframe
  results <- do.call(rbind, lapply(kernels, function(k) evaluate_svm_kernel(train_data, test_data, y_train, y_test, k)))
  
  return(results)  # Retourner les résultats sous forme de dataframe
}




# Fonction serveur pour la section Modeling
modeling_server <- function(input, output, session, selected_dataset, dataset_choice) {
  
  # Réagir lorsque le bouton "Train Model" est cliqué
  observeEvent(input$train_model, {
    
    dataset <- selected_dataset()
    
    # Vérifier quel dataset est sélectionné
    if ("y" %in% names(dataset)) {
      # Préparer les données pour le dataset Bank Marketing
      train_test_data <- prepare_data_bank()
      
    } else if ("Attrition" %in% names(dataset)) {
      # Préparer les données pour le dataset Employee Attrition
      train_test_data <- prepare_data_attrition()
      
    } else if ("Class" %in% names(dataset)) {
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
    
    print(input$model_choice)
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
      output$model_auc <- renderText({ paste("AUC:", round(auc, 3)) })
      output$roc_curve <- renderPlot({ plot.roc(roc_curve, col = "blue")})
      output$confusion_matrix <- renderPlot({
        # Convert the confusion matrix to a data frame for ggplot
        cm_df <- as.data.frame(confusion_matrix)
          
        # Create a heatmap with ggplot2
        ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "lightblue", high = "blue") +
        geom_text(aes(label = Freq), color = "white", size = 6) +
        labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 14))
      })
      
      # Optionnel : Importance des variables
      output$feature_importance <- renderPlot({
        importance <- tree$variable.importance  # Access variable importance from the decision tree model
        if (!is.null(importance)) {
          barplot(importance, main = "Feature Importance", col = "lightblue", las = 2)
        } else {
          plot.new()  # Create an empty plot if importance is NULL
          text(0.5, 0.5, "No feature importance available", cex = 1.5)
        }
      })
      
      # Fonction serveur pour afficher les résultats du Grid Search
      output$grid_results <- renderPlot({
        results <- train_grid_tree(train_data, test_data, y_train, y_test)
        
        ggplot(results, aes(x = minsplit, y = maxdepth, fill = auc)) +
          geom_tile() +
          scale_fill_gradient(low = "lightblue", high = "blue") +
          geom_text(aes(label = round(auc, 2)), color = "white") +
          labs(title = "Grid Search Results for Decision Tree (AUC)",
               x = "Min Samples Split", y = "Max Depth") +
          theme_minimal()
      })
      
      
    } else if (input$model_choice == "SVM Linear") {
      # Paramètres spécifiques au SVM
      model <- train_svm(train_data, test_data, y_train, y_test, 'linear')
      
      roc_curve = model$roc_curve
      auc = model$auc
      accuracy = model$accuracy
      confusion_matrix = model$confusion_matrix
      
      # Mise à jour des sorties
      output$model_accuracy <- renderText({ paste("Accuracy:", round(accuracy, 3)) })
      output$model_auc <- renderText({ paste("AUC:", round(auc, 3)) })
      output$roc_curve <- renderPlot({ plot.roc(roc_curve, col = "blue")})
      output$confusion_matrix <- renderPlot({
        # Convert the confusion matrix to a data frame for ggplot
        cm_df <- as.data.frame(confusion_matrix)
        
        # Create a heatmap with ggplot2
        ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "lightblue", high = "blue") +
          geom_text(aes(label = Freq), color = "white", size = 6) +
          labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                axis.text = element_text(size = 12), 
                plot.title = element_text(hjust = 0.5, size = 14))
      })
      output$grid_results <- renderPlot({
        # Exécution du grid search pour la régression logistique
        grid_results <- grid_search_logistic(X_train, y_train, X_test, y_test)
        
        # Visualisation des résultats avec ggplot
        ggplot(grid_results, aes(x = factor(lambda), y = auc, fill = auc)) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_gradient(low = "lightblue", high = "blue") +
          geom_text(aes(label = round(auc, 2)), vjust = -0.5, color = "black", size = 4) +
          labs(title = "Grid Search Results for Logistic Regression (AUC)",
               x = "Lambda (log scale)", y = "AUC") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          facet_wrap(~ alpha, nrow = 1)  # Affichage des résultats par valeur d'alpha
      })
      
      
      
      
    } else if (input$model_choice == "Logistic Regression") {
      # Paramètres spécifiques à la régression logistique
      model <- train_glm(train_data, test_data, y_train, y_test)
      
      roc_curve = model$roc_curve
      auc = model$auc
      accuracy = model$accuracy
      confusion_matrix = model$confusion_matrix
      
      # Mise à jour des sorties
      output$model_accuracy <- renderText({ paste("Accuracy:", round(accuracy, 3)) })
      output$model_auc <- renderText({ paste("AUC:", round(auc, 3)) })
      output$roc_curve <- renderPlot({ plot.roc(roc_curve, col = "blue")})
      output$confusion_matrix <- renderPlot({
        # Convert the confusion matrix to a data frame for ggplot
        cm_df <- as.data.frame(confusion_matrix)
        
        # Create a heatmap with ggplot2
        ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "lightblue", high = "blue") +
          geom_text(aes(label = Freq), color = "white", size = 6) +
          labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                axis.text = element_text(size = 12), 
                plot.title = element_text(hjust = 0.5, size = 14))
      })
    }else if (input$model_choice == "SVM Non Linear") {
      
      # Paramètres spécifiques au SVM
      model <- train_svm(train_data, test_data, y_train, y_test, 'polynomial')
      
      roc_curve = model$roc_curve
      auc = model$auc
      accuracy = model$accuracy
      confusion_matrix = model$confusion_matrix
      
      # Mise à jour des sorties
      output$model_accuracy <- renderText({ paste("Accuracy:", round(accuracy, 3)) })
      output$model_auc <- renderText({ paste("AUC:", round(auc, 3)) })
      output$roc_curve <- renderPlot({ plot.roc(roc_curve, col = "blue")})
      output$confusion_matrix <- renderPlot({
        # Convert the confusion matrix to a data frame for ggplot
        cm_df <- as.data.frame(confusion_matrix)
        
        # Create a heatmap with ggplot2
        ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "lightblue", high = "blue") +
          geom_text(aes(label = Freq), color = "white", size = 6) +
          labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                axis.text = element_text(size = 12), 
                plot.title = element_text(hjust = 0.5, size = 14))
      })
      
      # Appel de la fonction pour comparer les noyaux SVM
      kernel_results <- train_svm_non_linear(train_data, test_data, y_train, y_test)
      
      # Mise à jour des sorties pour les performances des noyaux
      output$grid_results <- renderPlot({
        ggplot(kernel_results, aes(x = kernel, y = auc, fill = auc)) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_gradient(low = "lightblue", high = "blue") +
          geom_text(aes(label = round(auc, 2)), vjust = -0.5, color = "black", size = 4) +
          labs(title = "Comparison of SVM Kernels (AUC)",
               x = "Kernel", y = "AUC") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      
    }
    
  })
}
