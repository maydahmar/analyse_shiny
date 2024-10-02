# R/modeling.R

library(caret)  # Pour l'entraînement des modèles
library(e1071)  # Pour SVM
library(rpart)  # Pour les arbres de décision
library(pROC)   # Pour calculer l'AUC

# Fonction pour entraîner différents modèles de prédiction
train_models <- function(data) {
  # Préparation des données
  set.seed(123)
  train_index <- createDataPartition(data$churn, p = 0.7, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Modèle : Arbre de Décision
  decision_tree <- rpart(churn ~ ., data = train_data)
  dt_pred <- predict(decision_tree, newdata = test_data, type = "prob")[,2]
  
  # Modèle : Régression Logistique
  logistic_model <- glm(churn ~ ., data = train_data, family = "binomial")
  lr_pred <- predict(logistic_model, newdata = test_data, type = "response")
  
  # Modèle : SVM sans kernel
  svm_model <- svm(churn ~ ., data = train_data, probability = TRUE)
  svm_pred <- predict(svm_model, newdata = test_data, probability = TRUE)[,2]
  
  # Modèle : SVM avec kernel
  svm_kernel_model <- svm(churn ~ ., data = train_data, kernel = "radial", probability = TRUE)
  svm_kernel_pred <- predict(svm_kernel_model, newdata = test_data, probability = TRUE)[,2]
  
  # Calculer l'AUC
  auc_results <- data.frame(
    Model = c("Decision Tree", "Logistic Regression", "SVM (No Kernel)", "SVM (With Kernel)"),
    AUC = c(
      auc(roc(test_data$churn, dt_pred)),
      auc(roc(test_data$churn, lr_pred)),
      auc(roc(test_data$churn, svm_pred)),
      auc(roc(test_data$churn, svm_kernel_pred))
    )
  )
  
  return(auc_results)
}

# Fonction pour afficher l'AUC des modèles
plot_auc <- function(model_results) {
  ggplot(model_results, aes(x = Model, y = AUC, fill = Model)) +
    geom_bar(stat = "identity") +
    labs(title = "AUC des Modèles de Prédiction", y = "AUC")
}

# Fonction pour créer un tableau récapitulatif des résultats des modèles
summary_table <- function(model_results) {
  model_results
}
