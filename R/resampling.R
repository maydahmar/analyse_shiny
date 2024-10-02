# R/resampling.R

library(ROSE)  # Pour le sur-échantillonnage/sous-échantillonnage

# Fonction pour appliquer des méthodes de sur-échantillonnage/sous-échantillonnage
apply_sampling_methods <- function(data) {
  # Sur-échantillonnage avec SMOTE
  smote_data <- SMOTE(churn ~ ., data = data, perc.over = 100, perc.under = 200)
  smote_results <- train_models(smote_data)
  
  # Sous-échantillonnage aléatoire
  undersample_data <- ovun.sample(churn ~ ., data = data, method = "under", N = min(table(data$churn) * 2))$data
  undersample_results <- train_models(undersample_data)
  
  # Combiner les résultats
  combined_results <- rbind(
    smote_results %>% mutate(Method = "SMOTE"),
    undersample_results %>% mutate(Method = "Random Undersampling")
  )
  
  return(combined_results)
}

# Fonction pour afficher l'AUC après sampling
plot_sampling_auc <- function(sampling_results) {
  ggplot(sampling_results, aes(x = Method, y = AUC, fill = Method)) +
    geom_bar(stat = "identity") +
    labs(title = "AUC après Sur-échantillonnage/Sous-échantillonnage", y = "AUC")
}

# Fonction pour créer un tableau récapitulatif des résultats après sampling
summary_sampling_table <- function(sampling_results) {
  sampling_results
}
