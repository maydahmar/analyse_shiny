# R/functions.R

# Charger les librairies nécessaires
library(dplyr)

# Fonction pour charger le jeu de données Credit Fraud
load_credit_fraud <- function() {
  read.csv("data/credit_fraud.csv")
}

# Fonction pour charger le jeu de données Bank Marketing
load_bank_marketing <- function() {
  read.csv("data/bank_marketing.csv")
}

# Fonction pour charger le jeu de données Employee Attrition
load_employee_attrition <- function() {
  read.csv("data/employee_attrition.csv")
}

# Fonction pour vérifier les dimensions et les valeurs manquantes
check_data_quality <- function(data) {
  dims <- dim(data)
  missing_values <- colSums(is.na(data))
  list(dimensions = dims, missing_values = missing_values)
}

# Autres fonctions utilitaires peuvent être ajoutées ici
