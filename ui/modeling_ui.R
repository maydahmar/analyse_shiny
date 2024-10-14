modeling_ui <- function() {
  fluidPage(
    useShinyjs(),  # Pour la gestion d'éléments interactifs et cachés
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")  # Ajouter du CSS si nécessaire
    ),
    
    titlePanel("Model Analysis and Predictions"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Model Selection"),
        
        # Choix du modèle à entraîner
        selectInput("model_choice", "Choose a Model",
                    choices = c("Decision Tree", "Logistic Regression", "SVM Linear", "SVM Non Linear"),
                    selected = "Decision Tree"),
        
        actionButton("train_model", "Train Model", icon = icon("play"))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Model Performance",
                   fluidRow(
                     box(title = "Model Accuracy", width = 12, status = "primary", solidHeader = TRUE,
                         verbatimTextOutput("model_accuracy")),
                     box(title = "Model AUC", width = 12, status = "info", solidHeader = TRUE,
                         verbatimTextOutput("model_auc")),
                     box(title = "Confusion Matrix", width = 12, status = "info", solidHeader = TRUE,
                         plotOutput("confusion_matrix"))
                   ),
                   fluidRow(
                     box(title = "ROC Curve", width = 12, status = "warning", solidHeader = TRUE,
                         plotOutput("roc_curve")),
                     box(title = "Feature Importance", width = 12, status = "success", solidHeader = TRUE,
                         plotOutput("feature_importance"))
                   )
          ),
          # Nouvel onglet pour les résultats du grid search
          tabPanel("Grid Search Results",
                   fluidRow(
                     box(title = "Grid Search", width = 12, status = "primary", solidHeader = TRUE,
                         plotOutput("grid_results"))
                   )
          ),tabPanel("Bank Marketing",
                     h4("Introduction"),
                     p("Le jeu de données contient des informations sur des campagnes marketing basées sur des appels téléphoniques. L'objectif est de prédire si un client va souscrire à un dépôt à terme ('oui' ou 'non'). Différents modèles de machine learning ont été testés et leurs performances ont été évaluées à l'aide de métriques comme l'exactitude (accuracy) et l'AUC."),
                     p("Les variables clés influençant les prédictions incluent la durée du dernier contact, les indicateurs économiques comme le taux Euribor et la variation de l'emploi, ainsi que les interactions précédentes avec les clients."),
                     
                     h4("Interprétation des Modèles"),
                     h5("Arbre de Décision"),
                     p("L'arbre de décision a montré des résultats décents, mais l'AUC était inférieure à celle des autres modèles. Les principales variables influentes étaient la durée de l'appel, les indicateurs économiques et les interactions passées avec le client."),
                     p("L'analyse du Grid Search a révélé que le paramètre `min_samples_split` n'a pratiquement aucune influence sur l'AUC, et que la profondeur maximale n'améliore plus les performances au-delà de 2."),
                     
                     h5("Régression Logistique"),
                     p("La régression logistique a eu des performances très solides, avec une meilleure capacité de discrimination (AUC élevée). Elle capte efficacement les relations entre les variables socio-économiques et la réponse des clients."),
                     
                     h5("SVM Linéaire et Noyau Polynomial"),
                     p("Le modèle SVM linéaire a des performances similaires à la régression logistique, mais avec une légère différence. Le Grid Search a montré que le noyau polynomial offre de meilleures performances, ce qui suggère que des relations non-linéaires sont présentes dans les données."),
                     
                     h4("Observations Générales"),
                     p("1. **Importance des Variables :** La durée de l'appel et les indicateurs économiques sont les variables les plus influentes dans la prédiction."),
                     p("2. **Comparaison des Modèles :** Bien que l'arbre de décision soit plus simple à interpréter, les modèles de régression logistique et de SVM sont plus performants. L'AUC plus élevée pour ces modèles montre une meilleure capacité à différencier les classes."),
                     p("3. **Complexité des Modèles :** Le modèle SVM avec noyau polynomial a légèrement surpassé les autres, ce qui montre que des relations non linéaires existent dans les données."),
                     
                     h4("Conclusion"),
                     p("L'analyse montre que la régression logistique et les modèles SVM sont les plus performants pour prédire l'adhésion à un dépôt à terme. Le SVM avec noyau polynomial se distingue comme étant légèrement supérieur. Pour aller plus loin, l'intégration de plus de variables socio-économiques et une optimisation plus poussée des hyperparamètres pourraient améliorer encore les résultats.")
          ),
          tabPanel("Employee Attrition",
                   h4("Introduction"),
                   p("Le jeu de données d'Employee Attrition contient des informations sur les employés, et l'objectif est de prédire si un employé quittera l'entreprise ('attrition') ou non."),
                   p("Plusieurs facteurs influencent l'attrition, notamment l'âge, les années passées dans l'entreprise, les voyages professionnels et la satisfaction au travail."),
                   
                   h4("Interprétation des Modèles"),
                   h5("Arbre de Décision"),
                   p("L'arbre de décision a montré une capacité modérée à prédire l'attrition. Les variables les plus importantes étaient les années travaillées dans l'entreprise, l'âge et les voyages professionnels."),
                   p("Le Grid Search a montré que la profondeur maximale influence les performances jusqu'à un certain point, mais que `min_samples_split` n'a pas d'impact notable."),
                   
                   h5("Régression Logistique"),
                   p("La régression logistique a donné des résultats corrects avec un AUC de 0.78, indiquant une bonne capacité de discrimination. Ce modèle capte les relations linéaires entre les variables et l'attrition, en particulier pour des variables telles que l'âge et la satisfaction au travail."),
                   
                   h5("SVM Linéaire et Noyau Polynomial"),
                   p("Le SVM linéaire a montré une performance moyenne. Cependant, le Grid Search a révélé que les noyaux non-linéaires, en particulier les noyaux polynomial et radial, offrent de meilleures performances."),
                   p("Le modèle SVM avec noyau polynomial a donné le meilleur résultat, avec un AUC de 0.883, ce qui indique des relations non-linéaires entre les variables et l'attrition."),
                   
                   h4("Observations Générales"),
                   p("1. **Importance des Variables :** Les années travaillées, l'âge, et les voyages professionnels sont des facteurs déterminants dans la prédiction de l'attrition."),
                   p("2. **Comparaison des Modèles :** Le SVM avec noyau polynomial s'est montré supérieur aux autres modèles en raison de sa capacité à capturer des relations complexes."),
                   p("3. **Facteurs Clés :** La satisfaction des employés (au travail et environnementale) joue un rôle crucial dans la décision de quitter ou non l'entreprise."),
                   
                   h4("Conclusion"),
                   p("L'analyse montre que les modèles non-linéaires, en particulier le SVM polynomial, sont les plus adaptés pour prédire l'attrition. Pour améliorer encore les résultats, des stratégies comme le re-sampling pour traiter le déséquilibre des classes et une optimisation plus poussée des hyperparamètres pourraient être envisagées.")
          )
        )
      )
    )
  )
}
