exploratory_analysis <- function(input, output, session, selected_dataset) {
  
  # Afficher le tableau du dataset de manière dynamique avec une barre de défilement horizontale
  output$dataset_table <- DT::renderDataTable({
    dataset <- selected_dataset()
    DT::datatable(
      dataset,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE  # Ajouter une barre de défilement horizontale
      ),
      class = 'display nowrap'  # Classe pour permettre le wrapping sans débordement
    )
  })
  
  # Afficher les dimensions du dataset, les valeurs manquantes et les attributs constants
  output$dataset_info <- renderTable({
    dataset <- selected_dataset()
    
    # Calcul des dimensions
    n_rows <- nrow(dataset)
    n_cols <- ncol(dataset)
    
    # Calcul des valeurs manquantes
    missing_values <- sum(is.na(dataset))
    
    # Calcul des attributs constants (colonnes qui ont la même valeur partout)
    constant_attrs <- sum(sapply(dataset, function(x) length(unique(x)) == 1))
    
    # Créer un tableau de résumé
    summary <- data.frame(
      "Metric" = c("Rows", "Columns", "Missing Values", "Constant Attributes"),
      "Count" = c(n_rows, n_cols, missing_values, constant_attrs)
    )
    
    return(summary)
  })
  
  # 1. Proportion de churn ----
  output$churn_plot <- renderPlot({
    dataset <- selected_dataset()
    
    # Vérifier la variable de churn en fonction du dataset
    churn_col <- if ("Class" %in% names(dataset)) {
      "Class"
    } else if ("y" %in% names(dataset)) {
      "y"
    } else if ("Attrition" %in% names(dataset)) {
      "Attrition"
    } else {
      NULL
    }
    
    # Vérifier si la variable de churn existe dans le dataset
    if (!is.null(churn_col) && churn_col %in% names(dataset)) {
      prop_table <- prop.table(table(dataset[[churn_col]]))
      
      # Visualiser la proportion
      barplot(
        prop_table,
        main = paste("Proportion de", churn_col),
        col = c("blue", "red"),
        names.arg = names(prop_table)
      )
    } else {
      plot.new()
      text(0.5, 0.5, "Aucune variable de churn détectée")
    }
  })
  
  # 2. Exploration des variables numériques ----
  output$num_var_histograms <- renderPlot({
    dataset <- selected_dataset()
    num_vars <- sapply(dataset, is.numeric)  # Identifier les variables numériques
    
    # Créer un histogramme pour chaque variable numérique
    par(mfrow = c(2, 2))  # Disposer les graphiques dans une grille
    for (var in names(num_vars[num_vars])) {
      hist(dataset[[var]], main = paste("Histogramme de", var), col = "skyblue", border = "white")
    }
    par(mfrow = c(1, 1))  # Réinitialiser la disposition des graphiques
  })
  
  # 3. Exploration des variables catégorielles ----
  output$cat_var_barplots <- renderPlot({
    dataset <- selected_dataset()
    cat_vars <- sapply(dataset, is.character) | sapply(dataset, is.factor)  # Identifier les variables catégorielles
    
    # Créer un barplot pour chaque variable catégorielle
    par(mfrow = c(2, 2))  # Disposer les graphiques dans une grille
    for (var in names(cat_vars[cat_vars])) {
      barplot(table(dataset[[var]]), main = paste("Barplot de", var), col = "lightgreen", border = "white", las = 2)
    }
    par(mfrow = c(1, 1))  # Réinitialiser la disposition des graphiques
  })
  
  
  
  
  
  
  
}
