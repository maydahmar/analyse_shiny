library(ggplot2)


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
  
  # 1. Afficher les types de colonnes ----
  output$column_types <- renderTable({
    dataset <- selected_dataset()
    
    # Extraire le nom des colonnes et leur type
    col_info <- data.frame(
      "Column" = names(dataset),
      "DataType" = sapply(dataset, class)
    )
    
    return(col_info)
  })
  
  # 2. Proportion de churn ----
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
  
  # Mettre à jour dynamiquement le menu de sélection pour les variables numériques
  observe({
    dataset <- selected_dataset()
    if (is.null(dataset)) return()
    numericCols <- names(dataset)[sapply(dataset, is.numeric)]  # Trouver les colonnes numériques
    updateSelectInput(session, "numericFeature", choices = numericCols)
  })
  
  # Calculer et afficher les statistiques pour la variable numérique sélectionnée
  output$featureStats <- renderTable({
    dataset <- selected_dataset()
    if (is.null(dataset) || is.null(input$numericFeature)) {
      return(data.frame(Statistic = "Aucune variable sélectionnée", Value = NA))
    }
    
    # Extraire la variable sélectionnée
    feature <- dataset[[input$numericFeature]]
    
    # Calcul des statistiques
    stats <- data.frame(
      Statistic = c("Number of Unique Values", "Number of Rows with Missing Values",
                    "Number of Rows with 0", "Number of Rows with Negative Values",
                    "Mean", "Standard Deviation", "Min", "Max", "Median"),
      Value = c(length(unique(na.omit(feature))),
                sum(is.na(feature)),
                sum(feature == 0),
                sum(feature < 0),
                mean(feature, na.rm = TRUE),
                sd(feature, na.rm = TRUE),
                min(feature, na.rm = TRUE),
                max(feature, na.rm = TRUE),
                median(feature, na.rm = TRUE))
    )
    return(stats)
  })
  
  # Générer et afficher un histogramme pour la variable numérique sélectionnée
  output$featureHist <- renderPlot({
    dataset <- selected_dataset()
    if (is.null(dataset) || is.null(input$numericFeature)) return()
    
    # Extraire la variable sélectionnée
    feature <- dataset[[input$numericFeature]]
    
    # Créer l'histogramme
    ggplot(data.frame(Feature = feature), aes(x = Feature)) +
      geom_histogram(bins = 50, fill = "blue", color = "black") +
      labs(x = input$numericFeature, y = "Count") +
      theme_minimal()
  })
  
  # Dynamiser les choix pour les variables numériques
  observe({
    dataset <- selected_dataset()
    numeric_vars <- names(dataset)[sapply(dataset, is.numeric)]
    updateSelectInput(session, "selected_numeric_var", choices = numeric_vars)
  })
  
  # Afficher un histogramme séparé pour les valeurs churn & non churn de la variable sélectionnée
  output$numeric_churn_histogram <- renderPlot({
    dataset <- selected_dataset()
    var <- input$selected_numeric_var
    
    # Vérifier la variable de churn dans le dataset
    churn_col <- if ("Class" %in% names(dataset)) {
      "Class"
    } else if ("y" %in% names(dataset)) {
      "y"
    } else if ("Attrition" %in% names(dataset)) {
      "Attrition"
    } else {
      NULL
    }
    
    # Si la variable churn existe et que la variable numérique est sélectionnée
    if (!is.null(churn_col) && !is.null(var) && churn_col %in% names(dataset) && var %in% names(dataset)) {
      ggplot(dataset, aes_string(x = var, fill = churn_col)) +
        geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
        labs(title = paste("Distribution de", var, "par Churn"),
             x = var, y = "Count") +
        scale_fill_manual(values = c("blue", "red"), name = churn_col) +
        theme_minimal()
    }
  })
  
  observe({
    dataset <- selected_dataset()
    if (is.null(dataset)) return()
    # Identifier les colonnes catégorielles
    categoricalCols <- names(dataset)[sapply(dataset, is.character) | sapply(dataset, is.factor)]
    updateSelectInput(session, "categoricalFeature", choices = categoricalCols)
  })
  
  
  
  
  output$catFeaturePlot <- renderPlot({
    dataset <- selected_dataset()
    if (is.null(dataset) || is.null(input$categoricalFeature)) return()
    
    # Extraire la variable catégorielle sélectionnée
    feature <- dataset[[input$categoricalFeature]]
    
    # Créer un barplot pour la variable catégorielle
    ggplot(data.frame(Feature = feature), aes(x = Feature)) +
      geom_bar(fill = "skyblue") +
      labs(x = input$categoricalFeature, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$cat_churn_plot <- renderPlot({
    dataset <- selected_dataset()
    if (is.null(dataset) || is.null(input$categoricalFeature)) return()
    
    # Identifier la colonne de churn
    churn_col <- if ("Class" %in% names(dataset)) {
      "Class"
    } else if ("y" %in% names(dataset)) {
      "y"
    } else if ("Attrition" %in% names(dataset)) {
      "Attrition"
    } else {
      NULL
    }
    
    # Vérifier si la colonne de churn existe
    if (!is.null(churn_col) && churn_col %in% names(dataset)) {
      ggplot(dataset, aes_string(x = input$categoricalFeature, fill = churn_col)) +
        geom_bar(position = "fill") +
        labs(x = input$categoricalFeature, y = "Proportion", fill = churn_col) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      plot.new()
      text(0.5, 0.5, "Aucune colonne de churn détectée")
    }
  })
  
  
  # Ajouter une nouvelle fonction pour calculer et afficher la matrice de corrélation
  output$correlation_matrix <- renderPlot({
    dataset <- selected_dataset()
    
    # Filtrer uniquement les variables numériques
    numeric_vars <- dataset[, sapply(dataset, is.numeric)]
    
    # Calculer la matrice de corrélation
    corr_matrix <- cor(numeric_vars, use = "complete.obs")
    
    # Afficher la matrice de corrélation sous forme de heatmap
    ggplot(data = as.data.frame(as.table(corr_matrix)), aes(Var1, Var2, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
      coord_fixed()
  })
  
  observe({
    dataset <- selected_dataset()
    if (is.null(dataset)) return()
    
    # Identifier les colonnes catégorielles
    categoricalCols <- names(dataset)[sapply(dataset, is.character) | sapply(dataset, is.factor)]
    
    # Mettre à jour les choix pour les deux listes déroulantes de variables catégorielles
    updateSelectInput(session, "catVar1", choices = categoricalCols)
    updateSelectInput(session, "catVar2", choices = categoricalCols)
  })
  
  # Calculer et afficher le V de Cramér pour les variables sélectionnées
  output$cramersV <- renderText({
    dataset <- selected_dataset()
    var1 <- input$catVar1
    var2 <- input$catVar2
    
    # Vérifier que les deux variables sont bien sélectionnées et existent dans le dataset
    if (!is.null(var1) && !is.null(var2) && var1 != var2 && var1 %in% names(dataset) && var2 %in% names(dataset)) {
      # Créer une table de contingence
      contingency_table <- table(dataset[[var1]], dataset[[var2]])
      
      # Calculer le test du chi-carré
      chisq_test <- chisq.test(contingency_table)
      
      # Calculer le V de Cramér
      cramers_v <- sqrt(chisq_test$statistic / (sum(chisq_test$observed) * (min(dim(chisq_test$observed)) - 1)))
      
      return(paste("Le coefficient de V de Cramér entre", var1, "et", var2, "est :", round(cramers_v, 3)))
    } else {
      return("Veuillez sélectionner deux variables catégorielles différentes.")
    }
  })
  
  
}