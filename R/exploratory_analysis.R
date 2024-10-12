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
  
  
  # Fichier server.R ou partie serveur dans app.R
  output$conclusion_text_bank <- renderUI({
    HTML("
    <div class='analysis-container fade-in'>
  

      <h3>Dimensions du Dataset et Qualité des Données</h3>
      <p>Le dataset contient <strong>41,188 observations</strong> et <strong>21 colonnes</strong>. Aucune valeur manquante n’est présente, ce qui est un point positif pour garantir l'intégrité des analyses.</p>
      
      
      <h3>Proportion de Churn</h3>
      <p>L'analyse de la variable cible <code>y</code> montre un <span class='tooltip'>déséquilibre des classes<span class='tooltiptext'> une majorité des individus n'ont pas churné (répondant 'no'), tandis qu'un petit pourcentage a churné (répondant 'yes'). Cela souligne la nécessité d'une approche spécifique, comme le re-sampling (sur-échantillonnage ou sous-échantillonnage) ou l’utilisation de méthodes adaptées aux classes déséquilibrées (ex. : SMOTE).</p>
      <p>Cet aspect doit être pris en compte lors de la modélisation pour éviter que le modèle ne devienne biaisé en faveur de la classe majoritaire.</p>
      
      <h3>Analyse des Variables Catégorielles</h3>
      </p>Les graphiques produits montrent les répartitions de churn pour les variables catégorielles par exemple:</p>
      <ul>
        <li><strong>Statut Marital</strong> : La majorité des churners et non churners sont mariés. Cependant, la proportion de churners semble plus élevée chez les individus célbataire.</li>
        <li><strong>Job :</strong> Certains métiers comme les etudiants et les retraités semblent présenter une proportion de churn légèrement plus élevée, mais cela reste subtil.</li>
        <li><strong> Contact : </strong>Les individus contactés via téléphone sont en majorité non churners, mais il serait utile de creuser davantage l'effet des différentes méthodes de contact.</li>
        <li><strong>Housing loan (prêt immobilier) :</strong> : Les individus sans prêt immobilier semblent avoir une proportion légèrement plus élevée de churners.</li>
        <li><strong>Éducation :</strong>  Les niveaux d'éducation plus bas (ex. : 'litterate') semblent avoir une proportion de churn plus importante que ceux ayant une éducation plus basique.</li>
      </ul>

      <h3>Analyse des Variables Numériques</h3>
      <ul>
        <p>Des histogrammes montrent la distribution des variables numériques en fonction du churn:</p>
        <li><strong>Age :</strong> On remarque que la population la plus jeune (moins de 30 ans) semble churner moins que la population plus âgée. Les individus entre 30 et 60 ans ont une distribution relativement homogène, mais avec des churners répartis tout au long de la distribution d'âge.</li>
        <li><strong>Durée de l'appel :</strong> Plus la durée de l'appel est longue, plus la probabilité de churn est élevée.</li>
      </ul>

      <h3>Interprétation de la Matrice de Corrélation</h3>

      <ul>
        <li><strong>Durée des appels :</strong> La variable <code>duration</code> est positivement corrélée avec <code>previous</code> (nombre de contacts précédents), indiquant que les clients ayant été contactés plusieurs fois dans le passé passent plus de temps au téléphone. Cela pourrait refléter un engagement accru lors de la campagne.</li>
        
        <li><strong>Facteurs économiques :</strong> Une forte corrélation positive est observée entre <code>emp.var.rate</code> (taux d'emploi) et <code>euribor3m</code> (taux Euribor à 3 mois). Cela est typique d'une relation économique, les deux indicateurs évoluant souvent ensemble.</li>
        
        <li><strong>Fréquence des contacts :</strong> Une légère corrélation positive entre <code>pdays</code> (jours depuis le dernier contact) et <code>campaign</code> (nombre de contacts) montre que les clients contactés plusieurs fois lors d'une campagne ont tendance à être recontactés après une longue période.</li>
        
        <li><strong>Variables de campagne et churn :</strong> La corrélation entre la durée de l’appel et le churn est la plus notable. Les interactions plus longues sont associées à une probabilité plus élevée de churn.</li>
      </ul>

      <h3>Conclusions</h3>
      <p>Les variables telles que la durée de l'appel, le statut marital, l'éducation et l'âge sont étroitement liées au churn. Cela indique des pistes d'optimisation pour les campagnes marketing.</p>

    
    </div>
  ")
  })
  
  # Affichage conditionnel du rapport "Employee Attrition" seulement si le dataset Employee Attrition est sélectionné
  output$conclusion_text_attrition <- renderUI({
    if (input$dataset_choice == "whole data.csv") {  # Remplacez "whole data.csv" par le nom de votre fichier
      HTML("
      <div class='analysis-container fade-in'>
      
        <h3>Dimensions du Dataset et Qualité des Données</h3>
        <p>Le dataset contient <strong>4410 observations</strong> et <strong>29 colonnes</strong>.</p>
        <p><strong>111 valeurs manquantes</strong> sont présentes, ce qui nécessite une gestion des données manquantes avant toute modélisation.</p>
        <p><strong>3 attributs constants</strong> ont été détectés, et ces colonnes peuvent être supprimées pour éviter les variables inutiles.</p>
        
        <h3>Proportion d'Attrition</h3>
        <p>L'analyse de la variable cible <code>Attrition</code> montre un déséquilibre des classes :</p>
        <ul>
          <li><strong>Non Attrition (No)</strong> : environ 84%.</li>
          <li><strong>Attrition (Yes)</strong> : environ 16%.</li>
        </ul>
        <p>Ce déséquilibre des classes peut nécessiter des approches spécifiques telles que le re-sampling ou l’utilisation de modèles adaptés aux données déséquilibrées.</p>
        
        <h3>Analyse des Variables Catégorielles</h3>
        <ul>
          <li><strong>Statut marital</strong> : Les employés célibataires semblent avoir un taux d'attrition légèrement plus élevé par rapport aux employés mariés et divorcés.</li>
          <li><strong>Job Role</strong> : Certains rôles, tels que les <code>Research director</code> et <code>Research scientist</code>, semblent présenter une proportion d'attrition plus élevée.</li>
          <li><strong>Business Travel</strong> : Les employés voyageant fréquemment semblent avoir un taux d'attrition plus élevé.</li>
        </ul>
        
        <h3>Analyse des Variables Numériques</h3>
        <ul>
          <li><strong>Âge</strong> : Les employés plus jeunes (< 30 ans) semblent moins sujets à l'attrition, tandis que la distribution devient homogène pour les employés entre 30 et 60 ans.</li>
          <li><strong>Distance from home: </strong> : la majorité des employés habitent à une distance relativement faible de leur lieu de travail (moins de 5 km). Ces employés ont une faible proportion d'attrition, ce qui peut indiquer que la proximité du lieu de travail est un facteur qui pourrait influencer positivement la rétention des employés. À mesure que la distance entre le domicile et le lieu de travail augmente (de 5 à 30 km), la proportion d'employés qui quittent l'entreprise (en rouge) semble plus homogène, même si la majorité reste des non-attrition (en bleu)</li>
          <li><strong>Monthly Income</strong> : Une faible corrélation a été observée entre l’attrition et le revenu mensuel, mais les employés avec un salaire plus bas semblent plus enclins à quitter l’entreprise.</li>
        </ul>
        
        <h3>Corrélation des Variables Numériques</h3>
        <ul>
          <li><strong>YearsAtCompany</strong> et <strong>YearsWithCurrManager</strong> sont positivement corrélés, ce qui indique que les employés ayant plus d'ancienneté ont également passé plus de temps avec le même manager.</li>
          <li><strong>MonthlyIncome</strong> est fortement corrélé avec <strong>JobLevel</strong>, ce qui est attendu car les niveaux de postes plus élevés tendent à recevoir des salaires plus importants.</li>
          <li><strong>YearsAtCompany</strong> Les nouveaux employés (moins de 5 ans) semblent plus susceptibles de quitter l'entreprise. Les employés ayant une longue ancienneté sont moins susceptibles de partir, ce qui suggère que la rétention s'améliore avec le temps passé dans l'entreprise.</li>
          <li><strong>DistanceFromHome</strong> et <strong>Attrition</strong> ne montrent pas de corrélation directe forte.</li>
        </ul>
        
        <h3>Conclusions</h3>
        <p>Les variables telles que <strong>l'âge</strong>, <strong>le revenu mensuel</strong>, <strong>le rôle dans l'entreprise</strong>, et <strong>le statut marital</strong> sont étroitement liées à l'attrition. Cela indique des pistes d'optimisation pour les stratégies RH visant à réduire le taux d'attrition.</p>
        
        <div>
  <h3>Matrice de corrélation:</h3>
  <ul>
    <li><strong>YearsAtCompany</strong> et <strong>YearsWithCurrManager</strong> sont fortement corrélés positivement, indiquant que les employés restant longtemps dans l'entreprise travaillent souvent avec le même manager.</li>
    <li><strong>MonthlyIncome</strong> est fortement corrélé avec <strong>JobLevel</strong>, ce qui est attendu, car les postes plus élevés reçoivent des salaires plus importants.</li>
    <li><strong>DistanceFromHome</strong> n'a pas de forte corrélation avec l'attrition ou d'autres variables, suggérant qu'elle n'influence pas fortement les départs.</li>
  </ul>

  <h3>Conclusion :</h3>
  <p>Les corrélations observées indiquent que les variables liées à l'ancienneté et à la rémunération influencent plus fortement les décisions d'attrition.</p>
</div>

      
      </div>
      ")
    }
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