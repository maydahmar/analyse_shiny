# analyse_shiny


url_app_shiny_inspiration : https://github.com/Public-Health-Scotland/scotpho-profiles-tool/blob/master/shiny_app/ui%20scripts/aboutUI.R

lien de l'application : 
https://scotland.shinyapps.io/ScotPHO_profiles_tool/

structure du projet :
mon_projet_shiny/
│
├── /data/
│   ├── credit_fraud.csv
│   ├── bank_marketing.csv
│   └── employee_attrition.csv
│
├── /R/
│   ├── functions.R                  # Fonctions utilitaires
│   ├── exploratory_analysis.R        # Fonctions pour l'analyse exploratoire
│   ├── modeling.R                   # Fonctions pour la modélisation
│   ├── resampling.R                 # Fonctions pour le sur-échantillonnage/sous-échantillonnage
│   └── hyperparameter_tuning.R      # Fonctions pour le tuning des hyperparamètres
│
├── /ui/                             # Dossier pour l'interface utilisateur
│   ├── exploratory_ui.R             # UI pour l'analyse exploratoire
│   ├── modeling_ui.R                # UI pour la modélisation
│   ├── resampling_ui.R              # UI pour le resampling
│   └── conclusions_ui.R              # UI pour les conclusions
│
├── app.R                            # Fichier principal pour l'application Shiny
├── /www/                            # Dossier pour les ressources (CSS, JS, images)
│   └── style.css                    # Fichier CSS pour le style de l'application
└── README.md                        # Documentation du projet
