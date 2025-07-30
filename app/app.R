# Point d'entrée principal de l'application
library(shiny)
library(shinyjs)
library(readr)
library(dplyr)

# Charger les fichiers de configuration et utilitaires
source("global.R")
source("utils/config_utils.R")
source("utils/data_utils.R")

# Charger les modules UI
source("ui/ui_main.R")
source("ui/ui_admin.R")
source("ui/ui_questionnaire.R")

# Charger les modules serveur
source("server/server_main.R")
source("server/server_admin.R")
source("server/server_config.R")
source("server/server_responses.R")

# Lancement de l'application
shinyApp(ui = main_ui, server = main_server)
