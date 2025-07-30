# Configuration globale de l'application

# Chemins des fichiers
CONFIG_FILE <- "config.rds"
OUTPUT_FILE <- file.path(Sys.getenv("RSTUDIO_CONNECT_APP_DATA_DIR", unset = "."), "reponses_strength.csv")

# Liste des panélistes par défaut
DEFAULT_PANELISTS <- c("Alice Dupont", 
                       "Invite 1",
                       "Invite 2",
                       "Invite 3",
                       "Invite 4",
                       "Invite 5")

# Choix des étapes disponibles
ETAPES_CHOICES <- c("NEAT", "BLOOM", "WET", "DRY", "DRYER")

# Choix des supports disponibles
SUPPORTS_CHOICES <- c("TS", "TC", "PS", "None")

# Choix des bases disponibles
BASE_CHOICES <- c("T05107", "T051M000G", "E025C004C", "Other")

# Credentials admin
ADMIN_USER <- "admin"
ADMIN_PASS <- "1234"

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
