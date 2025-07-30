# Serveur principal

# Fonction pour obtenir l'UI du questionnaire
get_questionnaire_ui <- function(page_num, global_config, product_ids) {
  current_page <- paste0("page", page_num)
  cfg <- global_config[[current_page]]
  
  # Vérification plus complète de la configuration
  if (is.null(cfg) || is.null(cfg$produits) || length(cfg$produits) == 0 ||
      is.null(cfg$base) || is.null(cfg$etapes) || length(cfg$etapes) == 0) {
    return(div(class = "alert alert-info",
               style = "margin: 20px; padding: 20px;",
               h4(icon("exclamation-circle"), " Configuration requise"),
               p("Veuillez d'abord configurer complètement cette page dans l'interface d'administration :"),
               tags$ul(
                 if (is.null(cfg$produits) || length(cfg$produits) == 0) 
                   tags$li("Ajouter au moins un produit"),
                 if (is.null(cfg$base)) 
                   tags$li("Définir la base d'évaluation"),
                 if (is.null(cfg$etapes) || length(cfg$etapes) == 0) 
                   tags$li("Sélectionner les étapes d'évaluation")
               ),
               br(),
               actionButton("go_to_admin", "Aller à l'administration", 
                            class = "btn-primary", icon = icon("cog"))
    ))
  }
  
  # Appeler la fonction de rendu du questionnaire
  render_questionnaire_ui(cfg)
}

main_server <- function(input, output, session) {
  
  # Configuration globale réactive
  global_config <- reactiveValues(
    page = 1,
    page1 = list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list()),
    page2 = list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list()),
    panelistes = DEFAULT_PANELISTS,
    admin_logged = FALSE,
    show_admin = FALSE,
    initialized = FALSE,
    temp_products = list(),
    temp_codes = list(),
    products_saved = TRUE,
    show_save_confirmation = FALSE,
    panelist_to_delete = NULL
  )
  
  # IDs des produits réactifs
  product_ids <- reactiveValues(
    page1 = character(0),
    page2 = character(0)
  )
  
  # Reactive values pour les produits dynamiques
  product_counter <- reactiveValues(count = 1)
  
  # Indicateur de changements
  products_changed <- reactiveVal(FALSE)
  
  # Initialisation de la configuration
  observe({
    if (!global_config$initialized) {
      initialize_config(global_config, product_ids)
      global_config$initialized <- TRUE
    }
  })
  
  # Navigation entre les onglets
  observeEvent(input$tabs, {
    if (input$tabs == "page1") {
      global_config$page <- 1
    } else if (input$tabs == "page2") {
      global_config$page <- 2
    }
  })
  
  # Bouton pour aller à l'administration depuis les questionnaires
  observeEvent(input$go_to_admin, {
    updateTabsetPanel(session, "main_tabs", selected = "admin")
  })
  
  # Gestion de l'affichage du bouton admin selon l'état de connexion
  output$adminLogged <- reactive({
    global_config$admin_logged
  })
  outputOptions(output, "adminLogged", suspendWhenHidden = FALSE)
  
  # Indicateur de configuration complète pour chaque page
  output$page1_configured <- reactive({
    cfg <- global_config$page1
    !is.null(cfg$produits) && length(cfg$produits) > 0 && 
      !is.null(cfg$base) && !is.null(cfg$etapes) && length(cfg$etapes) > 0
  })
  outputOptions(output, "page1_configured", suspendWhenHidden = FALSE)
  
  output$page2_configured <- reactive({
    cfg <- global_config$page2
    !is.null(cfg$produits) && length(cfg$produits) > 0 && 
      !is.null(cfg$base) && !is.null(cfg$etapes) && length(cfg$etapes) > 0
  })
  outputOptions(output, "page2_configured", suspendWhenHidden = FALSE)
  
  # Téléchargement des résultats
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("reponses_strength_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (file.exists(OUTPUT_FILE)) {
        file.copy(OUTPUT_FILE, file)
      } else {
        # Créer un fichier vide si aucune réponse n'existe encore
        write.csv(data.frame(Message = "Aucune réponse disponible pour le moment"), 
                  file, row.names = FALSE)
      }
    }
  )
  
  # Appel des modules serveur
  server_admin_logic(input, output, session, global_config, product_ids, products_changed)
  server_config_logic(input, output, session, global_config, product_ids, products_changed)
  server_responses_logic(input, output, session, global_config)
  
  # Gestion des UI de questionnaire avec gestion d'erreur
  output$questionnaire_ui1 <- renderUI({
    tryCatch({
      get_questionnaire_ui(1, global_config, product_ids)
    }, error = function(e) {
      div(class = "alert alert-danger",
          h4("Erreur de configuration"),
          p("Une erreur s'est produite lors du chargement du questionnaire."),
          p("Détail de l'erreur :", e$message))
    })
  })
  
  output$questionnaire_ui2 <- renderUI({
    tryCatch({
      get_questionnaire_ui(2, global_config, product_ids)
    }, error = function(e) {
      div(class = "alert alert-danger",
          h4("Erreur de configuration"),
          p("Une erreur s'est produite lors du chargement du questionnaire."),
          p("Détail de l'erreur :", e$message))
    })
  })
  
  # Debug: Observer pour vérifier les changements de configuration
  observe({
    if (global_config$initialized) {
      cat("=== État de la configuration ===\n")
      cat("Page courante:", global_config$page, "\n")
      cat("Page 1 - Produits:", length(global_config$page1$produits %||% character(0)), "\n")
      cat("Page 1 - Base:", !is.null(global_config$page1$base), "\n")
      cat("Page 1 - Étapes:", length(global_config$page1$etapes %||% character(0)), "\n")
      cat("Page 2 - Produits:", length(global_config$page2$produits %||% character(0)), "\n")
      cat("Page 2 - Base:", !is.null(global_config$page2$base), "\n")
      cat("Page 2 - Étapes:", length(global_config$page2$etapes %||% character(0)), "\n")
      cat("Admin connecté:", global_config$admin_logged, "\n")
      cat("Panélistes:", length(global_config$panelistes), "\n")
      cat("===============================\n")
    }
  })
  
  # Observer pour les changements de produits (pour debug)
  observe({
    if (products_changed()) {
      cat("Changements détectés dans les produits\n")
    }
  })
}
