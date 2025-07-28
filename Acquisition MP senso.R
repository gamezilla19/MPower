library(shiny)
library(shinyjs)
library(readr)
library(dplyr)

# Chemin du fichier de config et des réponses
config_file <- "config.rds"
output_file <- file.path(Sys.getenv("RSTUDIO_CONNECT_APP_DATA_DIR", unset = "."), "reponses_strength.csv")

# Liste des panélistes connus
panelistes_connus <- c("Alice Dupont", 
                       "Invite 1",
                       "Invite 2",
                       "Invite 3",
                       "Invite 4",
                       "Invite 5")

# UI (identique)
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .admin-panel {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 1000;
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      .admin-button {
        background-color: #007bff;
        color: white;
        border: none;
        padding: 8px 15px;
        border-radius: 4px;
        cursor: pointer;
        font-size: 12px;
      }
      .admin-button:hover {
        background-color: #0056b3;
      }
      .main-content {
        margin-top: 20px;
      }
      .admin-modal .modal-dialog {
        max-width: 90%;
        width: 1200px;
      }
      .admin-form {
        max-height: 70vh;
        overflow-y: auto;
        padding: 20px;
      }
      .config-section {
        background: #f8f9fa;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        border-left: 4px solid #007bff;
      }
      .product-inputs {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 10px;
        margin-bottom: 20px;
      }
      .product-input-group {
        border: 1px solid #ddd;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      .product-input-group:focus-within {
        border-color: #80bdff !important;
        box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25) !important;
        transition: border-color .15s ease-in-out, box-shadow .15s ease-in-out;
      }
      .btn-sm {
        padding: 3px 8px;
        font-size: 12px;
      }
      .save-confirmation {
        color: #28a745;
        font-weight: bold;
        margin-left: 10px;
        opacity: 0;
        transition: opacity 0.3s ease-in-out;
      }
      .save-confirmation.show {
        opacity: 1;
      }
      .products-section {
        border: 2px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .products-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 15px;
        border-bottom: 1px solid #dee2e6;
        padding-bottom: 10px;
      }
    "))
  ),
  
  # Panel Admin flottant
  div(class = "admin-panel",
      conditionalPanel(
        condition = "!output.adminLogged",
        actionButton("show_login", "Admin", class = "admin-button")
      ),
      conditionalPanel(
        condition = "output.adminLogged",
        actionButton("show_admin_panel", "Configuration", class = "admin-button"),
        br(), br(),
        downloadButton("download_results", "Télécharger", 
                       style = "font-size: 11px; padding: 5px 10px;")
      )
  ),
  
  # Contenu principal
  div(class = "main-content",
      titlePanel("Evaluation de la puissance MP"),
      tabsetPanel(
        id = "tabs",
        
        # Tab QUESTIONNAIRE PAGE 1
        tabPanel("Questionnaire - Page 1",
                 icon = icon("clipboard-list"),
                 br(),
                 uiOutput("questionnaire_ui1")
        ),
        
        # Tab QUESTIONNAIRE PAGE 2
        tabPanel("Questionnaire - Page 2",
                 icon = icon("clipboard-list"),
                 br(),
                 uiOutput("questionnaire_ui2")
        )
      )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Null coalescing operator
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Reactive values pour stocker la configuration de l'etude
  global_config <- reactiveValues(
    page = 1,
    page1 = list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list()),
    page2 = list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list()),
    admin_logged = FALSE,
    show_admin = FALSE,
    initialized = FALSE,
    # Valeurs temporaires pour l'édition des produits
    temp_products = list(),
    temp_codes = list(),
    # État de sauvegarde
    products_saved = TRUE,
    show_save_confirmation = FALSE
  )
  
  # Add reactive values for dynamic product inputs
  product_counter <- reactiveValues(count = 1)
  product_ids <- reactiveVal(1)
  
  # NOUVEAU : Reactive value pour contrôler les changements de produits
  products_changed <- reactiveVal(0)
  
  # DÉLAI DYNAMIQUE basé sur la complexité
  delay_time <- reactive({
    max(100, length(product_ids()) * 10)
  })
  
  # ========== FONCTIONS AMÉLIORÉES ==========
  
  # Fonction de vérification de changements réels
  has_real_changes <- function() {
    current_page_config <- if (global_config$page == 1) global_config$page1 else global_config$page2
    
    # Comparer les produits
    saved_products <- current_page_config$produits %||% character(0)
    temp_products <- unlist(global_config$temp_products) %||% character(0)
    
    # Comparer les codes
    saved_codes <- current_page_config$codes_produits %||% character(0)
    temp_codes <- unlist(global_config$temp_codes) %||% character(0)
    
    # Filtrer les entrées vides pour la comparaison
    temp_products_clean <- temp_products[nzchar(temp_products) & !is.na(temp_products)]
    temp_codes_clean <- temp_codes[nzchar(temp_codes) & !is.na(temp_codes)]
    
    !identical(saved_products, temp_products_clean) || !identical(saved_codes, temp_codes_clean)
  }
  
  # Fonctions de contrôle améliorées pour éviter les boucles
  check_input_integrity <- function(type, id) {
    input_val <- switch(type,
                        "product" = input[[paste0("admin_produit", id)]],
                        "code" = input[[paste0("admin_code_produit", id)]]) %||% ""
    
    stored_val <- switch(type,
                         "product" = global_config$temp_products[[as.character(id)]] %||% "",
                         "code" = global_config$temp_codes[[as.character(id)]] %||% "")
    
    !identical(trimws(input_val), trimws(stored_val))
  }
  
  update_config_safely <- function(type, id, value) {
    isolate({
      if (type == "product") {
        global_config$temp_products[[as.character(id)]] <- value
      } else {
        global_config$temp_codes[[as.character(id)]] <- value
      }
      global_config$products_saved <- FALSE
    })
  }
  
  # Initialize configuration on startup - MODIFIÉ
  observe({
    if (!global_config$initialized && file.exists(config_file)) {
      conf <- readRDS(config_file)
      global_config$page1 <- conf$page1 %||% list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list())
      global_config$page2 <- conf$page2 %||% list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list())
      
      # Initialize product IDs based on existing products
      max_products <- max(
        length(global_config$page1$produits %||% character(0)),
        length(global_config$page2$produits %||% character(0)),
        1
      )
      product_ids(seq_len(max_products))
      
      # Initialisation des valeurs temporaires
      load_temp_products_for_page(global_config$page)
      
      # Forcer l'état sauvegardé pour les deux pages
      global_config$products_saved <- TRUE
      global_config$initialized <- TRUE
    }
  })
  
  # Fonction pour charger les produits temporaires d'une page - CORRIGÉE
  load_temp_products_for_page <- function(page_num) {
    cfg <- if (page_num == 1) global_config$page1 else global_config$page2
    
    # Initialisation robuste avec vérification de longueur
    produits <- cfg$produits %||% character(0)
    codes <- cfg$codes_produits %||% character(0)
    
    # Synchronisation parfaite des IDs
    if (length(produits) == 0 && length(codes) == 0) {
      product_ids(integer(0))  # Aucun ID si vide
      global_config$temp_products <- list()
      global_config$temp_codes <- list()
    } else {
      product_ids(seq_along(produits))
      # Remplissage initial avec noms explicites
      global_config$temp_products <- setNames(as.list(produits), seq_along(produits))
      global_config$temp_codes <- setNames(as.list(codes), seq_along(codes))
    }
    
    # État sauvegardé si aucune modification
    global_config$products_saved <- TRUE
  }
  
  # OBSERVATEURS OPTIMISÉS - VERSION CORRIGÉE avec debounce
  observeEvent(product_ids(), {
    ids <- product_ids()
    lapply(ids, function(i) {
      local({
        id <- i
        input_name <- paste0("admin_produit", id)
        code_name <- paste0("admin_code_produit", id)
        
        # Créer des reactive expressions avec debounce
        product_input_debounced <- debounce(reactive({
          input[[input_name]]
        }), 500)
        
        code_input_debounced <- debounce(reactive({
          input[[code_name]]
        }), 500)
        
        # Observateurs sans throttle
        observeEvent(product_input_debounced(), {
          if (check_input_integrity("product", id)) {
            update_config_safely("product", id, product_input_debounced())
            products_changed(products_changed() + 1)
          }
        }, ignoreInit = TRUE)
        
        observeEvent(code_input_debounced(), {
          if (check_input_integrity("code", id)) {
            update_config_safely("code", id, code_input_debounced())
            products_changed(products_changed() + 1)
          }
        }, ignoreInit = TRUE)
      })
    })
  }, ignoreNULL = FALSE)
  
  # Observer pour changer de page - AMÉLIORÉS
  observeEvent(input$setup_page1, {
    # Vérifier s'il y a des changements réels
    if (has_real_changes()) {
      showNotification("Modifications non sauvegardées détectées", type = "warning")
      return()
    }
    global_config$page <- 1
    load_temp_products_for_page(1)
  })
  
  observeEvent(input$setup_page2, {
    # Vérifier s'il y a des changements réels
    if (has_real_changes()) {
      showNotification("Modifications non sauvegardées détectées", type = "warning")
      return()
    }
    global_config$page <- 2
    load_temp_products_for_page(2)
  })
  
  # Sauvegarde manuelle des produits - AMÉLIORÉE
  observeEvent(input$save_products, {
    # Filtrer les entrées réellement vides
    produits_valides <- unlist(global_config$temp_products) %>% 
      .[nzchar(.) & !is.na(.)]
    codes_valides <- unlist(global_config$temp_codes) %>% 
      .[nzchar(.) & !is.na(.)]
    
    # Validation
    if (length(produits_valides) == 0) {
      showNotification("Aucun produit valide à sauvegarder !", type = "warning")
      return()
    }
    
    if (any(nchar(produits_valides) < 2)) {
      showNotification("Les noms de produits doivent contenir au moins 2 caractères !", type = "error")
      return()
    }
    
    # Mise à jour atomique
    isolate({
      if (global_config$page == 1) {
        global_config$page1$produits <- produits_valides
        global_config$page1$codes_produits <- codes_valides
      } else {
        global_config$page2$produits <- produits_valides
        global_config$page2$codes_produits <- codes_valides
      }
      
      # Sauvegarder la configuration complète
      saveRDS(list(
        page1 = global_config$page1,
        page2 = global_config$page2
      ), file = config_file)
    })
    
    # Marquer comme sauvegardé
    global_config$products_saved <- TRUE
    
    # Feedback visuel
    global_config$show_save_confirmation <- TRUE
    showNotification(paste("Produits de la Page", global_config$page, "sauvegardés avec succès !"), 
                     type = "message", duration = 3)
    
    # Utiliser delay au lieu de once = TRUE
    delay(3000, {
      global_config$show_save_confirmation <- FALSE
    })
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("reponses_strength_", Sys.Date(), ".csv")
    },
    content = function(file) {
      file.copy(output_file, file)
    }
  )  
  
  # Modal de connexion admin
  observeEvent(input$show_login, {
    showModal(modalDialog(
      title = tagList(icon("lock"), "Connexion Administrateur"),
      size = "s",
      fluidRow(
        column(12,
               textInput("admin_user", "Utilisateur", placeholder = "admin"),
               passwordInput("admin_pass", "Mot de passe", placeholder = "••••"),
               br(),
               div(style = "text-align: center;",
                   actionButton("login_btn", "Se connecter", 
                                class = "btn-primary",
                                icon = icon("sign-in-alt"))
               )
        )
      ),
      footer = modalButton("Annuler")
    ))
  })
  
  observeEvent(input$login_btn, {
    if (input$admin_user == "admin" && input$admin_pass == "1234") {
      global_config$admin_logged <- TRUE
      removeModal()
      showNotification("Connexion réussie !", type = "message")
    } else {
      showNotification("Login ou mot de passe incorrect.", type = "error")
    }
  })
  
  # Modal du panel admin
  observeEvent(input$show_admin_panel, {
    showModal(modalDialog(
      title = tagList(icon("cogs"), "Configuration de l'étude"),
      size = "l",
      class = "admin-modal",
      div(class = "admin-form",
          uiOutput("admin_form_content")
      ),
      footer = tagList(
        actionButton("close_admin", "Fermer", class = "btn-secondary"),
        modalButton("Annuler")
      )
    ))
  })
  
  observeEvent(input$close_admin, {
    removeModal()
  })
  
  output$adminLogged <- reactive({
    global_config$admin_logged
  })
  outputOptions(output, "adminLogged", suspendWhenHidden = FALSE)
  
  # RENDERUI OPTIMISÉ avec gestion des cas vides
  output$dynamic_product_inputs <- renderUI({
    req(global_config$admin_logged)
    
    # Capture précise de l'état avant rendu
    runjs("
      const active = document.activeElement;
      window.preserveFocusState = active && active.matches('input[type=\"text\"]') ? {
        id: active.id,
        value: active.value,
        start: active.selectionStart,
        end: active.selectionEnd
      } : null;
    ")
    
    # Gestion du cas où il n'y a pas de produits
    ids <- product_ids()
    if (length(ids) == 0) {
      product_inputs <- list(
        div(class = "alert alert-info",
            tagList(icon("info-circle"), " Aucun produit configuré pour cette page. Cliquez sur 'Ajouter un produit' pour commencer."))
      )
    } else {
      # Génération des inputs
      product_inputs <- lapply(ids, function(i) {
        # Utiliser les valeurs temporaires
        current_product <- if (!is.null(global_config$temp_products[[as.character(i)]])) {
          global_config$temp_products[[as.character(i)]]
        } else {
          ""
        }
        
        current_code <- if (!is.null(global_config$temp_codes[[as.character(i)]])) {
          global_config$temp_codes[[as.character(i)]]
        } else {
          ""
        }
        
        div(class = "product-input-group", style = "margin-bottom: 10px;",
            div(style = "display: flex; gap: 10px;",
                textInput(paste0("admin_produit", i), paste("Produit", i),
                          value = current_product,
                          placeholder = "Nom du produit"),
                textInput(paste0("admin_code_produit", i), paste("Code", i),
                          value = current_code,
                          placeholder = "Code produit")
            ),
            if (i > 1 || length(ids) > 1) {
              actionButton(paste0("remove_", i), "", icon = icon("times"),
                           class = "btn-danger btn-sm",
                           style = "margin-top: 5px;")
            }
        )
      })
    }
    
    # Restauration différée avec contrôle de cohérence
    delay(delay_time(), {
      runjs("
        if (window.preserveFocusState) {
          const elem = document.getElementById(window.preserveFocusState.id);
          if (elem && elem.value === window.preserveFocusState.value) {
            elem.setSelectionRange(window.preserveFocusState.start, window.preserveFocusState.end);
            elem.focus();
          }
        }
      ")
    })
    
    # Section complète avec header et boutons
    div(class = "products-section",
        div(class = "products-header",
            h4(tagList(icon("boxes"), "Produits et codes")),
            div(
              actionButton("save_products", "Sauvegarder les produits", 
                           icon = icon("save"), 
                           class = if (global_config$products_saved) "btn-success" else "btn-warning"),
              if (global_config$show_save_confirmation) {
                span(class = "save-confirmation show", "✓ Sauvegardé !")
              } else {
                span(class = "save-confirmation", "")
              }
            )
        ),
        product_inputs,
        div(style = "margin-top: 15px;",
            actionButton("add_product", "Ajouter un produit", 
                         icon = icon("plus"), class = "btn-primary btn-sm"),
            if (has_real_changes()) {
              span(style = "color: #ffc107; margin-left: 15px; font-style: italic;",
                   icon("exclamation-triangle"), " Modifications non sauvegardées")
            }
        )
    )
  })
  
  # Liaison avec products_changed() - VERSION CORRIGÉE
  observeEvent(products_changed(), {
    # Forcer le re-rendu du dynamic_product_inputs
    output$dynamic_product_inputs <- renderUI({
      req(global_config$admin_logged)
      
      # Capture précise de l'état avant rendu
      runjs("
        const active = document.activeElement;
        window.preserveFocusState = active && active.matches('input[type=\"text\"]') ? {
          id: active.id,
          value: active.value,
          start: active.selectionStart,
          end: active.selectionEnd
        } : null;
      ")
      
      # Gestion du cas où il n'y a pas de produits
      ids <- product_ids()
      if (length(ids) == 0) {
        product_inputs <- list(
          div(class = "alert alert-info",
              tagList(icon("info-circle"), " Aucun produit configuré pour cette page. Cliquez sur 'Ajouter un produit' pour commencer."))
        )
      } else {
        # Génération des inputs
        product_inputs <- lapply(ids, function(i) {
          # Utiliser les valeurs temporaires
          current_product <- if (!is.null(global_config$temp_products[[as.character(i)]])) {
            global_config$temp_products[[as.character(i)]]
          } else {
            ""
          }
          
          current_code <- if (!is.null(global_config$temp_codes[[as.character(i)]])) {
            global_config$temp_codes[[as.character(i)]]
          } else {
            ""
          }
          
          div(class = "product-input-group", style = "margin-bottom: 10px;",
              div(style = "display: flex; gap: 10px;",
                  textInput(paste0("admin_produit", i), paste("Produit", i),
                            value = current_product,
                            placeholder = "Nom du produit"),
                  textInput(paste0("admin_code_produit", i), paste("Code", i),
                            value = current_code,
                            placeholder = "Code produit")
              ),
              if (i > 1 || length(ids) > 1) {
                actionButton(paste0("remove_", i), "", icon = icon("times"),
                             class = "btn-danger btn-sm",
                             style = "margin-top: 5px;")
              }
          )
        })
      }
      
      # Restauration différée avec contrôle de cohérence
      delay(delay_time(), {
        runjs("
          if (window.preserveFocusState) {
            const elem = document.getElementById(window.preserveFocusState.id);
            if (elem && elem.value === window.preserveFocusState.value) {
              elem.setSelectionRange(window.preserveFocusState.start, window.preserveFocusState.end);
              elem.focus();
            }
          }
        ")
      })
      
      # Section complète avec header et boutons
      div(class = "products-section",
          div(class = "products-header",
              h4(tagList(icon("boxes"), "Produits et codes")),
              div(
                actionButton("save_products", "Sauvegarder les produits", 
                             icon = icon("save"), 
                             class = if (global_config$products_saved) "btn-success" else "btn-warning"),
                if (global_config$show_save_confirmation) {
                  span(class = "save-confirmation show", "✓ Sauvegardé !")
                } else {
                  span(class = "save-confirmation", "")
                }
              )
          ),
          product_inputs,
          div(style = "margin-top: 15px;",
              actionButton("add_product", "Ajouter un produit", 
                           icon = icon("plus"), class = "btn-primary btn-sm"),
              if (has_real_changes()) {
                span(style = "color: #ffc107; margin-left: 15px; font-style: italic;",
                     icon("exclamation-triangle"), " Modifications non sauvegardées")
              }
          )
      )
    })
  }, ignoreInit = TRUE)
  
  # AJOUT/SUPPRESSION OPTIMISÉS
  observeEvent(input$add_product, {
    new_id <- if (length(product_ids()) == 0) 1 else max(product_ids()) + 1
    product_ids(c(product_ids(), new_id))
    
    isolate({
      # Initialiser avec des valeurs vides
      global_config$temp_products[[as.character(new_id)]] <- ""
      global_config$temp_codes[[as.character(new_id)]] <- ""
      global_config$products_saved <- FALSE
    })
    
    # Focus automatique sur le nouveau champ
    delay(delay_time(), {
      runjs(sprintf("document.getElementById('admin_produit%d').focus()", new_id))
    })
  })
  
  # Gestion dynamique des observateurs de suppression
  observe({
    lapply(product_ids(), function(i) {
      if (i > 1 || length(product_ids()) > 1) {
        observeEvent(input[[paste0("remove_", i)]], {
          # Supprimer des valeurs temporaires
          global_config$temp_products[[as.character(i)]] <- NULL
          global_config$temp_codes[[as.character(i)]] <- NULL
          # Supprimer de la liste des IDs
          updated_ids <- setdiff(product_ids(), i)
          product_ids(updated_ids)
          global_config$products_saved <- FALSE
          
          # Focus management après suppression avec délai adaptatif
          delay(delay_time(), {
            runjs("
              const inputs = document.querySelectorAll('[id^=\"admin_produit\"]');
              if (inputs.length > 0) inputs[inputs.length-1].focus();
            ")
          })
        }, ignoreInit = TRUE)
      }
    })
  })
  
  # Reste du code identique...
  output$admin_form_content <- renderUI({
    req(global_config$admin_logged)
    
    etapes_choices <- c("NEAT", "BLOOM", "WET", "DRY", "DRYER")
    
    cfg <- if (global_config$page == 1) global_config$page1 else global_config$page2
    
    tagList(
      # Sélection de page
      div(class = "config-section",
          h4(tagList(icon("file-alt"), "Sélection de la page")),
          div(style = "text-align: center; margin-bottom: 20px;",
              actionButton("setup_page1", "Configurer Page 1", 
                           class = if(global_config$page == 1) "btn-primary" else "btn-outline-primary",
                           style = "margin-right: 10px;"),
              actionButton("setup_page2", "Configurer Page 2", 
                           class = if(global_config$page == 2) "btn-primary" else "btn-outline-primary")
          ),
          h5(paste("Configuration actuelle : Page", global_config$page))
      ),
      
      # Configuration des produits
      uiOutput("dynamic_product_inputs"),
      
      # Configuration de base
      div(class = "config-section",
          h4(tagList(icon("flask"), "Base de l'étude")),
          selectizeInput("admin_base", NULL,
                         choices = c("T05107", "T051M000G", "E025C004C", "Other"),
                         selected = cfg$base,
                         options = list(create = TRUE, placeholder = "Sélectionner ou saisir une base"))
      ),
      
      # Configuration des étapes
      div(class = "config-section",
          h4(tagList(icon("list-ol"), "Étapes à évaluer")),
          checkboxGroupInput("admin_etapes", NULL,
                             choices = etapes_choices,
                             selected = cfg$etapes,
                             inline = TRUE)
      ),
      
      # Configuration des supports
      div(class = "config-section",
          h4(tagList(icon("tools"), "Supports par étape")),
          uiOutput("support_inputs")
      ),
      
      # Boutons de sauvegarde GLOBALE
      div(style = "text-align: center; margin-top: 30px; padding-top: 20px; border-top: 1px solid #dee2e6;",
          actionButton("save_config", "Sauvegarder la configuration complète", 
                       class = "btn-success btn-lg",
                       icon = icon("save"))
      )
    )
  })
  
  output$support_inputs <- renderUI({
    req(input$admin_etapes)
    cfg <- if (global_config$page == 1) global_config$page1 else global_config$page2
    
    lapply(input$admin_etapes, function(etape) {
      selected_supports <- if (!is.null(cfg$supports[[etape]])) cfg$supports[[etape]] else NULL
      div(style = "margin-bottom: 15px;",
          strong(paste("Supports pour", etape, ":")),
          checkboxGroupInput(paste0("support_", etape), NULL,
                             choices = c("TS", "TC", "PS", "None"),
                             selected = selected_supports,
                             inline = TRUE)
      )
    })
  })
  
  # Modified enregistrer_config function
  enregistrer_config <- function(page_num) {
    req(input$admin_base, input$admin_etapes)
    
    # Utiliser les produits déjà sauvegardés
    cfg <- if (page_num == 1) global_config$page1 else global_config$page2
    
    supports_list <- list()
    for (etape in input$admin_etapes) {
      supports_list[[etape]] <- input[[paste0("support_", etape)]]
    }
    
    config <- list(
      produits = cfg$produits,
      codes_produits = cfg$codes_produits,
      base = input$admin_base,
      etapes = input$admin_etapes,
      supports = supports_list
    )
    
    if (page_num == 1) {
      global_config$page1 <- config
    } else {
      global_config$page2 <- config
    }
    
    saveRDS(list(
      page1 = global_config$page1,
      page2 = global_config$page2
    ), file = config_file)
  }
  
  observeEvent(input$save_config, {
    if (has_real_changes()) {
      showNotification("Veuillez d'abord sauvegarder les produits avant la configuration complète !", 
                       type = "warning", duration = 5)
      return()
    }
    
    enregistrer_config(global_config$page)
    showNotification(paste("Configuration complète de la Page", global_config$page, "enregistrée avec succès !"), 
                     type = "message", duration = 3)
  })
  
  # Fonction pour générer l'interface des questionnaires
  render_questionnaire_ui <- function(cfg) {
    req(cfg$produits, cfg$base, cfg$etapes, cfg$supports)
    
    produit_panels <- lapply(seq_along(cfg$produits), function(i) {
      produit <- cfg$produits[i]
      code_produit <- if (length(cfg$codes_produits) >= i) cfg$codes_produits[i] else ""
      
      sliders <- list()
      for (etape in cfg$etapes) {
        supports <- cfg$supports[[etape]]
        if (!is.null(supports) && length(supports) > 0) {
          for (support in supports) {
            input_id <- paste0("strength_", produit, "_", etape, "_", support)
            icone <- switch(etape,
                            "NEAT" = icon("flask"),
                            "BLOOM" = icon("fire"),
                            "WET" = icon("tint"),
                            "DRY" = icon("wind"),
                            "DRYER" = icon("wind"),
                            icon("dot-circle"))
            sliders[[input_id]] <- sliderInput(
              inputId = input_id,
              label = tagList(icone, paste(etape, "/", support)),
              min = 0, max = 4, value = 0, step = 0.1, ticks = FALSE
            )
          }
        }
      }
      wellPanel(
        h4(tagList(icon("box"), paste("Produit :", produit))),
        if (code_produit != "") p(strong("Code :", code_produit)),
        sliders
      )
    })
    
    tagList(
      div(class = "config-section",
          selectInput(paste0("panelist_name_", cfg$page_id), 
                      tagList(icon("user"), "Votre nom :"), 
                      choices = c("Veuillez sélectionner" = "", panelistes_connus), 
                      selected = ""),   
          strong(paste("Base :", cfg$base))
      ),
      div(style = "background-color: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin-bottom: 20px; border-radius: 4px;",
          tagList(icon("info-circle"), " Merci d'évaluer la force pour chaque combinaison produit / support / étape.")),
      produit_panels,
      div(style = "text-align: center; margin-top: 30px;",
          actionButton(paste0("submit_", cfg$page_id), 
                       label = tagList(icon("check"), "Soumettre les réponses"),
                       class = "btn-success btn-lg")
      )
    )
  }
  
  # Génération des interfaces de questionnaire
  output$questionnaire_ui1 <- renderUI({ 
    if (!is.null(global_config$page1$produits)) {
      render_questionnaire_ui(c(global_config$page1, list(page_id = 1)))
    } else {
      div(class = "alert alert-warning", 
          style = "margin-top: 50px;",
          tagList(icon("exclamation-triangle"), 
                  " Configuration de la page 1 non définie. Veuillez configurer depuis le panel Admin."))
    }
  })
  
  output$questionnaire_ui2 <- renderUI({ 
    if (!is.null(global_config$page2$produits)) {
      render_questionnaire_ui(c(global_config$page2, list(page_id = 2)))
    } else {
      div(class = "alert alert-warning", 
          style = "margin-top: 50px;",
          tagList(icon("exclamation-triangle"), 
                  " Configuration de la page 2 non définie. Veuillez configurer depuis le panel Admin."))
    }
  })
  
  # Observateurs pour la soumission des réponses
  observeEvent(input$submit_1, {
    enregistrer_reponses(1)
  })
  
  observeEvent(input$submit_2, {
    enregistrer_reponses(2)
  })
  
  # Fonction pour enregistrer les réponses
  enregistrer_reponses <- function(page_id) {
    cfg <- if (page_id == 1) global_config$page1 else global_config$page2
    panelist_name <- input[[paste0("panelist_name_", page_id)]]
    
    if (is.null(panelist_name) || panelist_name == "") {
      showNotification("Veuillez sélectionner votre nom avant de valider.", type = "error")
      return(NULL)
    }
    
    # Vérification de duplication
    if (file.exists(output_file)) {
      previous <- read_csv(output_file, show_col_types = FALSE)
      deja_repondu <- previous %>%
        filter(paneliste == panelist_name, base == cfg$base, produit %in% cfg$produits)
      if (nrow(deja_repondu) > 0) {
        showNotification("Vous avez déjà répondu à ce test pour au moins un des produits.", type = "warning")
        return(NULL)
      }
    }
    
    responses <- list()
    
    # Parcourir les produits avec leur index pour associer le bon code
    for (i in seq_along(cfg$produits)) {
      produit <- cfg$produits[i]
      code_produit <- if (length(cfg$codes_produits) >= i) cfg$codes_produits[i] else ""
      
      for (etape in cfg$etapes) {
        supports <- cfg$supports[[etape]]
        if (!is.null(supports)) {
          for (support in supports) {
            input_id <- paste0("strength_", produit, "_", etape, "_", support)
            val <- input[[input_id]]
            if (!is.null(val)) {
              response <- list(
                timestamp = Sys.time(),
                paneliste = panelist_name,
                code_produit = code_produit,  # Code spécifique au produit
                produit = produit,
                base = cfg$base,
                attribut = "Strength",
                etape = etape,
                support = support,
                note = val
              )
              responses[[length(responses) + 1]] <- as.data.frame(response, stringsAsFactors = FALSE)
            }
          }
        }
      }
    }
    
    if (length(responses) > 0) {
      response_df <- do.call(rbind, responses)
      if (!file.exists(output_file)) {
        write_csv(response_df, output_file)
      } else {
        write_csv(response_df, output_file, append = TRUE)
      }
      showNotification("Merci ! Vos réponses ont été enregistrées avec succès.", type = "message", duration = 5)
    }
  }
}

# Lancement de l'application
shinyApp(ui, server)
