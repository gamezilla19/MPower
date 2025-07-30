library(shiny)
library(shinyjs)
library(readr)
library(dplyr)

# Chemin du fichier de config et des réponses
config_file <- "config.rds"
output_file <- file.path(Sys.getenv("RSTUDIO_CONNECT_APP_DATA_DIR", unset = "."), "reponses_strength.csv")

# Liste des panélistes connus (sera maintenant dynamique)
panelistes_connus <- c("Alice Dupont", 
                       "Invite 1",
                       "Invite 2",
                       "Invite 3",
                       "Invite 4",
                       "Invite 5")

# UI avec styles étendus et améliorés
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
        max-width: 95%;
        width: 1400px;
      }
      .admin-form {
        max-height: 80vh;
        overflow: hidden;
        padding: 0;
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
        transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
      }
      .product-input-group:focus-within {
        border-color: #80bdff !important;
        box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25) !important;
      }
      .product-inputs-wrapper {
        min-height: 50px;
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
      
      /* STYLES AMÉLIORÉS POUR LES PANÉLISTES */
      .panelist-item {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px 15px;
        margin-bottom: 8px;
        background: #fff;
        border: 1px solid #e9ecef;
        border-radius: 6px;
        transition: all 0.2s ease;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .panelist-item:hover {
        background: #f8f9fa;
        border-color: #007bff;
        transform: translateY(-1px);
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
      .panelist-name {
        font-weight: 500;
        color: #495057;
        font-size: 14px;
      }
      .panelist-actions {
        display: flex;
        gap: 5px;
      }
      .add-panelist-section {
        background: #e8f4fd;
        border: 2px dashed #007bff;
        border-radius: 8px;
        padding: 20px;
        margin-top: 15px;
        text-align: center;
      }
      
      /* NAVIGATION PAR ONGLETS AMÉLIORÉE */
      .nav-tabs {
        border-bottom: 2px solid #dee2e6;
        margin-bottom: 0;
      }
      .nav-tabs .nav-link {
        border: 1px solid transparent;
        border-radius: 8px 8px 0 0;
        padding: 12px 20px;
        font-weight: 500;
        color: #6c757d;
        transition: all 0.2s ease;
      }
      .nav-tabs .nav-link:hover {
        border-color: #e9ecef #e9ecef #dee2e6;
        background-color: #f8f9fa;
        color: #495057;
      }
      .nav-tabs .nav-link.active {
        background-color: #007bff !important;
        color: white !important;
        border-color: #007bff #007bff #007bff !important;
      }
      
      /* CONTENU DES ONGLETS AVEC SCROLL SÉPARÉ */
      .tab-content {
        border: 1px solid #dee2e6;
        border-top: none;
        background: white;
        border-radius: 0 0 8px 8px;
        height: 70vh;
        overflow: hidden;
      }
      .tab-pane {
        height: 100%;
        overflow-y: auto;
        padding: 20px;
        display: block !important;
        opacity: 1 !important;
        visibility: visible !important;
      }
      
      .tab-pane:not(.active) {
        height: 0;
        overflow: hidden;
        opacity: 0;
        visibility: hidden;
        padding: 0;
      }
      
      /* SCROLL PERSONNALISÉ */
      .tab-pane::-webkit-scrollbar {
        width: 8px;
      }
      .tab-pane::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 4px;
      }
      .tab-pane::-webkit-scrollbar-thumb {
        background: #c1c1c1;
        border-radius: 4px;
      }
      .tab-pane::-webkit-scrollbar-thumb:hover {
        background: #a8a8a8;
      }
      
      /* SECTION PANÉLISTES COMPACTE */
      .panelists-container {
        max-height: 400px;
        overflow-y: auto;
        border: 1px solid #e9ecef;
        border-radius: 6px;
        padding: 10px;
        background: #fafafa;
      }
      
      /* RESPONSIVE */
      @media (max-width: 768px) {
        .admin-modal .modal-dialog {
          width: 95%;
          margin: 10px auto;
        }
        .tab-content {
          height: 60vh;
        }
      }
    ")),
    
    # JavaScript amélioré pour la gestion des onglets et la préservation du focus
    tags$script(HTML("
      // Enhanced focus preservation system
      let focusState = null;
      let isUpdating = false;
      
      $(document).ready(function() {
        // Gestion des onglets Bootstrap
        $(document).on('click', '[data-toggle=\"tab\"]', function(e) {
          e.preventDefault();
          var target = $(this).attr('href');
          
          // Désactiver tous les onglets
          $('.nav-link').removeClass('active');
          $('.tab-pane').removeClass('show active');
          
          // Activer l'onglet cliqué
          $(this).addClass('active');
          $(target).addClass('show active');
        });
        
        // Capture focus state before any potential UI updates
        $(document).on('focusin', 'input[type=\"text\"]', function() {
          if (!isUpdating) {
            focusState = {
              id: this.id,
              value: this.value,
              start: this.selectionStart,
              end: this.selectionEnd
            };
          }
        });
        
        // Monitor input changes to preserve cursor position
        $(document).on('input', 'input[type=\"text\"]', function() {
          if (focusState && this.id === focusState.id) {
            focusState.value = this.value;
            focusState.start = this.selectionStart;
            focusState.end = this.selectionEnd;
          }
        });
        
        // Restore focus after UI updates
        window.restoreFocus = function() {
          if (focusState && !isUpdating) {
            isUpdating = true;
            setTimeout(function() {
              const elem = document.getElementById(focusState.id);
              if (elem) {
                elem.focus();
                elem.setSelectionRange(focusState.start, focusState.end);
              }
              isUpdating = false;
            }, 50);
          }
        };
        
        // Monitor for input container changes
        const observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            if (mutation.target.id === 'product_inputs_container') {
              setTimeout(window.restoreFocus, 100);
            }
          });
        });
        
        // Start observing when admin panel is opened
        $(document).on('shown.bs.modal', '.admin-modal', function() {
          const container = document.getElementById('product_inputs_container');
          if (container) {
            observer.observe(container, { childList: true, subtree: true });
          }
          // Force binding and trigger change events
          Shiny.bindAll(document.getElementById('product_inputs_container'));
          setTimeout(() => {
            Shiny.setInputValue('force_render', Date.now());
          }, 500);

        });
        
        // Empêcher la fermeture de modal lors des confirmations
        $(document).on('click', '.btn-danger[id^=\"confirm_delete\"]', function(e) {
          e.stopPropagation();
        });
        
        // Scroll automatique vers le haut lors du changement d'onglet
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
          var target = $(e.target).attr('href');
          $(target).scrollTop(0);
        });
      });
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
    page1 = list(produits = character(0), codes_produits = character(0), base = NULL, etapes = NULL, supports = list()),
    page2 = list(produits = character(0), codes_produits = character(0), base = NULL, etapes = NULL, supports = list()),
    panelistes = panelistes_connus,
    admin_logged = FALSE,
    show_admin = FALSE,
    initialized = FALSE,
    # Valeurs temporaires pour l'édition des produits
    temp_products = list(),
    temp_codes = list(),
    # État de sauvegarde
    products_saved = TRUE,
    show_save_confirmation = FALSE,
    # Nouvel état pour contrôler les mises à jour
    ui_updating = FALSE
  )
  
  # Reactive values for dynamic product inputs
  product_ids <- reactiveVal(integer(0))
  
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
  
  # CHARGEMENT INITIAL AMÉLIORÉ - Initialize configuration on startup
  observe({
    if (!global_config$initialized) {
      if (file.exists(config_file)) {
        conf <- readRDS(config_file)
        # Forcer le chargement des deux pages avec valeurs par défaut
        global_config$page1 <- conf$page1 %||% list(produits = character(0), codes_produits = character(0), base = NULL, etapes = NULL, supports = list())
        global_config$page2 <- conf$page2 %||% list(produits = character(0), codes_produits = character(0), base = NULL, etapes = NULL, supports = list())
        
        # Charger les panélistes sauvegardés ou utiliser la liste par défaut
        global_config$panelistes <- conf$panelistes %||% panelistes_connus
      }
      
      # Initialisation explicite des IDs pour la page courante
      current_cfg <- if (global_config$page == 1) global_config$page1 else global_config$page2
      product_ids(seq_along(current_cfg$produits %||% character(0)))
      
      # Initialisation des valeurs temporaires
      load_temp_products_for_page(global_config$page)
      
      # Forcer l'état sauvegardé pour les deux pages
      global_config$products_saved <- TRUE
      global_config$initialized <- TRUE
    }
  })
  
  # GESTION DES DONNÉES TEMPORAIRES AMÉLIORÉE - Fonction pour charger les produits temporaires d'une page
  load_temp_products_for_page <- function(page_num) {
    cfg <- if (page_num == 1) global_config$page1 else global_config$page2
    
    # Initialisation robuste avec vérification de longueur
    produits <- cfg$produits %||% character(0)
    codes <- cfg$codes_produits %||% character(0)
    
    # Réinitialisation atomique
    isolate({
      if (length(produits) == 0 && length(codes) == 0) {
        product_ids(integer(0))
        global_config$temp_products <- list()
        global_config$temp_codes <- list()
      } else {
        ids <- seq_along(produits)
        product_ids(ids)
        # Remplissage initial avec noms explicites
        global_config$temp_products <- setNames(as.list(produits), ids)
        global_config$temp_codes <- setNames(as.list(codes), ids)
      }
    })
    
    # État sauvegardé si aucune modification
    global_config$products_saved <- TRUE
  }
  
  # NOUVEAU SYSTÈME DE GESTION DES INPUTS AVEC UI STABLE
  observeEvent(product_ids(), {
    # Marquer que l'UI est en cours de mise à jour
    global_config$ui_updating <- TRUE
    
    # Clear existing inputs first
    removeUI(selector = "#product_inputs_container > div", multiple = TRUE)
    
    ids <- product_ids()
    if (length(ids) == 0) {
      insertUI(
        selector = "#product_inputs_container",
        ui = div(class = "alert alert-info",
                 tagList(icon("info-circle"), 
                         " Aucun produit configuré. Cliquez sur 'Ajouter un produit'."))
      )
    } else {
      # Insert fresh inputs with preserved values
      lapply(ids, function(i) {
        current_product <- global_config$temp_products[[as.character(i)]] %||% ""
        current_code <- global_config$temp_codes[[as.character(i)]] %||% ""
        
        insertUI(
          selector = "#product_inputs_container",
          ui = div(
            class = "product-input-group", 
            id = paste0("product_group_", i),
            div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
                div(style = "flex: 1;",
                    textInput(paste0("admin_produit", i), 
                              paste("Produit", i),
                              value = current_product,
                              placeholder = "Nom du produit")
                ),
                div(style = "flex: 1;",
                    textInput(paste0("admin_code_produit", i), 
                              paste("Code", i),
                              value = current_code,
                              placeholder = "Code produit")
                ),
                if(length(ids) > 1) {
                  div(style = "flex: 0 0 auto; padding-top: 25px;",
                      actionButton(paste0("remove_", i), "", 
                                   icon = icon("times"),
                                   class = "btn-danger btn-sm")
                  )
                }
            )
          )
        )
      })
    }
    
    # Marquer la fin de la mise à jour
    delay(100, {
      global_config$ui_updating <- FALSE
    })
    
  }, ignoreNULL = FALSE, priority = 100)
  
  # Enhanced input observers with better debouncing
  observe({
    ids <- product_ids()
    lapply(ids, function(i) {
      local({
        id <- i
        
        # Product name observer with debounce
        observeEvent(input[[paste0("admin_produit", id)]], {
          if (!global_config$ui_updating) {
            new_value <- input[[paste0("admin_produit", id)]] %||% ""
            old_value <- global_config$temp_products[[as.character(id)]] %||% ""
            
            if (!identical(trimws(new_value), trimws(old_value))) {
              isolate({
                global_config$temp_products[[as.character(id)]] <- new_value
                global_config$products_saved <- FALSE
              })
            }
          }
        }, ignoreInit = TRUE)
        
        # Product code observer with debounce
        observeEvent(input[[paste0("admin_code_produit", id)]], {
          if (!global_config$ui_updating) {
            new_value <- input[[paste0("admin_code_produit", id)]] %||% ""
            old_value <- global_config$temp_codes[[as.character(id)]] %||% ""
            
            if (!identical(trimws(new_value), trimws(old_value))) {
              isolate({
                global_config$temp_codes[[as.character(id)]] <- new_value
                global_config$products_saved <- FALSE
              })
            }
          }
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # NOUVEL OBSERVATEUR POUR LES CHANGEMENTS DE PAGE
  observeEvent(global_config$page, {
    runjs("$('#config-tab').trigger('shown.bs.tab')")
    updateTabsetPanel(session, "tabs", selected = paste0("Questionnaire - Page ", global_config$page))
  })
  
  observeEvent(input$force_render, {
    load_temp_products_for_page(global_config$page)
    product_ids(seq_along(global_config[[paste0("page", global_config$page)]]$produits))
  })
  # Observer pour changer de page
  observeEvent(input$setup_page1, {
    if (has_real_changes()) {
      showNotification("Modifications non sauvegardées détectées", type = "warning")
      return()
    }
    global_config$page <- 1
    load_temp_products_for_page(1)
  })
  
  observeEvent(input$setup_page2, {
    if (has_real_changes()) {
      showNotification("Modifications non sauvegardées détectées", type = "warning")
      return()
    }
    global_config$page <- 2
    load_temp_products_for_page(2)
  })
  
  # Enhanced add product functionality
  observeEvent(input$add_product, {
    current_ids <- product_ids()
    new_id <- if(length(current_ids) == 0) 1 else max(current_ids) + 1
    
    # Update IDs first
    product_ids(c(current_ids, new_id))
    
    # Initialize temp values
    isolate({
      global_config$temp_products[[as.character(new_id)]] <- ""
      global_config$temp_codes[[as.character(new_id)]] <- ""
      global_config$products_saved <- FALSE
    })
    
    # Focus management with delay
    delay(200, {
      runjs(sprintf("
        const elem = document.getElementById('admin_produit%d');
        if (elem) {
          elem.focus();
          elem.select();
        }
      ", new_id))
    })
  })
  
  # Enhanced remove functionality
  observe({
    ids <- product_ids()
    lapply(ids, function(i) {
      observeEvent(input[[paste0("remove_", i)]], {
        # Remove from UI immediately
        removeUI(selector = paste0("#product_group_", i))
        
        # Update data structures
        isolate({
          global_config$temp_products[[as.character(i)]] <- NULL
          global_config$temp_codes[[as.character(i)]] <- NULL
          product_ids(setdiff(product_ids(), i))
          global_config$products_saved <- FALSE
        })
        
        # Focus management after removal
        delay(150, {
          remaining_ids <- product_ids()
          if (length(remaining_ids) > 0) {
            last_id <- max(remaining_ids)
            runjs(sprintf("
              const elem = document.getElementById('admin_produit%d');
              if (elem) elem.focus();
            ", last_id))
          }
        })
      }, ignoreInit = TRUE)
    })
  })
  
  # SAUVEGARDE MANUELLE DES PRODUITS AVEC SYNCHRONISATION RENFORCÉE
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
    
    # MISE À JOUR EXPLICITE DES DEUX PAGES
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
        page2 = global_config$page2,
        panelistes = global_config$panelistes
      ), file = config_file)
    })
    
    # RAFRAÎCHISSEMENT FORCÉ
    load_temp_products_for_page(global_config$page)
    
    # Marquer comme sauvegardé
    global_config$products_saved <- TRUE
    
    # Feedback visuel
    global_config$show_save_confirmation <- TRUE
    showNotification(paste("Produits de la Page", global_config$page, "sauvegardés avec succès !"), 
                     type = "message", duration = 3)
    
    delay(3000, {
      global_config$show_save_confirmation <- FALSE
    })
  })
  
  # ========== GESTION DES PANÉLISTES ==========
  
  # Ajouter un panéliste
  observeEvent(input$add_panelist_btn, {
    new_name <- trimws(input$new_panelist_name)
    if (new_name == "") {
      showNotification("Veuillez saisir un nom de panéliste.", type = "warning")
      return()
    }
    
    if (new_name %in% global_config$panelistes) {
      showNotification("Ce panéliste existe déjà.", type = "warning")
      return()
    }
    
    # Ajouter le panéliste
    global_config$panelistes <- c(global_config$panelistes, new_name)
    
    # Sauvegarder immédiatement
    saveRDS(list(
      page1 = global_config$page1,
      page2 = global_config$page2,
      panelistes = global_config$panelistes
    ), file = config_file)
    
    # Réinitialiser le champ
    updateTextInput(session, "new_panelist_name", value = "")
    
    showNotification(paste("Panéliste", new_name, "ajouté avec succès !"), type = "message")
  })
  
  # Supprimer un panéliste (observateur dynamique)
  observe({
    lapply(global_config$panelistes, function(panelist) {
      btn_id <- paste0("remove_panelist_", gsub("[^A-Za-z0-9]", "_", panelist))
      observeEvent(input[[btn_id]], {
        # Stocker le panéliste à supprimer
        global_config$panelist_to_delete <- panelist
        
        # Modal de confirmation
        showModal(modalDialog(
          title = tagList(icon("exclamation-triangle", style = "color: #dc3545;"), "Confirmer la suppression"),
          div(
            style = "text-align: center; padding: 20px;",
            h5(paste("Êtes-vous sûr de vouloir supprimer le panéliste :")),
            h4(style = "color: #dc3545; font-weight: bold;", panelist),
            br(),
            p("Cette action est irréversible.")
          ),
          footer = tagList(
            actionButton("cancel_delete", "Annuler", class = "btn-secondary"),
            actionButton("confirm_delete_panelist", "Supprimer", class = "btn-danger")
          ),
          size = "m",
          easyClose = FALSE
        ))
      }, ignoreInit = TRUE)
    })
  })
  
  # Annuler la suppression
  observeEvent(input$cancel_delete, {
    global_config$panelist_to_delete <- NULL
    removeModal()
  })
  
  # Confirmer la suppression
  observeEvent(input$confirm_delete_panelist, {
    panelist_to_remove <- global_config$panelist_to_delete
    
    if (!is.null(panelist_to_remove)) {
      # Supprimer le panéliste
      global_config$panelistes <- setdiff(global_config$panelistes, panelist_to_remove)
      
      # Sauvegarder
      saveRDS(list(
        page1 = global_config$page1,
        page2 = global_config$page2,
        panelistes = global_config$panelistes
      ), file = config_file)
      
      showNotification(paste("Panéliste", panelist_to_remove, "supprimé avec succès."), type = "message")
      global_config$panelist_to_delete <- NULL
    }
    
    removeModal()
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
  
  # MÉCANISME DE RAFRAÎCHISSEMENT DE L'UI - Modal du panel admin avec onglets AMÉLIORÉE
  observeEvent(input$show_admin_panel, {
    # Forcer le rechargement des produits
    load_temp_products_for_page(global_config$page)
    
    showModal(modalDialog(
      title = tagList(icon("cogs"), "Configuration de l'étude"),
      size = "l",
      class = "admin-modal",
      div(class = "admin-form",
          # Navigation par onglets avec JavaScript
          tags$ul(class = "nav nav-tabs", role = "tablist",
                  tags$li(class = "nav-item",
                          tags$a(class = "nav-link active", 
                                 `data-toggle` = "tab", 
                                 href = "#config-tab", 
                                 role = "tab",
                                 tagList(icon("cogs"), "Configuration"))),
                  tags$li(class = "nav-item",
                          tags$a(class = "nav-link", 
                                 `data-toggle` = "tab", 
                                 href = "#panelists-tab", 
                                 role = "tab",
                                 tagList(icon("users"), "Panélistes")))
          ),
          
          # Contenu des onglets avec scroll séparé
          div(class = "tab-content",
              # Onglet Configuration
              div(class = "tab-pane fade show active", 
                  id = "config-tab", 
                  role = "tabpanel",
                  uiOutput("admin_config_content")
              ),
              
              # Onglet Panélistes
              div(class = "tab-pane fade", 
                  id = "panelists-tab", 
                  role = "tabpanel",
                  uiOutput("admin_panelists_content")
              )
          )
      ),
      footer = tagList(
        actionButton("close_admin", "Fermer", class = "btn-secondary")
      )
    ))
    
    # FORCER L'ACTIVATION DE L'ONGLET CONFIGURATION
    runjs("
      setTimeout(function() {
        // Activer le premier onglet et son contenu
        $('#config-tab').addClass('show active');
        $('a[href=\"#config-tab\"]').addClass('active');
        
        // S'assurer que l'onglet panélistes est inactif
        $('#panelists-tab').removeClass('show active');
        $('a[href=\"#panelists-tab\"]').removeClass('active');
        
        // Forcer le rendu initial
        if (typeof Shiny !== 'undefined' && Shiny.bindAll) {
          Shiny.bindAll(document.getElementById('config-tab'));
        }
      }, 150);
    ")
  }, priority = 1000)
  
  observeEvent(input$close_admin, {
    removeModal()
  })
  
  output$adminLogged <- reactive({
    global_config$admin_logged
  })
  outputOptions(output, "adminLogged", suspendWhenHidden = FALSE)
  
  # ========== CONTENU DE L'ONGLET PANÉLISTES AMÉLIORÉ ==========
  output$admin_panelists_content <- renderUI({
    req(global_config$admin_logged)
    
    panelist_items <- lapply(global_config$panelistes, function(panelist) {
      btn_id <- paste0("remove_panelist_", gsub("[^A-Za-z0-9]", "_", panelist))
      
      div(class = "panelist-item",
          span(class = "panelist-name", panelist),
          div(class = "panelist-actions",
              actionButton(btn_id, "", 
                           icon = icon("trash"), 
                           class = "btn-danger btn-sm",
                           title = "Supprimer ce panéliste",
                           onclick = "event.stopPropagation();")
          )
      )
    })
    
    tagList(
      # Section d'ajout EN HAUT
      div(class = "add-panelist-section",
          h4(tagList(icon("user-plus"), "Ajouter un nouveau panéliste")),
          fluidRow(
            column(8,
                   textInput("new_panelist_name", 
                             label = NULL,
                             placeholder = "Nom du nouveau panéliste",
                             value = "")
            ),
            column(4,
                   actionButton("add_panelist_btn", 
                                "Ajouter", 
                                icon = icon("plus"),
                                class = "btn-success",
                                style = "width: 100%;")
            )
          ),
          div(class = "alert alert-info",
              style = "margin-top: 15px;",
              tagList(icon("lightbulb"), 
                      " Les panélistes ajoutés seront immédiatement disponibles dans les questionnaires."))
      ),
      
      # Liste des panélistes existants
      div(class = "config-section",
          h4(tagList(icon("users"), "Panélistes actuels (", length(global_config$panelistes), ")")),
          
          if (length(global_config$panelistes) > 0) {
            div(class = "panelists-container",
                panelist_items
            )
          } else {
            div(class = "alert alert-info",
                tagList(icon("info-circle"), " Aucun panéliste configuré."))
          }
      )
    )
  })
  
  # ========== CONTENU DE L'ONGLET CONFIGURATION AVEC UI STABLE ==========
  output$admin_config_content <- renderUI({
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
      
      # Configuration des produits - CONTENEUR STABLE
      div(class = "products-section",
          div(class = "products-header",
              h4(tagList(icon("boxes"), "Produits et codes")),
              div(
                actionButton("save_products", "Sauvegarder les produits", 
                             icon = icon("save"), 
                             class = "btn-warning"),
                uiOutput("save_confirmation_ui")
              )
          ),
          # CONTENEUR STATIQUE - Ne sera jamais re-rendu
          div(id = "product_inputs_container",
              class = "product-inputs-wrapper"
          ),
          div(style = "margin-top: 15px;",
              actionButton("add_product", "Ajouter un produit", 
                           icon = icon("plus"), class = "btn-primary btn-sm"),
              uiOutput("unsaved_changes_indicator")
          )
      ),
      
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
  
  # Enhanced UI feedback components
  output$save_confirmation_ui <- renderUI({
    if (global_config$show_save_confirmation) {
      span(class = "save-confirmation show", 
           style = "color: #28a745; font-weight: bold; margin-left: 10px;",
           "✓ Sauvegardé !")
    }
  })
  
  output$unsaved_changes_indicator <- renderUI({
    if (has_real_changes()) {
      span(style = "color: #ffc107; margin-left: 15px; font-style: italic;",
           icon("exclamation-triangle"), " Modifications non sauvegardées")
    }
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
  
  # SYNCHRONISATION RENFORCÉE - Fonction pour enregistrer la configuration
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
    
    # Mise à jour explicite des deux pages
    if (page_num == 1) {
      global_config$page1 <- config
    } else {
      global_config$page2 <- config
    }
    
    saveRDS(list(
      page1 = global_config$page1,
      page2 = global_config$page2,
      panelistes = global_config$panelistes
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
  
  # GESTION DES ÉTATS VIDES - Fonction pour générer l'interface des questionnaires
  render_questionnaire_ui <- function(cfg) {
    # Gestion des états vides
    if (length(cfg$produits) == 0) {
      return(
        div(class = "alert alert-info",
            tagList(icon("info-circle"), 
                    " Aucun produit configuré - Veuillez ajouter des produits via le panel Admin"))
      )
    }
    
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
                      choices = c("Veuillez sélectionner" = "", global_config$panelistes), 
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
    if (!is.null(global_config$page1$produits) && length(global_config$page1$produits) > 0) {
      render_questionnaire_ui(c(global_config$page1, list(page_id = 1)))
    } else {
      div(class = "alert alert-warning", 
          style = "margin-top: 50px;",
          tagList(icon("exclamation-triangle"), 
                  " Configuration de la page 1 non définie. Veuillez configurer depuis le panel Admin."))
    }
  })
  
  output$questionnaire_ui2 <- renderUI({ 
    if (!is.null(global_config$page2$produits) && length(global_config$page2$produits) > 0) {
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
