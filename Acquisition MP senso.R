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
      
      /* STYLES AMÉLIORÉS POUR LA GESTION DES FICHES (jusqu'à 10) */
      .sheet-management {
        background: #fff3cd;
        border: 2px solid #ffc107;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
      }
      
      .sheet-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: 15px;
        margin-bottom: 20px;
      }
      
      .sheet-status {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 12px 15px;
        background: white;
        border-radius: 8px;
        border: 2px solid #dee2e6;
        transition: all 0.2s ease;
      }
      
      .sheet-status:hover {
        border-color: #007bff;
        box-shadow: 0 2px 8px rgba(0,123,255,0.15);
      }
      
      .sheet-indicator {
        display: flex;
        align-items: center;
        gap: 10px;
        flex: 1;
      }
      
      .sheet-badge {
        padding: 4px 10px;
        border-radius: 12px;
        font-weight: bold;
        font-size: 11px;
        text-transform: uppercase;
      }
      
      .sheet-active {
        background: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      
      .sheet-inactive {
        background: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
      
      .sheet-actions {
        display: flex;
        gap: 8px;
      }
      
      .sheet-preview {
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin-top: 15px;
        max-height: 300px;
        overflow-y: auto;
      }
      
      .preview-item {
        display: flex;
        justify-content: space-between;
        padding: 8px 0;
        border-bottom: 1px dashed #dee2e6;
        font-size: 13px;
      }
      
      .preview-item:last-child {
        border-bottom: none;
      }
      
      /* STYLES POUR LES BOUTONS DE PAGE (jusqu'à 10) */
      .page-buttons-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
        gap: 10px;
        margin-bottom: 20px;
      }
      
      .page-button {
        padding: 10px 15px !important;
        font-size: 13px !important;
        border-radius: 6px !important;
        transition: all 0.2s ease !important;
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
      
      /* CONTENU DES ONGLETS ENTIÈREMENT ADAPTATIF */
      .tab-content {
        border: 1px solid #dee2e6;
        border-top: none;
        background: white;
        border-radius: 0 0 8px 8px;
        overflow: hidden;
      }
      .tab-pane {
        overflow-y: auto;
        padding: 20px;
        display: block !important;
        opacity: 1 !important;
        visibility: visible !important;
      }
      
      /* LIMITATION DE HAUTEUR SEULEMENT POUR LE MODAL ADMIN */
      .admin-modal .tab-pane {
        max-height: 70vh;
      }


      /* STYLES POUR L'ÉDITION DES NOMS DE PAGES */
      .page-name-editor {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 10px;
      }
      
      .page-name-input {
        flex: 1;
        padding: 6px 10px;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        font-size: 13px;
      }
      
      .page-name-input:focus {
        border-color: #007bff;
        box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25);
        outline: none;
      }
      
      .edit-name-btn, .save-name-btn, .cancel-name-btn {
        padding: 4px 8px;
        font-size: 11px;
        border-radius: 3px;
        border: none;
        cursor: pointer;
      }
      
      .edit-name-btn {
        background: #6c757d;
        color: white;
      }
      
      .save-name-btn {
        background: #28a745;
        color: white;
      }
      
      .cancel-name-btn {
        background: #dc3545;
        color: white;
      }
      
      .sheet-name-display {
        font-weight: 600;
        color: #495057;
      }
      
      .tab-pane:not(.active) {
        height: 0;
        overflow: hidden;
        opacity: 0;
        visibility: hidden;
        padding: 0;
      }
      
      .page-name-input-wrapper {
        flex: 1;
      }
      
      .page-name-input-wrapper input {
        width: 100%;
        padding: 6px 10px;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        font-size: 13px;
      }
      
      .page-name-input-wrapper input:focus {
        border-color: #007bff;
        box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25);
        outline: none;
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
      
      /* STYLES POUR LES COMMENTAIRES */
      .comment-box {
        margin-top: 15px;
        padding: 15px;
        background: #f8f9fa;
        border-radius: 6px;
        border: 1px solid #dee2e6;
      }
      
      .comment-box textarea {
        width: 100%;
        resize: vertical;
        min-height: 80px;
      }
      
      .step-section {
        margin-bottom: 25px;
        border-bottom: 1px dashed #ddd;
        padding-bottom: 15px;
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
        .sheet-grid {
          grid-template-columns: 1fr;
        }
        .page-buttons-grid {
          grid-template-columns: repeat(2, 1fr);
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
  
  # Contenu principal - ONGLETS DYNAMIQUES
  div(class = "main-content",
      titlePanel("Evaluation de la puissance MP"),
      uiOutput("dynamic_tabs")
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Null coalescing operator
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # CONFIGURATION ÉTENDUE POUR 10 PAGES MAXIMUM
  init_page_config <- function() {
    list(
      produits = character(0), 
      codes_produits = character(0), 
      base = NULL, 
      etapes = NULL, 
      supports = list(),
      # NOUVEAU : nom personnalisé de la page
      page_name = NULL
    )
  }
  
  # Reactive values pour stocker la configuration de l'etude (ÉTENDU À 10 PAGES)
  global_config <- reactiveValues(
    page = 1,
    page1 = init_page_config(),
    page2 = init_page_config(),
    page3 = init_page_config(),
    page4 = init_page_config(),
    page5 = init_page_config(),
    page6 = init_page_config(),
    page7 = init_page_config(),
    page8 = init_page_config(),
    page9 = init_page_config(),
    page10 = init_page_config(),
    panelistes = panelistes_connus,
    admin_logged = FALSE,
    show_admin = FALSE,
    initialized = FALSE,
    # NOUVEAU : Gestion des fiches actives (par défaut 1 et 2)
    active_sheets = c(1, 2),
    # Valeurs temporaires pour l'édition des produits
    page_names = c(
      "Page 1", "Page 2", "Page 3", "Page 4", "Page 5",
      "Page 6", "Page 7", "Page 8", "Page 9", "Page 10"
    ),
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
  
  # ========== FONCTIONS AMÉLIORÉES POUR LA GESTION DE 10 FICHES ==========
  
  # Fonction pour ajouter une fiche (étendue pour 10 pages)
  add_sheet <- function(sheet_num) {
    if (sheet_num < 1 || sheet_num > 10) {
      showNotification("Numéro de fiche invalide (1-10 autorisés)", type = "error")
      return()
    }
    
    if (!sheet_num %in% global_config$active_sheets) {
      global_config$active_sheets <- sort(c(global_config$active_sheets, sheet_num))
      
      # Initialiser la nouvelle fiche si elle n'existe pas
      if (is.null(global_config[[paste0("page", sheet_num)]])) {
        global_config[[paste0("page", sheet_num)]] <- init_page_config()
      }
      
      # Sauvegarder immédiatement
      save_global_config()
      
      showNotification(paste("Fiche", sheet_num, "ajoutée avec succès !"), type = "message")
    }
  }
  
  # Fonction pour retirer une fiche (étendue pour 10 pages)
  remove_sheet <- function(sheet_num) {
    if (length(global_config$active_sheets) <= 1) {
      showNotification("Impossible de supprimer la dernière fiche active !", type = "error")
      return()
    }
    
    if (sheet_num %in% global_config$active_sheets) {
      global_config$active_sheets <- setdiff(global_config$active_sheets, sheet_num)
      
      # Si on supprime la page courante, basculer vers la première disponible
      if (global_config$page == sheet_num) {
        global_config$page <- min(global_config$active_sheets)
        load_temp_products_for_page(global_config$page)
      }
      
      # Sauvegarder immédiatement
      save_global_config()
      
      showNotification(paste("Fiche", sheet_num, "retirée avec succès !"), type = "message")
    }
  }
  
  # Fonction pour sauvegarder la configuration globale (étendue pour 10 pages)
  save_global_config <- function() {
    config_to_save <- list(
      panelistes = global_config$panelistes,
      active_sheets = global_config$active_sheets,
      page_names = global_config$page_names
    )
    
    # Sauvegarder toutes les pages (1 à 10)
    for (i in 1:10) {
      config_to_save[[paste0("page", i)]] <- global_config[[paste0("page", i)]]
    }
    
    saveRDS(config_to_save, file = config_file)
  }
  
  # ========== GÉNÉRATION DYNAMIQUE DES ONGLETS ==========
  
  output$dynamic_tabs <- renderUI({
    active_sheets <- global_config$active_sheets
    
    if (length(active_sheets) == 0) {
      return(div(class = "alert alert-warning",
                 "Aucune fiche active. Veuillez configurer les fiches depuis le panel Admin."))
    }
    
    # Créer les onglets dynamiquement avec noms personnalisés
    tab_panels <- lapply(active_sheets, function(sheet_num) {
      page_name <- global_config$page_names[sheet_num]
      tabPanel(
        page_name,  # Utiliser le nom personnalisé
        icon = icon("clipboard-list"),
        br(),
        uiOutput(paste0("questionnaire_ui", sheet_num))
      )
    })
    
    do.call(tabsetPanel, c(list(id = "tabs"), tab_panels))
  })
  
  # ========== FONCTIONS AMÉLIORÉES ==========
  
  # Fonction de vérification de changements réels
  has_real_changes <- function() {
    current_page_config <- global_config[[paste0("page", global_config$page)]]
    
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
  
  # CHARGEMENT INITIAL AMÉLIORÉ (étendu pour 10 pages)
  observe({
    if (!global_config$initialized) {
      if (file.exists(config_file)) {
        conf <- readRDS(config_file)
        
        # Charger toutes les pages (1 à 10)
        for (i in 1:10) {
          page_key <- paste0("page", i)
          global_config[[page_key]] <- conf[[page_key]] %||% init_page_config()
        }
        
        # Charger les panélistes sauvegardés ou utiliser la liste par défaut
        global_config$panelistes <- conf$panelistes %||% panelistes_connus
        
        # Charger les fiches actives ou utiliser les valeurs par défaut
        global_config$active_sheets <- conf$active_sheets %||% c(1, 2)
        
        global_config$page_names <- conf$page_names %||% c(
          "Page 1", "Page 2", "Page 3", "Page 4", "Page 5",
          "Page 6", "Page 7", "Page 8", "Page 9", "Page 10"
        )
      }
      
      # Initialisation explicite des IDs pour la page courante
      current_cfg <- global_config[[paste0("page", global_config$page)]]
      product_ids(seq_along(current_cfg$produits %||% character(0)))
      
      # Initialisation des valeurs temporaires
      load_temp_products_for_page(global_config$page)
      
      # Forcer l'état sauvegardé pour toutes les pages
      global_config$products_saved <- TRUE
      global_config$initialized <- TRUE
    }
  })
  
  # GESTION DES DONNÉES TEMPORAIRES AMÉLIORÉE
  load_temp_products_for_page <- function(page_num) {
    cfg <- global_config[[paste0("page", page_num)]]
    
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
  
  # OBSERVATEURS DYNAMIQUES POUR LES 10 PAGES
  observe({
    # Créer dynamiquement les observateurs pour chaque page (1 à 10)
    for (page_num in 1:10) {
      local({
        current_page <- page_num
        
        # Observateur pour changer de page
        observeEvent(input[[paste0("setup_page", current_page)]], {
          if (has_real_changes()) {
            showNotification("Modifications non sauvegardées détectées", type = "warning")
            return()
          }
          global_config$page <- current_page
          load_temp_products_for_page(current_page)
        })
        
        # Observateur pour ajouter une fiche
        observeEvent(input[[paste0("add_sheet_", current_page)]], {
          add_sheet(current_page)
        })
        
        # Observateur pour supprimer une fiche
        observeEvent(input[[paste0("remove_sheet_", current_page)]], {
          remove_sheet(current_page)
        })
      })
    }
  })
  
  observeEvent(input$force_render, {
    load_temp_products_for_page(global_config$page)
    current_cfg <- global_config[[paste0("page", global_config$page)]]
    product_ids(seq_along(current_cfg$produits))
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
    save_global_config()
    
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
      save_global_config()
      
      showNotification(paste("Panéliste", panelist_to_remove, "supprimé avec succès."), type = "message")
      global_config$panelist_to_delete <- NULL
    }
    
    removeModal()
  })
  
  # ========== GESTION DE L'ÉDITION DES NOMS DE PAGES ==========
  
  # Observateurs pour l'édition des noms (pour toutes les pages)
  observe({
    for (sheet_num in 1:10) {
      local({
        current_sheet <- sheet_num
        
        # Observateur pour commencer l'édition
        observeEvent(input[[paste0("edit_name_", current_sheet)]], {
          current_name <- global_config$page_names[current_sheet]
          
          # Remplacer l'affichage par un champ d'édition
          removeUI(selector = paste0("#name_display_", current_sheet))
          
          insertUI(
            selector = paste0("#edit_name_", current_sheet),
            where = "beforeBegin",
            ui = tagList(
              div(class = "page-name-input-wrapper",
                  textInput(paste0("name_input_", current_sheet), 
                            label = NULL,
                            value = current_name,
                            placeholder = "Nom de la page")),
              actionButton(paste0("save_name_", current_sheet), "", 
                           icon = icon("check"), 
                           class = "save-name-btn",
                           title = "Sauvegarder"),
              actionButton(paste0("cancel_name_", current_sheet), "", 
                           icon = icon("times"), 
                           class = "cancel-name-btn",
                           title = "Annuler")
            )
          )
          
          # Cacher le bouton d'édition
          shinyjs::hide(paste0("edit_name_", current_sheet))
          
          # Focus sur le champ
          runjs(paste0("document.getElementById('name_input_", current_sheet, "').focus();"))
        })
        
        # Observateur pour sauvegarder le nom
        observeEvent(input[[paste0("save_name_", current_sheet)]], {
          new_name <- trimws(input[[paste0("name_input_", current_sheet)]])
          
          if (new_name == "") {
            showNotification("Le nom ne peut pas être vide !", type = "warning")
            return()
          }
          
          if (nchar(new_name) > 50) {
            showNotification("Le nom ne peut pas dépasser 50 caractères !", type = "warning")
            return()
          }
          
          # Mettre à jour le nom
          global_config$page_names[current_sheet] <- new_name
          
          # Sauvegarder
          save_global_config()
          
          # Restaurer l'affichage normal
          restore_name_display(current_sheet, new_name)
          
          showNotification(paste("Nom de la fiche", current_sheet, "mis à jour !"), type = "message")
        })
        
        # Observateur pour annuler l'édition
        observeEvent(input[[paste0("cancel_name_", current_sheet)]], {
          current_name <- global_config$page_names[current_sheet]
          restore_name_display(current_sheet, current_name)
        })
      })
    }
  })
  
  
  # Fonction helper pour restaurer l'affichage normal
  restore_name_display <- function(sheet_num, name) {
    # Supprimer les éléments d'édition
    removeUI(selector = paste0("#name_input_", sheet_num))
    removeUI(selector = paste0("#save_name_", sheet_num))
    removeUI(selector = paste0("#cancel_name_", sheet_num))
    
    # Restaurer l'affichage du nom
    insertUI(
      selector = paste0("#edit_name_", sheet_num),
      where = "beforeBegin",
      ui = span(class = "sheet-name-display", id = paste0("name_display_", sheet_num), name)
    )
    
    # Réafficher le bouton d'édition
    shinyjs::show(paste0("edit_name_", sheet_num))
  }
  
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
                                 href = "#sheets-tab", 
                                 role = "tab",
                                 tagList(icon("file-alt"), "Gestion des fiches"))),
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
              
              # ONGLET : Gestion des fiches (ÉTENDU À 10)
              div(class = "tab-pane fade", 
                  id = "sheets-tab", 
                  role = "tabpanel",
                  uiOutput("admin_sheets_content")
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
        
        // S'assurer que les autres onglets sont inactifs
        $('#sheets-tab, #panelists-tab').removeClass('show active');
        $('a[href=\"#sheets-tab\"], a[href=\"#panelists-tab\"]').removeClass('active');
        
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
  
  # ========== ONGLET : GESTION DES FICHES AVEC NOMS PERSONNALISÉS ==========
  output$admin_sheets_content <- renderUI({
    req(global_config$admin_logged)
    
    active_sheets <- global_config$active_sheets
    
    # Créer les éléments pour chaque fiche possible (1 à 10)
    sheet_elements <- lapply(1:10, function(sheet_num) {
      is_active <- sheet_num %in% active_sheets
      cfg <- global_config[[paste0("page", sheet_num)]]
      page_name <- global_config$page_names[sheet_num]
      
      # Compter les produits configurés
      nb_products <- length(cfg$produits %||% character(0))
      
      div(class = "sheet-status",
          div(class = "sheet-indicator", style = "flex-direction: column; align-items: flex-start;",
              # Affichage/édition du nom
              div(class = "page-name-editor",
                  if(is_active) {
                    tagList(
                      span(class = "sheet-name-display", id = paste0("name_display_", sheet_num), page_name),
                      actionButton(paste0("edit_name_", sheet_num), "", 
                                   icon = icon("edit"), 
                                   class = "edit-name-btn",
                                   title = "Modifier le nom")
                    )
                  } else {
                    span(class = "sheet-name-display", page_name)
                  }
              ),
              # Indicateurs de statut
              div(style = "display: flex; align-items: center; gap: 10px; margin-top: 5px;",
                  span(class = paste("sheet-badge", if(is_active) "sheet-active" else "sheet-inactive"),
                       if(is_active) "ACTIVE" else "INACTIVE"),
                  if(is_active && nb_products > 0) {
                    span(style = "color: #6c757d; font-size: 11px;",
                         paste("(", nb_products, "produit(s))"))
                  }
              )
          ),
          div(class = "sheet-actions",
              if(!is_active) {
                actionButton(paste0("add_sheet_", sheet_num), 
                             "Activer", 
                             icon = icon("plus"),
                             class = "btn-success btn-sm")
              } else {
                actionButton(paste0("remove_sheet_", sheet_num), 
                             "Désactiver", 
                             icon = icon("minus"),
                             class = "btn-danger btn-sm",
                             disabled = length(active_sheets) <= 1)
              }
          )
      )
    })
    
    # Aperçu des fiches actives avec noms personnalisés
    preview_elements <- if(length(active_sheets) > 0) {
      lapply(active_sheets, function(sheet_num) {
        cfg <- global_config[[paste0("page", sheet_num)]]
        products <- cfg$produits %||% character(0)
        page_name <- global_config$page_names[sheet_num]
        
        div(class = "preview-item",
            strong(paste(page_name, ":")),
            span(if(length(products) > 0) {
              if(length(products) <= 3) {
                paste(products, collapse = ", ")
              } else {
                paste(paste(products[1:3], collapse = ", "), "... (", length(products), "total)")
              }
            } else {
              "Non configurée"
            })
        )
      })
    } else {
      list(div(class = "alert alert-info", "Aucune fiche active"))
    }
    
    tagList(
      div(class = "sheet-management",
          h4(tagList(icon("file-alt"), "Gestion des fiches de test (1-10)")),
          p("Activez ou désactivez les fiches de test selon vos besoins. Cliquez sur l'icône d'édition pour personnaliser le nom d'une fiche active."),
          
          # Grille des fiches avec layout amélioré
          div(class = "sheet-grid", sheet_elements),
          
          div(class = "sheet-preview",
              h5(tagList(icon("eye"), paste("Aperçu des fiches actives (", length(active_sheets), "/10)"))),
              preview_elements
          ),
          
          div(class = "alert alert-info", style = "margin-top: 20px;",
              tagList(
                icon("info-circle"),
                " ",
                strong("Note :"), 
                " Les fiches désactivées conservent leur configuration mais ne sont pas visibles aux panélistes. Vous pouvez personnaliser le nom des fiches actives."
              )
          )
      )
    )
  })
  
  
  # ========== CONTENU DE L'ONGLET PANÉLISTES ==========
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
                      " Les panélistes ajoutés seront immédiatement disponibles dans tous les questionnaires."))
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
  
  # ========== CONTENU DE L'ONGLET CONFIGURATION (ÉTENDU À 10 PAGES) ==========
  output$admin_config_content <- renderUI({
    req(global_config$admin_logged)
    
    etapes_choices <- c("NEAT", "BLOOM", "WET", "DRY", "DRYER")
    cfg <- global_config[[paste0("page", global_config$page)]]
    active_sheets <- global_config$active_sheets
    
    # Boutons pour changer de page (seulement les fiches actives)
    page_buttons <- lapply(active_sheets, function(sheet_num) {
      page_name <- global_config$page_names[sheet_num]
      actionButton(paste0("setup_page", sheet_num), 
                   page_name,  # Utiliser le nom personnalisé
                   class = paste("page-button", if(global_config$page == sheet_num) "btn-primary" else "btn-outline-primary"))
    })
    
    tagList(
      # Sélection de page (seulement les fiches actives)
      div(class = "config-section",
          h4(tagList(icon("file-alt"), "Sélection de la page à configurer")),
          div(class = "page-buttons-grid", page_buttons),
          div(class = "alert alert-info",
              tagList(icon("info-circle"), 
                      paste("Configuration actuelle : Page", global_config$page, 
                            if(length(active_sheets) > 1) paste(" (", length(active_sheets), "pages actives)" ) else "")))
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
    cfg <- global_config[[paste0("page", global_config$page)]]
    
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
    
    # MISE À JOUR EXPLICITE DE LA PAGE COURANTE
    isolate({
      current_page_key <- paste0("page", global_config$page)
      global_config[[current_page_key]]$produits <- produits_valides
      global_config[[current_page_key]]$codes_produits <- codes_valides
      
      # Sauvegarder la configuration complète
      save_global_config()
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
  
  # SYNCHRONISATION RENFORCÉE - Fonction pour enregistrer la configuration
  enregistrer_config <- function(page_num) {
    req(input$admin_base, input$admin_etapes)
    
    # Utiliser les produits déjà sauvegardés
    cfg <- global_config[[paste0("page", page_num)]]
    
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
    
    # Mise à jour explicite de la page courante
    global_config[[paste0("page", page_num)]] <- config
    
    save_global_config()
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
  
  # GESTION DES ÉTATS VIDES - Fonction pour générer l'interface des questionnaires AVEC COMMENTAIRES
  render_questionnaire_ui <- function(cfg, page_id) {
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
      
      etape_blocks <- list()
      
      for (etape in cfg$etapes) {
        supports <- cfg$supports[[etape]]
        if (!is.null(supports) && length(supports) > 0) {
          # Sliders pour les supports
          sliders <- lapply(supports, function(support) {
            input_id <- paste0("strength_", page_id, "_", produit, "_", etape, "_", support)
            icone <- switch(etape,
                            "NEAT" = icon("flask"),
                            "BLOOM" = icon("fire"),
                            "WET" = icon("tint"),
                            "DRY" = icon("wind"),
                            "DRYER" = icon("wind"),
                            icon("dot-circle"))
            sliderInput(
              inputId = input_id,
              label = tagList(icone, paste(etape, "/", support)),
              min = 0, max = 4, value = 0, step = 0.1, ticks = FALSE
            )
          })
          
          # Commentaire pour l'étape
          comment_id <- paste0("comment_", page_id, "_", i, "_", etape)
          comment_input <- textAreaInput(
            comment_id,
            label = paste("Commentaire pour", etape),
            rows = 2,
            placeholder = "Saisissez votre observation ici..."
          )
          
          etape_blocks[[etape]] <- tagList(
            div(class = "step-section",
                sliders,
                div(class = "comment-box",
                    comment_input
                )
            )
          )
        }
      }
      
      wellPanel(
        h4(tagList(icon("box"), paste("Produit :", produit))),
        if (code_produit != "") p(strong("Code :", code_produit)),
        etape_blocks
      )
    })
    
    tagList(
      div(class = "config-section",
          selectInput(paste0("panelist_name_", page_id), 
                      tagList(icon("user"), "Votre nom :"), 
                      choices = c("Veuillez sélectionner" = "", global_config$panelistes), 
                      selected = ""),   
          strong(paste("Base :", cfg$base))
      ),
      div(style = "background-color: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin-bottom: 20px; border-radius: 4px;",
          tagList(icon("info-circle"), " Merci d'évaluer la force pour chaque combinaison produit / support / étape et d'ajouter vos commentaires.")),
      produit_panels,
      div(style = "text-align: center; margin-top: 30px;",
          actionButton(paste0("submit_", page_id), 
                       label = tagList(icon("check"), "Soumettre les réponses"),
                       class = "btn-success btn-lg")
      )
    )
  }
  
  # Génération DYNAMIQUE des interfaces de questionnaire pour toutes les fiches actives (1-10)
  observe({
    active_sheets <- global_config$active_sheets
    
    lapply(active_sheets, function(sheet_num) {
      output[[paste0("questionnaire_ui", sheet_num)]] <- renderUI({ 
        cfg <- global_config[[paste0("page", sheet_num)]]
        if (!is.null(cfg$produits) && length(cfg$produits) > 0) {
          render_questionnaire_ui(cfg, sheet_num)
        } else {
          div(class = "alert alert-warning", 
              style = "margin-top: 50px;",
              tagList(icon("exclamation-triangle"), 
                      paste(" La Page", sheet_num, "n'est pas encore configurée. Veuillez ajouter des produits via le panel Admin.")))
        }
      })
    })
  })
  
  # ========== GESTION DES SOUMISSIONS POUR TOUTES LES PAGES (1-10) ==========
  
  # Fonction pour sauvegarder les réponses
  save_responses <- function(cfg, page_id, panelist_name, responses, comments) {
    timestamp <- Sys.time()
    
    # Créer les données de réponse
    response_data <- data.frame(
      timestamp = timestamp,
      page = page_id,
      panelist = panelist_name,
      base = cfg$base,
      produit = character(0),
      code_produit = character(0),
      etape = character(0),
      support = character(0),
      strength = numeric(0),
      commentaire = character(0),
      stringsAsFactors = FALSE
    )
    
    # Parcourir tous les produits et étapes
    for (i in seq_along(cfg$produits)) {
      produit <- cfg$produits[i]
      code_produit <- if (length(cfg$codes_produits) >= i) cfg$codes_produits[i] else ""
      
      for (etape in cfg$etapes) {
        supports <- cfg$supports[[etape]]
        if (!is.null(supports) && length(supports) > 0) {
          
          # Récupérer le commentaire pour cette étape
          comment_id <- paste0("comment_", page_id, "_", i, "_", etape)
          comment_text <- comments[[comment_id]] %||% ""
          
          for (support in supports) {
            input_id <- paste0("strength_", page_id, "_", produit, "_", etape, "_", support)
            strength_value <- responses[[input_id]] %||% 0
            
            # Ajouter une ligne pour chaque combinaison
            new_row <- data.frame(
              timestamp = timestamp,
              page = page_id,
              panelist = panelist_name,
              base = cfg$base,
              produit = produit,
              code_produit = code_produit,
              etape = etape,
              support = support,
              strength = strength_value,
              commentaire = comment_text,
              stringsAsFactors = FALSE
            )
            
            response_data <- rbind(response_data, new_row)
          }
        }
      }
    }
    
    # Sauvegarder dans le fichier CSV
    if (file.exists(output_file)) {
      existing_data <- read_csv(output_file, show_col_types = FALSE)
      combined_data <- rbind(existing_data, response_data)
    } else {
      combined_data <- response_data
    }
    
    write_csv(combined_data, output_file)
    
    return(nrow(response_data))
  }
  
  # Créer dynamiquement les observateurs de soumission pour toutes les fiches actives
  observe({
    active_sheets <- global_config$active_sheets
    
    lapply(active_sheets, function(sheet_num) {
      local({
        current_sheet <- sheet_num
        
        observeEvent(input[[paste0("submit_", current_sheet)]], {
          cfg <- global_config[[paste0("page", current_sheet)]]
          panelist_name <- input[[paste0("panelist_name_", current_sheet)]]
          
          # Validation
          if (is.null(panelist_name) || panelist_name == "") {
            showNotification("Veuillez sélectionner votre nom avant de soumettre.", type = "error")
            return()
          }
          
          if (length(cfg$produits) == 0) {
            showNotification("Aucun produit configuré pour cette page.", type = "error")
            return()
          }
          
          # Collecter toutes les réponses et commentaires
          all_inputs <- reactiveValuesToList(input)
          
          # Filtrer les réponses de force pour cette page
          strength_pattern <- paste0("^strength_", current_sheet, "_")
          strength_responses <- all_inputs[grepl(strength_pattern, names(all_inputs))]
          
          # Filtrer les commentaires pour cette page
          comment_pattern <- paste0("^comment_", current_sheet, "_")
          comment_responses <- all_inputs[grepl(comment_pattern, names(all_inputs))]
          
          # Vérifier qu'il y a au moins quelques réponses
          if (length(strength_responses) == 0) {
            showNotification("Aucune évaluation détectée. Veuillez vérifier la configuration.", type = "error")
            return()
          }
          
          # Sauvegarder les réponses
          tryCatch({
            nb_responses <- save_responses(cfg, current_sheet, panelist_name, strength_responses, comment_responses)
            
            showNotification(
              paste("Réponses sauvegardées avec succès ! (", nb_responses, "évaluations enregistrées pour la Page", current_sheet, ")"), 
              type = "message", 
              duration = 5
            )
            
            # Réinitialiser les champs (optionnel)
            # updateSelectInput(session, paste0("panelist_name_", current_sheet), selected = "")
            
          }, error = function(e) {
            showNotification(paste("Erreur lors de la sauvegarde :", e$message), type = "error")
          })
        })
      })
    })
  })
  
  # ========== FONCTIONS UTILITAIRES SUPPLÉMENTAIRES ==========
  
  # Fonction pour valider la configuration d'une page
  validate_page_config <- function(cfg) {
    errors <- character(0)
    
    if (length(cfg$produits) == 0) {
      errors <- c(errors, "Aucun produit configuré")
    }
    
    if (is.null(cfg$base) || cfg$base == "") {
      errors <- c(errors, "Base non définie")
    }
    
    if (is.null(cfg$etapes) || length(cfg$etapes) == 0) {
      errors <- c(errors, "Aucune étape sélectionnée")
    }
    
    if (!is.null(cfg$etapes)) {
      for (etape in cfg$etapes) {
        if (is.null(cfg$supports[[etape]]) || length(cfg$supports[[etape]]) == 0) {
          errors <- c(errors, paste("Aucun support défini pour l'étape", etape))
        }
      }
    }
    
    return(errors)
  }
  
  # Observer pour afficher les erreurs de configuration
  observe({
    active_sheets <- global_config$active_sheets
    
    lapply(active_sheets, function(sheet_num) {
      cfg <- global_config[[paste0("page", sheet_num)]]
      errors <- validate_page_config(cfg)
      
      if (length(errors) > 0) {
        # Vous pouvez ajouter ici une logique pour afficher les erreurs
        # Par exemple, dans les logs ou dans l'interface
      }
    })
  })
  
  # ========== NETTOYAGE ET OPTIMISATIONS ==========
  
  
  # Fonction de nettoyage lors de la fermeture de session
  session$onSessionEnded(function() {
    # Sauvegarder une dernière fois si nécessaire
    tryCatch({
      if (isolate(global_config$initialized) && !isolate(global_config$products_saved)) {
        save_global_config()
      }
    }, error = function(e) {
      # Ignorer les erreurs lors de la fermeture
    })
  })
  
  # Observer pour la gestion de la mémoire
  observe({
    # Nettoyer les observateurs non utilisés si nécessaire
    # (Shiny le fait automatiquement, mais on peut ajouter du nettoyage personnalisé ici)
  })
  
  # ========== FONCTIONNALITÉS AVANCÉES ==========
  
  # Fonction pour exporter la configuration
  output$export_config <- downloadHandler(
    filename = function() {
      paste0("config_strength_", Sys.Date(), ".rds")
    },
    content = function(file) {
      file.copy(config_file, file)
    }
  )
  
  # Fonction pour importer une configuration (si nécessaire)
  observeEvent(input$import_config, {
    # Logique d'importation de configuration
    # À implémenter selon les besoins
  })
  
  # ========== MONITORING ET DEBUGGING ==========
  
  # Observer pour le debugging (peut être supprimé en production)
  observe({
    if (getOption("shiny.debug", FALSE)) {
      cat("Active sheets:", paste(global_config$active_sheets, collapse = ", "), "\n")
      cat("Current page:", global_config$page, "\n")
      cat("Products saved:", global_config$products_saved, "\n")
    }
  })
  
  # ========== FINALISATION ==========
  
  # Message de démarrage
  observe({
    if (global_config$initialized) {
      cat("Application Strength MP initialisée avec", length(global_config$active_sheets), "fiches actives\n")
    }
  })
  
} # Fin du serveur

# ========== LANCEMENT DE L'APPLICATION ==========

# Démarrer l'application Shiny
shinyApp(ui = ui, server = server)
