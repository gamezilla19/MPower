library(shiny)
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

# Reactive values pour stocker la configuration de l'etude
global_config <- reactiveValues(
  page = 1,
  page1 = list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list()),
  page2 = list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list()),
  admin_logged = FALSE,
  show_admin = FALSE
)

# UI
ui <- fluidPage(
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
    "))
  ),
  
  # Panel Admin flottant (déplacé à droite)
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
        
        # Tab QUESTIONNAIRE PAGE 1 ---------------------------------------------
        tabPanel("Questionnaire - Page 1",
                 icon = icon("clipboard-list"),
                 br(),
                 uiOutput("questionnaire_ui1")
        ),
        
        # Tab QUESTIONNAIRE PAGE 2 ---------------------------------------------
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
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("reponses_strength_", Sys.Date(), ".csv")
    },
    content = function(file) {
      file.copy(output_file, file)
    }
  )  
  
  # Charger les paramètres au démarrage
  if (file.exists(config_file)) {
    conf <- readRDS(config_file)
    global_config$page1 <- conf$page1
    global_config$page2 <- conf$page2
  }
  
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
  
  output$admin_form_content <- renderUI({
    req(global_config$admin_logged)
    
    etapes_choices <- c("NEAT", "BLOOM", "WET", "DRY", "DRYER")
    n_max <- 10
    
    cfg <- if (global_config$page == 1) global_config$page1 else global_config$page2
    
    produit_inputs <- div(class = "product-inputs",
                          lapply(1:n_max, function(i) {
                            div(
                              textInput(paste0("admin_produit", i), paste("Produit", i),
                                        value = if (!is.null(cfg$produits) && length(cfg$produits) >= i) cfg$produits[[i]] else "",
                                        placeholder = paste("Nom du produit", i)),
                              textInput(paste0("admin_code_produit", i), paste("Code", i),
                                        value = if (!is.null(cfg$codes_produits) && length(cfg$codes_produits) >= i) cfg$codes_produits[[i]] else "",
                                        placeholder = paste("Code produit", i))
                            )
                          })
    )
    
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
      div(class = "config-section",
          h4(tagList(icon("boxes"), "Produits et codes")),
          produit_inputs
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
      
      # Boutons de sauvegarde
      div(style = "text-align: center; margin-top: 30px; padding-top: 20px; border-top: 1px solid #dee2e6;",
          actionButton("save_config", "Sauvegarder la configuration", 
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
  
  enregistrer_config <- function(page_num) {
    req(input$admin_base, input$admin_etapes)
    n_max <- 10
    
    # Récupérer tous les produits saisis
    produits_saisis <- sapply(1:n_max, function(i) input[[paste0("admin_produit", i)]])
    produits_saisis <- produits_saisis[produits_saisis != ""]
    
    # Récupérer tous les codes produits saisis
    codes_produits_saisis <- sapply(1:n_max, function(i) input[[paste0("admin_code_produit", i)]])
    # Garder seulement les codes correspondant aux produits non vides
    codes_produits_saisis <- codes_produits_saisis[1:length(produits_saisis)]
    
    supports_list <- list()
    for (etape in input$admin_etapes) {
      supports_list[[etape]] <- input[[paste0("support_", etape)]]
    }
    
    config <- list(
      produits = produits_saisis,
      codes_produits = codes_produits_saisis,
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
  
  observeEvent(input$setup_page1, {
    global_config$page <- 1
  })
  
  observeEvent(input$setup_page2, {
    global_config$page <- 2
  })
  
  observeEvent(input$save_config, {
    enregistrer_config(global_config$page)
    showNotification(paste("Configuration de la Page", global_config$page, "enregistrée avec succès !"), 
                     type = "message", duration = 3)
  })
  
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
  
  observeEvent(input$submit_1, {
    enregistrer_reponses(1)
  })
  
  observeEvent(input$submit_2, {
    enregistrer_reponses(2)
  })
  
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

shinyApp(ui, server)
