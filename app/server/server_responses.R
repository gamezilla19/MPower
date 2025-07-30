# Gestion des réponses

server_responses_logic <- function(input, output, session, global_config) {
  
  # Génération des interfaces de questionnaire
  output$questionnaire_ui1 <- renderUI({ 
    if (!is.null(global_config$page1$produits)) {
      cfg <- c(global_config$page1, list(page_id = 1, panelistes = global_config$panelistes))
      render_questionnaire_ui(cfg)
    } else {
      get_config_warning_ui(1)
    }
  })
  
  output$questionnaire_ui2 <- renderUI({ 
    if (!is.null(global_config$page2$produits)) {
      cfg <- c(global_config$page2, list(page_id = 2, panelistes = global_config$panelistes))
      render_questionnaire_ui(cfg)
    } else {
      get_config_warning_ui(2)
    }
  })
  
  # Observateurs pour la soumission des réponses
  observeEvent(input$submit_1, {
    save_responses(input, global_config, 1)
  })
  
  observeEvent(input$submit_2, {
    save_responses(input, global_config, 2)
  })
}

save_responses <- function(input, global_config, page_id) {
  cfg <- if (page_id == 1) global_config$page1 else global_config$page2
  panelist_name <- input[[paste0("panelist_name_", page_id)]]
  
  if (is.null(panelist_name) || panelist_name == "") {
    showNotification("Veuillez sélectionner votre nom avant de valider.", type = "error")
    return(NULL)
  }
  
  # Vérification de duplication
  if (check_duplicate_response(panelist_name, cfg)) {
    showNotification("Vous avez déjà répondu à ce test pour au moins un des produits.", type = "warning")
    return(NULL)
  }
  
  # Collecte des réponses
  responses <- collect_responses(input, cfg, panelist_name)
  
  if (length(responses) > 0) {
    save_responses_to_file(responses)
    showNotification("Merci ! Vos réponses ont été enregistrées avec succès.", type = "message", duration = 5)
  }
}

# Gestion des réponses (suite)

collect_responses <- function(input, cfg, panelist_name) {
  responses <- list()
  
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
              code_produit = code_produit,
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
  
  return(responses)
}

check_duplicate_response <- function(panelist_name, cfg) {
  if (file.exists(OUTPUT_FILE)) {
    previous <- read_csv(OUTPUT_FILE, show_col_types = FALSE)
    deja_repondu <- previous %>%
      filter(paneliste == panelist_name, base == cfg$base, produit %in% cfg$produits)
    return(nrow(deja_repondu) > 0)
  }
  return(FALSE)
}

save_responses_to_file <- function(responses) {
  response_df <- do.call(rbind, responses)
  if (!file.exists(OUTPUT_FILE)) {
    write_csv(response_df, OUTPUT_FILE)
  } else {
    write_csv(response_df, OUTPUT_FILE, append = TRUE)
  }
}

get_config_warning_ui <- function(page_num) {
  div(class = "alert alert-warning", 
      style = "margin-top: 50px;",
      tagList(icon("exclamation-triangle"), 
              paste(" Configuration de la page", page_num, "non définie. Veuillez configurer depuis le panel Admin.")))
}

