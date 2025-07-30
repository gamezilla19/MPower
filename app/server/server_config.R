# Gestion de la configuration

server_config_logic <- function(input, output, session, global_config, product_ids, products_changed) {
  
  # Changement de page
  observe({
    if (!is.null(input$config_page)) {
      global_config$page <- as.numeric(input$config_page)
    }
  })
  
  # Rendu du contenu de configuration
  output$admin_config_content <- renderUI({
    render_config_content(input, global_config, product_ids, products_changed)
  })
  
  # Rendu du contenu des panélistes
  output$admin_panelists_content <- renderUI({
    render_panelists_content(global_config)
  })
  
  # Gestion des produits dynamiques
  observe({
    current_page <- paste0("page", global_config$page)
    cfg <- global_config[[current_page]]
    
    if (!is.null(cfg$produits) && length(cfg$produits) > 0) {
      current_ids <- product_ids[[current_page]]
      
      for (i in seq_along(cfg$produits)) {
        local({
          idx <- i
          current_id <- if (length(current_ids) >= idx) current_ids[idx] else paste0("prod_", idx)
          
          # Observer pour les noms de produits
          observe({
            input_id <- paste0("produit_", current_id)
            if (!is.null(input[[input_id]])) {
              if (cfg$produits[idx] != input[[input_id]]) {
                cfg$produits[idx] <- input[[input_id]]
                global_config[[current_page]]$produits <- cfg$produits
                products_changed(TRUE)
              }
            }
          })
          
          # Observer pour les codes produits
          observe({
            input_id <- paste0("code_", current_id)
            if (!is.null(input[[input_id]])) {
              if (cfg$codes_produits[idx] != input[[input_id]]) {
                cfg$codes_produits[idx] <- input[[input_id]]
                global_config[[current_page]]$codes_produits <- cfg$codes_produits
                products_changed(TRUE)
              }
            }
          })
        })
      }
    }
  })
  
  # Ajout de produit
  observeEvent(input$add_product, {
    current_page <- paste0("page", global_config$page)
    cfg <- global_config[[current_page]]
    
    # Initialiser les listes si elles n'existent pas
    if (is.null(cfg$produits)) cfg$produits <- character(0)
    if (is.null(cfg$codes_produits)) cfg$codes_produits <- character(0)
    
    new_length <- length(cfg$produits) + 1
    cfg$produits <- c(cfg$produits, paste("Produit", new_length))
    cfg$codes_produits <- c(cfg$codes_produits, paste0("P", new_length))
    
    # Mettre à jour les IDs de produits
    if (is.null(product_ids[[current_page]])) {
      product_ids[[current_page]] <- character(0)
    }
    product_ids[[current_page]] <- c(product_ids[[current_page]], paste0("prod_", new_length))
    
    global_config[[current_page]] <- cfg
    products_changed(TRUE)
    
    showNotification("Produit ajouté avec succès", type = "message", duration = 2)
  })
  
  # Suppression de produit
  observe({
    current_page <- paste0("page", global_config$page)
    cfg <- global_config[[current_page]]
    
    if (!is.null(cfg$produits) && length(cfg$produits) > 0) {
      current_ids <- product_ids[[current_page]]
      
      for (i in seq_along(cfg$produits)) {
        local({
          idx <- i
          current_id <- if (length(current_ids) >= idx) current_ids[idx] else paste0("prod_", idx)
          
          observeEvent(input[[paste0("remove_", current_id)]], {
            if (length(cfg$produits) > 1) {
              cfg$produits <- cfg$produits[-idx]
              cfg$codes_produits <- cfg$codes_produits[-idx]
              
              # Mettre à jour les IDs
              product_ids[[current_page]] <- product_ids[[current_page]][-idx]
              
              global_config[[current_page]] <- cfg
              products_changed(TRUE)
              
              showNotification("Produit supprimé", type = "message", duration = 2)
            } else {
              showNotification("Impossible de supprimer le dernier produit", type = "warning", duration = 3)
            }
          })
        })
      }
    }
  })
  
  # Gestion des autres champs de configuration
  observe({
    current_page <- paste0("page", global_config$page)
    
    # Base
    if (!is.null(input$admin_base)) {
      if (is.null(global_config[[current_page]]$base) || 
          global_config[[current_page]]$base != input$admin_base) {
        global_config[[current_page]]$base <- input$admin_base
        products_changed(TRUE)
      }
    }
    
    # Étapes
    if (!is.null(input$admin_etapes)) {
      if (is.null(global_config[[current_page]]$etapes) || 
          !identical(global_config[[current_page]]$etapes, input$admin_etapes)) {
        global_config[[current_page]]$etapes <- input$admin_etapes
        products_changed(TRUE)
      }
    }
  })
  
  # Gestion des supports dynamiques
  observe({
    if (!is.null(input$admin_etapes) && length(input$admin_etapes) > 0) {
      current_page <- paste0("page", global_config$page)
      
      for (etape in input$admin_etapes) {
        local({
          etape_name <- etape
          input_id <- paste0("support_", gsub("[^A-Za-z0-9]", "_", etape_name))
          
          observe({
            if (!is.null(input[[input_id]])) {
              if (is.null(global_config[[current_page]]$supports)) {
                global_config[[current_page]]$supports <- list()
              }
              
              if (is.null(global_config[[current_page]]$supports[[etape_name]]) ||
                  global_config[[current_page]]$supports[[etape_name]] != input[[input_id]]) {
                global_config[[current_page]]$supports[[etape_name]] <- input[[input_id]]
                products_changed(TRUE)
              }
            }
          })
        })
      }
    }
  })
  
  # Sauvegarde de la configuration
  observeEvent(input$save_config, {
    tryCatch({
      config_to_save <- list(
        page1 = global_config$page1,
        page2 = global_config$page2,
        panelistes = global_config$panelistes
      )
      saveRDS(config_to_save, CONFIG_FILE)
      
      global_config$products_saved <- TRUE
      global_config$show_save_confirmation <- TRUE
      products_changed(FALSE)
      
      # Masquer la confirmation après 3 secondes
      invalidateLater(3000, session)
      global_config$show_save_confirmation <- FALSE
      
      showNotification("Configuration sauvegardée avec succès!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Erreur lors de la sauvegarde:", e$message), type = "error", duration = 5)
    })
  })
  
  # Gestion des panélistes
  manage_panelists_logic(input, output, session, global_config)
}

# Fonction auxiliaire pour la gestion des panélistes
manage_panelists_logic <- function(input, output, session, global_config) {
  
  # Ajout de panéliste
  observeEvent(input$add_panelist, {
    if (!is.null(input$new_panelist_name) && nchar(trimws(input$new_panelist_name)) > 0) {
      new_name <- trimws(input$new_panelist_name)
      
      if (!new_name %in% global_config$panelistes) {
        global_config$panelistes <- c(global_config$panelistes, new_name)
        updateTextInput(session, "new_panelist_name", value = "")
        showNotification("Panéliste ajouté avec succès", type = "message", duration = 2)
      } else {
        showNotification("Ce panéliste existe déjà", type = "warning", duration = 3)
      }
    } else {
      showNotification("Veuillez saisir un nom valide", type = "error", duration = 3)
    }
  })
  
  # Suppression de panéliste
  observe({
    if (!is.null(global_config$panelistes) && length(global_config$panelistes) > 0) {
      for (i in seq_along(global_config$panelistes)) {
        local({
          idx <- i
          observeEvent(input[[paste0("remove_panelist_", idx)]], {
            if (length(global_config$panelistes) > 1) {
              global_config$panelistes <- global_config$panelistes[-idx]
              showNotification("Panéliste supprimé", type = "message", duration = 2)
            } else {
              showNotification("Impossible de supprimer le dernier panéliste", type = "warning", duration = 3)
            }
          })
        })
      }
    }
  })
}
