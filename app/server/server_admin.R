# Logique d'administration

server_admin_logic <- function(input, output, session, global_config, product_ids, products_changed) {
  
  # Credentials administrateur
  ADMIN_USER <- "admin"
  ADMIN_PASS <- "1234"
  
  # Observer pour le bouton admin dans la navbar
  observeEvent(input$admin_btn, {
    if (!global_config$admin_logged) {
      showModal(get_login_modal_ui())
    } else {
      # Ajouter le runjs pour forcer l'activation des onglets
      showModal(get_admin_modal_ui())
      
      # AJOUTER CETTE PARTIE CRITIQUE
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
    }
  })
  
  # Observer pour la tentative de connexion
  observeEvent(input$login_submit, {
    if (input$admin_username == ADMIN_USER && input$admin_password == ADMIN_PASS) {
      global_config$admin_logged <- TRUE
      removeModal()
      showNotification("Connexion réussie !", type = "success")
      
      # Ouvrir directement le panel admin après connexion
      showModal(get_admin_modal_ui())
      
      # Forcer l'activation de l'onglet configuration
      runjs("
        setTimeout(function() {
          $('#config-tab').addClass('show active');
          $('a[href=\"#config-tab\"]').addClass('active');
          $('#panelists-tab').removeClass('show active');
          $('a[href=\"#panelists-tab\"]').removeClass('active');
        }, 150);
      ")
    } else {
      showNotification("Identifiants incorrects !", type = "error")
    }
  })
  
  # Observer pour fermer la modal de connexion
  observeEvent(input$login_cancel, {
    removeModal()
  })
  
  # Modal de connexion admin (ancien système - gardé pour compatibilité)
  observeEvent(input$show_login, {
    showModal(get_login_modal_ui())
  })
  
  # Authentification admin (ancien système - gardé pour compatibilité)
  observeEvent(input$login_btn, {
    if (input$admin_user == ADMIN_USER && input$admin_pass == ADMIN_PASS) {
      global_config$admin_logged <- TRUE
      removeModal()
      showNotification("Connexion réussie !", type = "message")
    } else {
      showNotification("Login ou mot de passe incorrect.", type = "error")
    }
  })
  
  # Affichage du panel admin
  observeEvent(input$show_admin_panel, {
    showModal(get_admin_modal_ui())
    
    # Forcer l'activation de l'onglet configuration
    runjs("
      setTimeout(function() {
        $('#config-tab').addClass('show active');
        $('a[href=\"#config-tab\"]').addClass('active');
        $('#panelists-tab').removeClass('show active');
        $('a[href=\"#panelists-tab\"]').removeClass('active');
      }, 150);
    ")
  })
  
  # Fermeture du panel admin
  observeEvent(input$close_admin, {
    removeModal()
  })
  
  # Observer pour la déconnexion
  observeEvent(input$logout_btn, {
    global_config$admin_logged <- FALSE
    removeModal()  # Fermer le panel admin
    showNotification("Déconnexion réussie", type = "success")
  })
  
  # Gestion des panélistes
  manage_panelists(input, output, session, global_config)
}

manage_panelists <- function(input, output, session, global_config) {
  
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
    
    # Sauvegarder
    save_complete_config(global_config)
    
    # Réinitialiser le champ
    updateTextInput(session, "new_panelist_name", value = "")
    
    showNotification(paste("Panéliste", new_name, "ajouté avec succès !"), type = "message")
  })
  
  # Supprimer un panéliste
  observe({
    lapply(global_config$panelistes, function(panelist) {
      btn_id <- paste0("remove_panelist_", gsub("[^A-Za-z0-9]", "_", panelist))
      observeEvent(input[[btn_id]], {
        global_config$panelist_to_delete <- panelist
        show_delete_confirmation_modal(panelist)
      }, ignoreInit = TRUE)
    })
  })
  
  # Confirmer la suppression
  observeEvent(input$confirm_delete_panelist, {
    panelist_to_remove <- global_config$panelist_to_delete
    
    if (!is.null(panelist_to_remove)) {
      global_config$panelistes <- setdiff(global_config$panelistes, panelist_to_remove)
      save_complete_config(global_config)
      showNotification(paste("Panéliste", panelist_to_remove, "supprimé avec succès."), type = "message")
      global_config$panelist_to_delete <- NULL
    }
    
    removeModal()
  })
  
  # Annuler la suppression
  observeEvent(input$cancel_delete, {
    global_config$panelist_to_delete <- NULL
    removeModal()
  })
}

show_delete_confirmation_modal <- function(panelist) {
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
}
