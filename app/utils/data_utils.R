# Utilitaires de données et rendu UI

render_config_content <- function(input, global_config, product_ids, products_changed) {
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
                       choices = BASE_CHOICES,
                       selected = cfg$base,
                       options = list(create = TRUE, placeholder = "Sélectionner ou saisir une base"))
    ),
    
    # Configuration des étapes
    div(class = "config-section",
        h4(tagList(icon("list-ol"), "Étapes à évaluer")),
        checkboxGroupInput("admin_etapes", NULL,
                           choices = ETAPES_CHOICES,
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
        actionButton("save_config", "Sauvegarder la configuration complète", 
                     class = "btn-success btn-lg",
                     icon = icon("save"))
    )
  )
}

render_panelists_content <- function(global_config) {
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
    # Section d'ajout
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
    
    # Liste des panélistes
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
}

render_dynamic_product_inputs <- function(global_config, product_ids, products_changed) {
  # Capture de l'état du focus
  runjs("
    const active = document.activeElement;
    window.preserveFocusState = active && active.matches('input[type=\"text\"]') ? {
      id: active.id,
      value: active.value,
      start: active.selectionStart,
      end: active.selectionEnd
    } : null;
  ")
  
  # Gestion du cas vide
  ids <- product_ids()
  if (length(ids) == 0) {
    product_inputs <- list(
      div(class = "alert alert-info",
          tagList(icon("info-circle"), " Aucun produit configuré pour cette page. Cliquez sur 'Ajouter un produit' pour commencer."))
    )
  } else {
    # Génération des inputs
    product_inputs <- lapply(ids, function(i) {
      current_product <- global_config$temp_products[[as.character(i)]] %||% ""
      current_code <- global_config$temp_codes[[as.character(i)]] %||% ""
      
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
  
  # Restauration du focus
  delay(100, {
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
  
  # Section complète
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
          if (has_real_changes(global_config)) {
            span(style = "color: #ffc107; margin-left: 15px; font-style: italic;",
                 icon("exclamation-triangle"), " Modifications non sauvegardées")
          }
      )
  )
}

render_support_inputs <- function(input, global_config) {
  req(input$admin_etapes)
  cfg <- if (global_config$page == 1) global_config$page1 else global_config$page2
  
  lapply(input$admin_etapes, function(etape) {
    selected_supports <- if (!is.null(cfg$supports[[etape]])) cfg$supports[[etape]] else NULL
    div(style = "margin-bottom: 15px;",
        strong(paste("Supports pour", etape, ":")),
        checkboxGroupInput(paste0("support_", etape), NULL,
                           choices = SUPPORTS_CHOICES,
                           selected = selected_supports,
                           inline = TRUE)
    )
  })
}
