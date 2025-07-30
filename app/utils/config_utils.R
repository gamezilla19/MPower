# Utilitaires de configuration

initialize_config <- function(global_config, product_ids) {
  if (file.exists(CONFIG_FILE)) {
    conf <- readRDS(CONFIG_FILE)
    global_config$page1 <- conf$page1 %||% list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list())
    global_config$page2 <- conf$page2 %||% list(produits = NULL, codes_produits = NULL, base = NULL, etapes = NULL, supports = list())
    global_config$panelistes <- conf$panelistes %||% DEFAULT_PANELISTS
    
    # Initialize product IDs based on existing products
    max_products <- max(
      length(global_config$page1$produits %||% character(0)),
      length(global_config$page2$produits %||% character(0)),
      1
    )
    product_ids(seq_len(max_products))
    
    # Initialisation des valeurs temporaires
    load_temp_products_for_page(global_config, product_ids, global_config$page)
    
    global_config$products_saved <- TRUE
    global_config$initialized <- TRUE
  }
}

load_temp_products_for_page <- function(global_config, product_ids, page_num) {
  cfg <- if (page_num == 1) global_config$page1 else global_config$page2
  
  produits <- cfg$produits %||% character(0)
  codes <- cfg$codes_produits %||% character(0)
  
  if (length(produits) == 0 && length(codes) == 0) {
    product_ids(integer(0))
    global_config$temp_products <- list()
    global_config$temp_codes <- list()
  } else {
    product_ids(seq_along(produits))
    global_config$temp_products <- setNames(as.list(produits), seq_along(produits))
    global_config$temp_codes <- setNames(as.list(codes), seq_along(codes))
  }
  
  global_config$products_saved <- TRUE
}

has_real_changes <- function(global_config) {
  current_page_config <- if (global_config$page == 1) global_config$page1 else global_config$page2
  
  saved_products <- current_page_config$produits %||% character(0)
  temp_products <- unlist(global_config$temp_products) %||% character(0)
  
  saved_codes <- current_page_config$codes_produits %||% character(0)
  temp_codes <- unlist(global_config$temp_codes) %||% character(0)
  
  temp_products_clean <- temp_products[nzchar(temp_products) & !is.na(temp_products)]
  temp_codes_clean <- temp_codes[nzchar(temp_codes) & !is.na(temp_codes)]
  
  !identical(saved_products, temp_products_clean) || !identical(saved_codes, temp_codes_clean)
}

check_input_integrity <- function(type, id, input, global_config) {
  input_val <- switch(type,
                      "product" = input[[paste0("admin_produit", id)]],
                      "code" = input[[paste0("admin_code_produit", id)]]) %||% ""
  
  stored_val <- switch(type,
                       "product" = global_config$temp_products[[as.character(id)]] %||% "",
                       "code" = global_config$temp_codes[[as.character(id)]] %||% "")
  
  !identical(trimws(input_val), trimws(stored_val))
}

update_config_safely <- function(type, id, value, global_config) {
  if (type == "product") {
    global_config$temp_products[[as.character(id)]] <- value
  } else {
    global_config$temp_codes[[as.character(id)]] <- value
  }
  global_config$products_saved <- FALSE
}

save_complete_config <- function(global_config) {
  saveRDS(list(
    page1 = global_config$page1,
    page2 = global_config$page2,
    panelistes = global_config$panelistes
  ), file = CONFIG_FILE)
}

save_page_config <- function(input, global_config, page_num) {
  req(input$admin_base, input$admin_etapes)
  
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
  
  save_complete_config(global_config)
}

save_products <- function(global_config, input, output, session) {
  produits_valides <- unlist(global_config$temp_products) %>% 
    .[nzchar(.) & !is.na(.)]
  codes_valides <- unlist(global_config$temp_codes) %>% 
    .[nzchar(.) & !is.na(.)]
  
  if (length(produits_valides) == 0) {
    showNotification("Aucun produit valide à sauvegarder !", type = "warning")
    return()
  }
  
  if (any(nchar(produits_valides) < 2)) {
    showNotification("Les noms de produits doivent contenir au moins 2 caractères !", type = "error")
    return()
  }
  
  isolate({
    if (global_config$page == 1) {
      global_config$page1$produits <- produits_valides
      global_config$page1$codes_produits <- codes_valides
    } else {
      global_config$page2$produits <- produits_valides
      global_config$page2$codes_produits <- codes_valides
    }
    
    save_complete_config(global_config)
  })
  
  global_config$products_saved <- TRUE
  global_config$show_save_confirmation <- TRUE
  
  showNotification(paste("Produits de la Page", global_config$page, "sauvegardés avec succès !"), 
                   type = "message", duration = 3)
  
  delay(3000, {
    global_config$show_save_confirmation <- FALSE
  })
}

remove_product <- function(global_config, product_ids, id) {
  global_config$temp_products[[as.character(id)]] <- NULL
  global_config$temp_codes[[as.character(id)]] <- NULL
  updated_ids <- setdiff(product_ids(), id)
  product_ids(updated_ids)
  global_config$products_saved <- FALSE
  
  delay(100, {
    runjs("
      const inputs = document.querySelectorAll('[id^=\"admin_produit\"]');
      if (inputs.length > 0) inputs[inputs.length-1].focus();
    ")
  })
}
