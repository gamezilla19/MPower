# Interface des questionnaires

render_questionnaire_ui <- function(cfg) {
  req(cfg$produits, cfg$base, cfg$etapes, cfg$supports)
  
  produit_panels <- lapply(seq_along(cfg$produits), function(i) {
    create_product_panel(cfg, i)
  })
  
  tagList(
    get_questionnaire_header(cfg),
    get_questionnaire_info(),
    produit_panels,
    get_questionnaire_footer(cfg)
  )
}

create_product_panel <- function(cfg, i) {
  produit <- cfg$produits[i]
  code_produit <- if (length(cfg$codes_produits) >= i) cfg$codes_produits[i] else ""
  
  sliders <- list()
  for (etape in cfg$etapes) {
    supports <- cfg$supports[[etape]]
    if (!is.null(supports) && length(supports) > 0) {
      for (support in supports) {
        sliders <- append(sliders, create_strength_slider(produit, etape, support))
      }
    }
  }
  
  wellPanel(
    h4(tagList(icon("box"), paste("Produit :", produit))),
    if (code_produit != "") p(strong("Code :", code_produit)),
    sliders
  )
}

create_strength_slider <- function(produit, etape, support) {
  input_id <- paste0("strength_", produit, "_", etape, "_", support)
  icone <- get_etape_icon(etape)
  
  list(sliderInput(
    inputId = input_id,
    label = tagList(icone, paste(etape, "/", support)),
    min = 0, max = 4, value = 0, step = 0.1, ticks = FALSE
  ))
}

get_etape_icon <- function(etape) {
  switch(etape,
         "NEAT" = icon("flask"),
         "BLOOM" = icon("fire"),
         "WET" = icon("tint"),
         "DRY" = icon("wind"),
         "DRYER" = icon("wind"),
         icon("dot-circle"))
}

get_questionnaire_header <- function(cfg) {
  div(class = "config-section",
      selectInput(paste0("panelist_name_", cfg$page_id), 
                  tagList(icon("user"), "Votre nom :"), 
                  choices = c("Veuillez sélectionner" = "", cfg$panelistes), 
                  selected = ""),   
      strong(paste("Base :", cfg$base))
  )
}

get_questionnaire_info <- function() {
  div(style = "background-color: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin-bottom: 20px; border-radius: 4px;",
      tagList(icon("info-circle"), " Merci d'évaluer la force pour chaque combinaison produit / support / étape."))
}

get_questionnaire_footer <- function(cfg) {
  div(style = "text-align: center; margin-top: 30px;",
      actionButton(paste0("submit_", cfg$page_id), 
                   label = tagList(icon("check"), "Soumettre les réponses"),
                   class = "btn-success btn-lg")
  )
}
