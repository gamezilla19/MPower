# Interface utilisateur principale

# Fonction pour obtenir les styles CSS principaux
get_main_css <- function() {
  "
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
  .config-section {
    background: #f8f9fa;
    padding: 15px;
    margin-bottom: 20px;
    border-radius: 5px;
    border-left: 4px solid #007bff;
  }
  .admin-modal .modal-dialog {
    max-width: 90%;
  }
  .form-group {
    margin-bottom: 15px;
  }
  .nav-tabs .nav-link {
    color: #007bff;
  }
  .nav-tabs .nav-link.active {
    background-color: #007bff;
    color: white;
    border-color: #007bff;
  }
  .tab-content {
    padding: 20px;
    border: 1px solid #dee2e6;
    border-top: none;
    border-radius: 0 0 5px 5px;
  }
  .questionnaire-section {
    margin-bottom: 30px;
    padding: 20px;
    background: #ffffff;
    border: 1px solid #e9ecef;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  .product-header {
    background: #e3f2fd;
    padding: 10px 15px;
    margin: -20px -20px 20px -20px;
    border-radius: 8px 8px 0 0;
    border-bottom: 2px solid #2196f3;
  }
  .rating-scale {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin: 10px 0;
  }
  .scale-labels {
    display: flex;
    justify-content: space-between;
    font-size: 12px;
    color: #666;
    margin-top: 5px;
  }
  "
}

# Fonction pour obtenir le JavaScript principal
get_main_js <- function() {
  "
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
    
    // Gestion des sliders pour une meilleure UX
    $(document).on('input', '.slider-input', function() {
      var value = $(this).val();
      var label = $(this).siblings('.slider-value');
      if (label.length) {
        label.text(value);
      }
    });
    
    // Animation pour les sections de questionnaire
    $('.questionnaire-section').hide().fadeIn(800);
  });
  "
}

# Panel admin flottant
get_admin_panel_ui <- function() {
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
  )
}

# Interface utilisateur principale
main_ui <- fluidPage(
  useShinyjs(),
  
  # Styles CSS
  tags$head(
    tags$style(HTML(get_main_css())),
    tags$script(HTML(get_main_js())),
    # Ajout de Bootstrap pour les onglets
    tags$link(rel = "stylesheet", 
              href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"),
    tags$script(src = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js")
  ),
  
  # Panel Admin flottant
  get_admin_panel_ui(),
  
  # Contenu principal
  div(class = "main-content",
      titlePanel("Evaluation de la puissance MP"),
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        
        # Tab QUESTIONNAIRE PAGE 1
        tabPanel("Questionnaire - Page 1",
                 value = "page1",
                 icon = icon("clipboard-list"),
                 br(),
                 uiOutput("questionnaire_ui1")
        ),
        
        # Tab QUESTIONNAIRE PAGE 2
        tabPanel("Questionnaire - Page 2",
                 value = "page2", 
                 icon = icon("clipboard-list"),
                 br(),
                 uiOutput("questionnaire_ui2")
        )
      )
  )
)
