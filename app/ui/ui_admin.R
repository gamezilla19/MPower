# Interface d'administration

get_login_modal_ui <- function() {
  modalDialog(
    title = tagList(icon("lock"), "Connexion Administrateur"),
    size = "s",
    div(class = "login-form",
        textInput("admin_username", "Nom d'utilisateur:", placeholder = "admin"),
        passwordInput("admin_password", "Mot de passe:", placeholder = "Mot de passe"),
        br(),
        div(class = "text-center",
            actionButton("admin_login", "Se connecter", class = "btn btn-primary"),
            actionButton("cancel_login", "Annuler", class = "btn btn-secondary")
        )
    ),
    footer = NULL
  )
}

get_admin_modal_ui <- function() {
  tagList(
    # JavaScript pour la gestion des onglets
    tags$script(HTML("
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
        
        // Empêcher la fermeture de modal lors des confirmations
        $(document).on('click', '.btn-danger[id^=\"confirm_delete\"]', function(e) {
          e.stopPropagation();
        });
      });
    ")),
    
    modalDialog(
      title = tagList(icon("cogs"), "Configuration de l'étude"),
      size = "l",
      class = "admin-modal",
      div(class = "admin-form",
          # Navigation par onglets
          get_admin_tabs_nav(),
          
          # Contenu des onglets
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
    )
  )
}

get_admin_tabs_nav <- function() {
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
  )
}

get_complete_admin_css <- function() {
  "
  /* Styles pour la modal d'administration */
  .admin-modal .modal-dialog {
    max-width: 95%;
    width: 1400px;
  }
  
  .admin-form {
    max-height: 80vh;
    overflow: hidden;
    padding: 0;
  }
  
  /* Styles pour le formulaire de login */
  .login-form {
    padding: 20px;
    text-align: left;
  }
  
  .login-form .form-group {
    margin-bottom: 15px;
  }
  
  .login-form .text-center {
    margin-top: 20px;
  }
  
  .login-form .btn {
    margin: 0 5px;
    padding: 8px 20px;
  }
  
  /* Styles pour les inputs de produits */
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
    background: #fafafa;
    transition: all 0.2s ease;
  }
  
  .product-input-group:hover {
    background: #f0f8ff;
    border-color: #007bff;
  }
  
  /* Styles pour les onglets */
  .nav-tabs {
    border-bottom: 2px solid #dee2e6;
    margin-bottom: 0;
    background: #f8f9fa;
  }
  
  .nav-tabs .nav-link {
    border: 1px solid transparent;
    border-radius: 8px 8px 0 0;
    padding: 12px 20px;
    font-weight: 500;
    color: #6c757d;
    transition: all 0.2s ease;
    margin-right: 2px;
  }
  
  .nav-tabs .nav-link:hover {
    background-color: #e9ecef;
    color: #495057;
  }
  
  .nav-tabs .nav-link.active {
    background-color: #007bff !important;
    color: white !important;
    border-color: #007bff #007bff #007bff !important;
    box-shadow: 0 2px 4px rgba(0,123,255,0.3);
  }
  
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
  
  /* Styles pour les panélistes */
  .panelist-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 12px 15px;
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
    background: linear-gradient(135deg, #e8f4fd 0%, #f0f9ff 100%);
    border: 2px dashed #007bff;
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 20px;
    text-align: left;
  }
  
  .add-panelist-section h4 {
    color: #007bff;
    margin-bottom: 15px;
  }
  
  /* Styles pour la section des produits */
  .products-section {
    border: 2px solid #dee2e6;
    border-radius: 8px;
    padding: 15px;
    margin-bottom: 20px;
    background: #fafafa;
  }
  
  .products-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 15px;
    border-bottom: 1px solid #dee2e6;
    padding-bottom: 10px;
  }
  
  .products-header h4 {
    margin: 0;
    color: #495057;
  }
  
  .save-confirmation {
    color: #28a745;
    font-weight: bold;
    margin-left: 10px;
    opacity: 0;
    transition: opacity 0.3s ease-in-out;
    font-size: 14px;
  }
  
  .save-confirmation.show {
    opacity: 1;
    animation: pulse 0.5s ease-in-out;
  }
  
  @keyframes pulse {
    0% { transform: scale(1); }
    50% { transform: scale(1.05); }
    100% { transform: scale(1); }
  }
  
  .panelists-container {
    max-height: 400px;
    overflow-y: auto;
    border: 1px solid #e9ecef;
    border-radius: 6px;
    padding: 10px;
    background: #fafafa;
  }
  
  /* Scrollbar personnalisé */
  .panelists-container::-webkit-scrollbar {
    width: 8px;
  }
  
  .panelists-container::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 4px;
  }
  
  .panelists-container::-webkit-scrollbar-thumb {
    background: #c1c1c1;
    border-radius: 4px;
  }
  
  .panelists-container::-webkit-scrollbar-thumb:hover {
    background: #a8a8a8;
  }
  
  /* Styles pour les sections de configuration */
  .config-section {
    background: #f8f9fa;
    padding: 15px;
    margin-bottom: 20px;
    border-radius: 5px;
    border-left: 4px solid #007bff;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  }
  
  .config-section h4 {
    color: #495057;
    margin-bottom: 15px;
    font-size: 16px;
  }
  
  /* Styles pour les boutons */
  .btn-sm {
    padding: 4px 8px;
    font-size: 12px;
  }
  
  .btn-danger:hover {
    transform: scale(1.05);
    transition: transform 0.1s ease;
  }
  
  /* Styles pour les alertes */
  .alert {
    border-radius: 6px;
    border: none;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  }
  
  .alert-info {
    background-color: #e3f2fd;
    color: #0277bd;
    border-left: 4px solid #2196f3;
  }
  
  .alert-warning {
    background-color: #fff3e0;
    color: #ef6c00;
    border-left: 4px solid #ff9800;
  }
  
  /* Styles pour les inputs */
  .form-control:focus {
    border-color: #007bff;
    box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25);
  }
  
  /* Animation pour les changements */
  .fade-in {
    animation: fadeIn 0.3s ease-in;
  }
  
  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(10px); }
    to { opacity: 1; transform: translateY(0); }
  }
  
  /* Responsive design */
  @media (max-width: 768px) {
    .admin-modal .modal-dialog {
      width: 95%;
      margin: 10px auto;
    }
    
    .products-header {
      flex-direction: column;
      align-items: flex-start;
      gap: 10px;
    }
    
    .product-input-group > div {
      flex-direction: column;
    }
    
    .nav-tabs .nav-link {
      padding: 8px 12px;
      font-size: 14px;
    }
  }
  
  /* Styles pour les tooltips */
  [title] {
    cursor: help;
  }
  
  /* Styles pour les états de chargement */
  .loading {
    opacity: 0.6;
    pointer-events: none;
  }
  
  /* Styles pour les messages de validation */
  .validation-message {
    font-size: 12px;
    margin-top: 5px;
    padding: 5px;
    border-radius: 3px;
  }
  
  .validation-error {
    color: #dc3545;
    background-color: #f8d7da;
    border: 1px solid #f5c6cb;
  }
  
  .validation-success {
    color: #155724;
    background-color: #d4edda;
    border: 1px solid #c3e6cb;
  }
  "
}

# Fonction pour obtenir les styles CSS (alias pour compatibilité)
get_admin_css <- function() {
  get_complete_admin_css()
}
