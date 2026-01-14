library(shiny)
library(bslib)
library(bsicons)

ui <- page_sidebar(
  theme = pl_dark_theme, 
  
  # --- 1. HEADER WITH LOGO ---
  title = div(
    style = "display: flex; align-items: center; gap: 12px;",
    img(src = "https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg", 
        height = "35px", 
        style = "filter: brightness(0) invert(1); opacity: 0.9;"), 
    div(
      div(style = "font-size: 1.1rem; font-weight: 900; color: white; line-height: 1;", "FPL MANAGER"),
      div(style = "font-size: 0.65rem; color: #00FF85; letter-spacing: 1.5px; font-weight: bold;", "SEASON 2025/26")
    )
  ),
  
  # --- 2. CSS STYLING ---
  tags$head(tags$style(HTML("
    /* GLOBAL SCROLLBAR */
    ::-webkit-scrollbar { width: 6px; }
    ::-webkit-scrollbar-track { background: #190028; }
    ::-webkit-scrollbar-thumb { background: #00FF85; border-radius: 3px; }
    
    /* COMPACT SIDEBAR INPUTS */
    .form-group { margin-bottom: 8px !important; }
    
    .sidebar-title { 
      color: #00FF85; 
      font-size: 0.7rem; 
      font-weight: bold; 
      text-transform: uppercase; 
      margin-bottom: 2px; 
      letter-spacing: 0.5px; 
    }
    
    /* SLIM DROPDOWNS */
    .selectize-input {
      background-color: #190028 !important;
      border: 1px solid #444 !important;
      color: white !important;
      border-radius: 6px !important;
      min-height: 32px !important; 
      padding: 4px 8px !important;
      font-size: 0.85rem !important;
    }
    .selectize-dropdown { background-color: #190028; border: 1px solid #444; color: #ccc; }
    .selectize-dropdown .active { background-color: #2A0040; color: #00FF85; }
    
    /* --- NEW: NEON NUMERIC INPUT (Replaces Slider) --- */
    #max_price {
      background-color: #190028 !important;
      color: #00FF85 !important;      /* Neon Text */
      border: 1px solid #444 !important;
      font-weight: bold !important;
      font-family: 'Courier New', monospace; /* Tech font */
      border-radius: 6px;
      padding-left: 10px;
    }
    #max_price:focus {
      border-color: #00FF85 !important;
      box-shadow: 0 0 10px rgba(0, 255, 133, 0.2);
    }
    
    /* NAVIGATION TABS */
    .bslib-navs-card-underline .nav-link { color: #aaa !important; font-size: 0.9rem; padding: 10px 15px; }
    .bslib-navs-card-underline .nav-link.active { color: #00FF85 !important; border-bottom-color: #00FF85 !important; }
  "))),
  
  # --- 3. SIDEBAR ---
  sidebar = sidebar(
    width = 250, # Compact width
    bg = "#2A0040",
    gap = "0px", 
    
    # Dashboard Icon
    div(style = "padding-bottom: 10px; border-bottom: 1px solid rgba(255,255,255,0.1); margin-bottom: 10px;",
        span(bs_icon("sliders"), " Dashboard Filters", style = "color: white; font-weight: bold; font-size: 0.9rem;")
    ),
    
    # 1. TEAM
    div(class="sidebar-title", "Filter Team"),
    selectInput("team_select", label = NULL, choices = c("All Teams"), selected = "All Teams"),
    
    # 2. POSITION
    div(class="sidebar-title", "Position"),
    selectInput("position_select", label = NULL, 
                choices = c("All Positions", "GKP", "DEF", "MID", "FWD"), 
                selected = "All Positions"),
    
    # 3. AVAILABILITY
    div(class="sidebar-title", "Availability"),
    selectInput("status_select", label = NULL, 
                choices = c("All Players", "Available Only", "Doubts/Injured"), 
                selected = "All Players"),
    
    # 4. MAX PRICE (Updated to Digital Input)
    div(class="sidebar-title", "Max Price (£)"),
    numericInput("max_price", label = NULL, value = 16.0, min = 3.5, max = 16.0, step = 0.1),
    
    # Footer
    div(style = "margin-top: 15px; font-size: 0.65rem; color: #666; text-align: center; border-top: 1px solid rgba(255,255,255,0.05); padding-top: 10px;",
        "Last Sync: ", format(Sys.Date(), "%b %d")
    )
  ),
  
  # --- 4. MAIN DASHBOARD ---
  navset_card_underline(
    title = "Analysis Dashboard",
    nav_panel("Player Explorer", playerExplorerUI("explorer_module")),
    nav_panel("Dream Team", dreamTeamUI("dream_team_module")),
    nav_panel("League Analysis", leagueAnalysisUI("league_module")),
    nav_panel("Accuracy Check", accuracyUI("accuracy_module")),
    nav_panel("Season History", historyUI("history_module"))
  )
)