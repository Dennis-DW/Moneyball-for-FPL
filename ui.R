page_sidebar(
  theme = pl_dark_theme,
  title = "FPL Manager 2025/26",
  
  # --- CUSTOM CSS ---
  tags$head(tags$style(HTML("
    .value-box-grid { overflow: visible !important; }
    .card-body { overflow: visible !important; }
    ::-webkit-scrollbar { width: 8px; }
    ::-webkit-scrollbar-track { background: #190028; }
    ::-webkit-scrollbar-thumb { background: #00FF85; border-radius: 4px; }
    .card { border: 1px solid #38003C; box-shadow: 0px 4px 15px rgba(0, 255, 133, 0.1); }
  "))),
  
  # --- GLOBAL SIDEBAR ---
  sidebar = sidebar(
    bg = "#2A0040",
    
    # 1. TEAM FILTER
    selectInput("team_select", "Filter Team:", choices = c("All Teams"), selected = "All Teams"),
    
    # 2. POSITION FILTER
    selectInput("position_select", "Position:", 
                choices = c("All Positions", "GKP", "DEF", "MID", "FWD"), 
                selected = "All Positions"),
    
    # 3. AVAILABILITY FILTER
    selectInput("status_select", "Availability:", 
                choices = c("All Players", "Available Only", "Doubts/Injured"), 
                selected = "All Players"),
    
    # 4. PRICE FILTER
    sliderInput("cost_slider", "Max Price:", min = 4.0, max = 16.0, value = 16.0, step = 0.5, ticks = FALSE, pre = "£"),
    
    hr()
  ),
  
  # --- MAIN DASHBOARD AREA ---
  navset_card_underline(
    title = "Analysis Dashboard",
    
    # TAB 1: EXPLORER
    nav_panel("Player Explorer", playerExplorerUI("explorer_module")),
    
    # TAB 2: DREAM TEAM 
    nav_panel("Dream Team", dreamTeamUI("dream_team_module")),
    
    # TAB 3: LEAGUE ANALYSIS
    nav_panel("League Analysis", leagueAnalysisUI("league_module")),
    
    # TAB 4: ACCURACY CHECK
    nav_panel("Accuracy Check", accuracyUI("accuracy_module")),
    
    # TAB 5: HISTORY
    nav_panel("Season History", historyUI("history_module"))
  )
)