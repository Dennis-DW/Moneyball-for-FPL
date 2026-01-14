function(input, output, session) {
  
  # 1. DATA HANDLING
  req(fpl_data)
  
  # --- LOAD FIXTURES (Safety Check) ---
  fixtures_df <- tryCatch({
    readRDS("data/fpl_raw.rds")$fixtures
  }, error = function(e) {
    NULL 
  })
  
  # 2. UPDATE SIDEBAR INPUTS
  updateSelectInput(
    session,
    "team_select",
    choices = c("All Teams", sort(unique(fpl_data$team_name)))
  )
  
  # MODULE 1: PLAYER EXPLORER
  # FIX: Connected 'price_filter' to the new 'input$max_price'
  playerExplorerServer(
    "explorer_module",
    fpl_data,
    team_filter       = reactive(input$team_select),
    position_filter   = reactive(input$position_select),
    status_filter     = reactive(input$status_select),
    price_filter      = reactive(input$max_price) 
  )
  
  # MODULE 2: DREAM TEAM
  dreamTeamServer("dream_team_module", fpl_data)
  
  # MODULE 3: ACCURACY CHECK
  if (exists("accuracyServer")) accuracyServer("accuracy_module", fpl_data)
  
  # MODULE 4: SEASON HISTORY
  if (exists("historyServer")) {
    historyServer("history_module", fpl_data, fixtures_df)
  }
  
  # MODULE 5: LEAGUE ANALYSIS
  if (exists("leagueAnalysisServer")) {
    leagueAnalysisServer("league_module", fpl_data)
  }
}