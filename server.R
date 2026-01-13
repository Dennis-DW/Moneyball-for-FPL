# server.R

function(input, output, session) {
  
  # 1. DATA HANDLING
  req(fpl_data)
  
  # 2. UPDATE SIDEBAR INPUTS
  updateSelectInput(
    session,
    "team_select",
    choices = c("All Teams", sort(unique(fpl_data$team_name)))
  )
  
  # MODULE 1: PLAYER EXPLORER
  playerExplorerServer(
    "explorer_module",
    fpl_data,
    team_filter     = reactive(input$team_select),
    position_filter = reactive(input$position_select),
    status_filter   = reactive(input$status_select),
    price_filter    = reactive(input$cost_slider)
  )
  
  # MODULE 2: DREAM TEAM AI
  dreamTeamServer("dream_team_module", fpl_data, budget_input = NULL)
  
  # MODULE 3: ACCURACY CHECK
  if (exists("accuracyServer")) accuracyServer("accuracy_module", fpl_data)
  
  # MODULE 4: SEASON HISTORY
  if (exists("historyServer")) historyServer("history_module", fpl_data)
  
  # MODULE 5: LEAGUE ANALYSIS (FIXED)
  leagueAnalysisServer("league_module", fpl_data)
}