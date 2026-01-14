# modules/league_analysis_ui.R

leagueAnalysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- CSS STYLING ---
    tags$head(tags$style(HTML("
      /* 1. LEFT SIDEBAR & LAYOUT */
      .sidebar-panel {
        background-color: #190028;
        border: 1px solid #38003C;
        border-radius: 12px;
        padding: 20px; 
        height: 100%;
        display: flex;
        flex-direction: column;
        gap: 15px;
      }
      .sidebar-header {
        border-bottom: 1px solid #38003C;
        padding-bottom: 10px;
        margin-bottom: 5px;
        display: flex;
        justify-content: space-between;
        align-items: baseline;
      }
      .sidebar-timestamp { color: #888; font-size: 0.75rem; }

      /* 2. KPI CARDS */
      .kpi-stack { display: flex; flex-direction: column; gap: 10px; }
      .kpi-card {
        background: rgba(255,255,255,0.04);
        border: 1px solid rgba(255,255,255,0.1);
        border-left: 5px solid #00FF85; 
        border-radius: 8px;
        padding: 10px 15px; 
        display: flex;
        justify-content: space-between;
        align-items: center;
        height: 65px; 
      }
      .kpi-left { display: flex; flex-direction: column; justify-content: center; }
      .kpi-label { font-size: 0.65rem; color: #bbb; text-transform: uppercase; font-weight: 600; margin-bottom: 2px; }
      .kpi-value { font-size: 1.2rem; font-weight: 800; color: #fff; line-height: 1; }
      .kpi-icon { font-size: 1.5rem; color: #666; opacity: 0.6; }
      .accent-pink { border-left-color: #E90052; }
      .accent-cyan { border-left-color: #04F5FF; }

      /* 3. TABLE CONTAINER */
      .standings-wrapper {
        flex-grow: 1; 
        overflow-y: auto;
        border-top: 1px solid #38003C;
        padding-top: 15px;
        min-height: 250px;
      }

      /* 4. CUSTOM DROPDOWN STYLING  */
      .selectize-input {
        background: #2A0040 !important;
        border: 1px solid #00FF85 !important;
        color: white !important;
        border-radius: 6px !important;
        padding: 8px 10px !important;
        box-shadow: 0 0 5px rgba(0, 255, 133, 0.2);
        font-weight: bold;
      }
      .selectize-input input { color: white !important; }
      .selectize-dropdown {
        background: #190028 !important;
        border: 1px solid #00FF85 !important;
        color: white !important;
      }
      .selectize-dropdown .option { color: #ccc !important; }
      .selectize-dropdown .active {
        background: #00FF85 !important;
        color: #190028 !important; 
        font-weight: bold;
      }
      .form-group { margin-bottom: 0px; }

      /* 5. RIGHT CONTENT & COMPACT TABS */
      .bslib-card { 
        border: 1px solid #38003C; 
        background-color: #190028; 
        border-radius: 12px; 
      }
      
      /* --- UPDATED TAB STYLING START --- */
      .nav-tabs { 
        display: flex !important;
        flex-wrap: nowrap !important; /* Force single row */
        white-space: nowrap !important;
        overflow-x: auto !important;
        overflow-y: hidden;
        border-bottom: 1px solid #38003C !important; 
        background-color: #2A0040 !important;
        border-top-left-radius: 12px;
        border-top-right-radius: 12px;
        padding: 0 5px !important; 
        height: 35px !important; /* Reduced Height */
        align-items: center;
      }
      
      .nav-tabs .nav-link { 
        color: #bbb !important; 
        border: none !important; 
        margin-right: 2px; 
        font-size: 0.75rem !important; /* Reduced Font Size */
        padding: 0 10px !important; /* Reduced Padding */
        height: 35px !important; /* Match container height */
        display: flex;
        align-items: center;
        gap: 5px;
      }
      
      .nav-tabs .nav-link:hover { color: #fff !important; background: rgba(255,255,255,0.05); }
      
      .nav-tabs .nav-link.active { 
        color: #fff !important; 
        background-color: #190028 !important; 
        border-bottom: 2px solid #00FF85 !important;
        font-weight: bold;
      }
      
      .nav-tabs::-webkit-scrollbar { height: 0px; background: transparent; }
      /* --- UPDATED TAB STYLING END --- */
      
      .tab-content { background-color: #190028; padding: 0; height: 100%; }
    "))),
    
    # --- MAIN LAYOUT ---
    layout_columns(
      col_widths = c(5, 7),
      fill = FALSE,
      gap = "25px",
      
      # --- COLUMN 1: SIDEBAR ---
      div(class = "sidebar-panel",
          # A. Header
          div(class = "sidebar-header",
              div(style="font-size: 1.2rem; font-weight: bold; color: white;", uiOutput(ns("league_title_display"))),
              div(class = "sidebar-timestamp", uiOutput(ns("refresh_timestamp")))
          ),
          # B. KPI Stack
          div(class = "kpi-stack",
              div(class = "kpi-card",
                  div(class = "kpi-left", span(class = "kpi-label", "Current Leader"), span(class = "kpi-value", textOutput(ns("val_leader")))),
                  bs_icon("trophy-fill", class = "kpi-icon")
              ),
              div(class = "kpi-card accent-pink",
                  div(class = "kpi-left", span(class = "kpi-label", "Avg League Score"), span(class = "kpi-value", uiOutput(ns("val_avg")))),
                  bs_icon("bar-chart-line", class = "kpi-icon")
              ),
              div(class = "kpi-card accent-cyan",
                  div(class = "kpi-left", span(class = "kpi-label", "Rivals Tracked"), span(class = "kpi-value", textOutput(ns("val_count")))),
                  bs_icon("people-fill", class = "kpi-icon")
              )
          ),
          # C. Live Standings
          div(style = "margin-top: 15px; font-size: 1rem; font-weight: bold; color: #00FF85; border-bottom: 1px solid #333; padding-bottom: 5px;", 
              bs_icon("table"), " Live Standings"),
          div(class = "standings-wrapper",
              gt_output(ns("standings_table"))
          )
      ),
      
      # --- COLUMN 2: MAIN ANALYSIS TABS ---
      div(class = "bslib-card", style = "height: 600px;",
          navset_card_tab(
            full_screen = TRUE,
            height = "100%", 
            
            # Tab 1: Consistency
            nav_panel(title = "Consistency", icon = bs_icon("grid-3x3"),
                      div(style = "padding: 10px; height: 100%; display: flex; flex-direction: column;",
                          div(style = "text-align: right; margin-bottom: 15px;",
                              radioButtons(ns("heatmap_metric"), NULL, choices = c("Raw Points", "Vs League Average"), selected = "Vs League Average", inline = TRUE)
                          ),
                          div(style = "flex-grow: 1;", plotlyOutput(ns("race_chart"), height = "100%"))
                      )
            ),
            
            # Tab 2: Transfers
            nav_panel(title = "Transfers", icon = bs_icon("arrow-left-right"),
                      div(style = "padding: 10px; height: 100%;", plotlyOutput(ns("transfers_chart"), height = "100%"))
            ),
            
            # Tab 3: Ownership
            nav_panel(title = "Ownership", icon = bs_icon("bar-chart-steps"),
                      div(style = "padding: 10px; height: 100%;", plotlyOutput(ns("ownership_chart"), height = "100%"))
            ),
            
            # Tab 4: Similarity & Structure
            nav_panel(title = "Similarity", icon = bs_icon("intersect"),
                      div(style = "display: flex; height: 100%; padding: 15px; gap: 20px;",
                          div(style = "flex: 1; display: flex; flex-direction: column;",
                              div(style = "font-size: 0.9rem; font-weight: bold; color: #00FF85; margin-bottom: 10px;", "Squad Similarity Matrix"),
                              div(style = "flex-grow: 1;", plotlyOutput(ns("similarity_chart"), height = "100%"))
                          ),
                          div(style = "flex: 1; display: flex; flex-direction: column; border-left: 1px solid #333; padding-left: 20px;",
                              div(style = "font-size: 0.9rem; font-weight: bold; color: #00FF85; margin-bottom: 10px;", "Squad Structure Profile"),
                              div(style = "flex-grow: 1;", plotlyOutput(ns("structure_radar"), height = "100%"))
                          )
                      )
            ),
            
            # Tab 5: Decisions
            nav_panel(title = "Decisions", icon = bs_icon("lightning-charge-fill"),
                      div(style = "display: flex; flex-direction: column; height: 100%; gap: 15px; padding: 20px;",
                          # TOP: Chart
                          div(style = "height: 40%; border-bottom: 1px solid #444; padding-bottom: 10px;", 
                              plotlyOutput(ns("season_cap_chart"), height = "100%")
                          ),
                          # BOTTOM: Table with Styled Selector
                          div(style = "height: 60%; display: flex; flex-direction: column;",
                              div(style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
                                  span("Select Gameweek:", style="color: #888; margin-right: 10px; font-size: 0.8rem;"),
                                  div(style = "width: 140px;", 
                                      selectInput(ns("gw_selector"), label = NULL, choices = NULL)
                                  )
                              ),
                              div(style = "flex-grow: 1; overflow-y: auto;", gt_output(ns("gw_cap_table")))
                          )
                      )
            ),
            
            # Tab 6: Regret
            nav_panel(title = "Regret", icon = bs_icon("emoji-frown-fill"),
                      div(style = "display: flex; flex-direction: column; height: 100%; padding: 20px;",
                          div(style = "height: 40%; border-bottom: 1px solid #444; padding-bottom: 10px;", plotlyOutput(ns("bench_chart"), height = "100%")),
                          div(style = "height: 60%; padding-top: 15px;", plotlyOutput(ns("bench_timeline_chart"), height = "100%"))
                      )
            )
          )
      )
    )
  )
}