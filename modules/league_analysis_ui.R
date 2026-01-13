# modules/league_analysis_ui.R

leagueAnalysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    includeCSS("www/league_styles.css"),
    
    # 1. STATUS BAR
    div(class = "input-bar",
        style = "display: flex; align-items: center; justify-content: space-between; padding: 10px 20px; background: #2A0040; border-radius: 8px; margin-bottom: 20px;",
        div(style = "flex-grow: 1;", uiOutput(ns("league_title_display"))),
        div(style = "text-align: right;", uiOutput(ns("refresh_timestamp")))
    ),
    
    # 2. KPI METRICS
    layout_columns(
      col_widths = c(4, 4, 4), fill = FALSE,
      value_box(title = "League Leader", value = textOutput(ns("val_leader")), showcase = bs_icon("trophy-fill"), theme = "primary"),
      value_box(title = "Average Score", value = uiOutput(ns("val_avg")), showcase = bs_icon("bar-chart-line"), theme = "secondary"),
      value_box(title = "Rivals Tracked", value = textOutput(ns("val_count")), showcase = bs_icon("people-fill"), theme = "primary")
    ),
    
    # 3. VISUALIZATIONS LAYOUT
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;", # ALIGNMENT FIX
      
      # --- LEFT COLUMN (Standings + Structure) ---
      div(style = "flex: 0 0 35%; max-width: 35%;", 
          
          # A. Live Standings
          card(
            class = "league-card", full_screen = TRUE, height = "auto", fill = FALSE, style = "margin-bottom: 20px;",
            card_header(span(bs_icon("table"), " Live Standings")),
            gt_output(ns("standings_table"))
          ),
          
          # B. Positional Structure Radar (No Filters, Optimized Height)
          card(
            class = "league-card", full_screen = TRUE, height = "500px", fill = FALSE,
            card_header(span(bs_icon("diagram-3"), " Squad Structure")),
            plotlyOutput(ns("structure_radar"), height = "450px") 
          )
      ),
      
      # --- RIGHT COLUMN (Tabbed Analysis) ---
      div(style = "flex: 1;", 
          navset_card_tab(
            full_screen = TRUE, height = "550px", 
            wrapper = function(...) card(class = "league-card", ...), 
            
            # 1. Consistency
            nav_panel(
              title = "Consistency", 
              icon = bs_icon("grid-3x3"), 
              div(style = "padding: 10px; display: flex; justify-content: flex-end;",
                  radioButtons(ns("heatmap_metric"), NULL, choices = c("Raw Points", "Vs League Average"), selected = "Vs League Average", inline = TRUE)
              ),
              plotlyOutput(ns("race_chart"), height = "100%")
            ),
            
            # 2. Transfers
            nav_panel(
              title = "Transfers", 
              icon = bs_icon("arrow-left-right"),
              plotlyOutput(ns("transfers_chart"), height = "100%")
            ),
            
            # 3. Ownership
            nav_panel(title = "Ownership", icon = bs_icon("bar-chart-steps"), plotlyOutput(ns("ownership_chart"), height = "100%")),
            
            # 4. Similarity
            nav_panel(title = "Similarity", icon = bs_icon("intersect"), plotlyOutput(ns("similarity_chart"), height = "100%")),
            
            # 5. Decisions
            nav_panel(
              title = "Decisions", 
              icon = bs_icon("lightning-charge-fill"),
              div(style = "height: 45%; padding-bottom: 10px; border-bottom: 1px solid #444;", plotlyOutput(ns("season_cap_chart"), height = "100%")),
              div(style = "height: 55%; position: relative;", 
                  div(style = "position: absolute; top: 0px; right: 5px; width: 110px; z-index: 20;", selectInput(ns("gw_selector"), label = NULL, choices = NULL, width = "100%")),
                  div(style = "height: 100%; overflow-y: auto;", gt_output(ns("gw_cap_table")))
              )
            ),
            
            # 6. Regret (Split View: Meter + Timeline)
            nav_panel(
              title = "Regret", 
              icon = bs_icon("emoji-frown-fill"),
              
              # Top: The Summary Meter
              div(style = "height: 45%; padding-bottom: 10px; border-bottom: 1px solid #444;",
                  plotlyOutput(ns("bench_chart"), height = "100%")
              ),
              
              # Bottom: The Timeline of Pain
              div(style = "height: 55%; padding-top: 10px;",
                  plotlyOutput(ns("bench_timeline_chart"), height = "100%")
              )
            )
          )
      )
    )
  )
}