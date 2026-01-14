# modules/league_analysis_server.R

# Ensure chart components are loaded
source("modules/components/league_chart.R")
source("modules/components/structure_chart.R") 

leagueAnalysisServer <- function(id, fpl_data) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize Data Store
    data_store <- reactiveValues(
      standings = NULL, 
      history = NULL, 
      picks = NULL, 
      transfers = NULL, 
      league_name = "League Analysis", 
      updated = NULL
    )
    
    # 1. LOAD DATA ON STARTUP
    observeEvent(TRUE, {
      target_id <- "1167592"
      
      # A. Load Standings
      res_s <- get_league_standings_rds(target_id)
      
      if (!is.null(res_s)) {
        data_store$standings <- res_s$data
        data_store$league_name <- res_s$league_name
        data_store$updated <- res_s$updated
        
        # B. Load History
        res_h <- get_league_history_rds(target_id, res_s$data)
        if (!is.null(res_h)) data_store$history <- res_h$data
        
        if (!is.null(res_h)) {
          # C. Load Picks (Squads)
          res_p <- get_league_picks_rds(target_id, res_s$data, res_h$data)
          if (!is.null(res_p)) {
            data_store$picks <- res_p$data
            
            # Populate GW Selector
            available_gws <- sort(unique(data_store$picks$event), decreasing = TRUE)
            updateSelectInput(session, "gw_selector", 
                              choices = available_gws, 
                              selected = available_gws[1]) 
          }
          
          # D. Load Transfers (Pre-fetched RDS)
          res_t <- get_league_transfers_rds(target_id, res_s$data)
          if (!is.null(res_t)) {
            data_store$transfers <- res_t$data
          }
        }
      }
    }, once = TRUE)
    
    # 2. KPI Logic
    avg_trend_data <- reactive({
      req(data_store$history)
      trend <- data_store$history %>%
        group_by(event) %>%
        summarise(gw_avg = mean(total_points - lag(total_points, default = 0)), .groups = "drop") %>%
        filter(!is.na(gw_avg)) %>% slice_tail(n = 5)
      current <- last(trend$gw_avg); prev <- mean(head(trend$gw_avg, -1))
      list(val = round(current, 1), diff = round(current - prev, 1), status = if (current >= prev) "up" else "down")
    })
    
    # 3. Basic UI Outputs
    output$league_title_display <- renderUI({ tags$h4(data_store$league_name, style = "color:#00FF85;font-weight:bold;") })
    output$refresh_timestamp <- renderUI({ req(data_store$updated); tags$div(paste("Local Sync:", format(data_store$updated, "%H:%M"))) })
    output$val_leader <- renderText({ req(data_store$standings); data_store$standings$player_name[1] })
    output$val_count <- renderText({ req(data_store$standings); nrow(data_store$standings) })
    
    output$val_avg <- renderUI({
      req(data_store$standings, avg_trend_data())
      tr <- avg_trend_data(); color <- if(tr$status == "up") "#00FF85" else "#E90052"
      icon <- if(tr$status == "up") "arrow-up-short" else "arrow-down-short"
      tagList(div(prettyNum(round(mean(data_store$standings$points), 0), big.mark=","), style = "font-size: 1.8rem; line-height: 1;"), div(class = "trend-label", style = paste0("color: ", color, ";"), bs_icon(icon), paste0(abs(tr$diff), " vs last 5 avg")))
    })
    
    output$standings_table <- render_gt({
      req(data_store$standings, data_store$history)
      render_league_standings_table(data_store$standings, data_store$history)
    })
    
    # --- 4. CHARTS ---
    
    # A. Consistency Heatmap
    output$race_chart <- renderPlotly({
      req(data_store$history, input$heatmap_metric)
      render_league_performance_heatmap(as.data.frame(data_store$history), input$heatmap_metric)
    })
    
    # B. Structure Radar 
    output$structure_radar <- renderPlotly({
      req(data_store$picks)
      # Ensure structure_chart.R is sourced or function is available
      if(exists("render_position_radar")) {
        render_position_radar(as.data.frame(data_store$picks), fpl_data)
      } else {
        return(NULL)
      }
    })
    
    # C. Transfers Efficiency
    output$transfers_chart <- renderPlotly({
      req(data_store$transfers) 
      render_transfer_efficiency(as.data.frame(data_store$transfers), fpl_data)
    })
    
    # D. Ownership
    output$ownership_chart <- renderPlotly({ 
      req(data_store$picks)
      render_ownership_bar(as.data.frame(data_store$picks), fpl_data) 
    })
    
    # E. Similarity (FIXED FUNCTION NAME)
    output$similarity_chart <- renderPlotly({ 
      req(data_store$picks)
       render_similarity_heatmap(as.data.frame(data_store$picks)) 
    })
    
    # F. Decisions (Captaincy)
    output$season_cap_chart <- renderPlotly({ req(data_store$picks); render_season_captaincy(as.data.frame(data_store$picks), fpl_data) })
    output$gw_cap_table <- render_gt({ req(data_store$picks, input$gw_selector); render_gw_captaincy_table(as.data.frame(data_store$picks), fpl_data, input$gw_selector) })
    
    # G. Regret (Bench Meter + Timeline)
    output$bench_chart <- renderPlotly({ req(data_store$picks); render_bench_efficiency(as.data.frame(data_store$picks), fpl_data) })
    
    output$bench_timeline_chart <- renderPlotly({
      req(data_store$picks)
      render_bench_timeline(as.data.frame(data_store$picks), fpl_data)
    })
  })
}