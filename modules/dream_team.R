library(shiny)
library(dplyr)
library(gt)
library(bslib)
library(bsicons)
library(plotly)

# --- LOAD DEPENDENCIES ---
# need the optimizer to generate the team
if(file.exists("R/03_optimizer.R")) source("R/03_optimizer.R")
# need the manager to SAVE the result to CSV
if(file.exists("R/06_prediction_manager.R")) source("R/06_prediction_manager.R")

# --- UI COMPONENT ---
dreamTeamUI <- function(id) {
  ns <- NS(id)
  
  div(
    style = "padding-bottom: 50px;", 
    tagList(
      # --- CSS ---
      tags$head(tags$style(HTML("
        .pro-card { border: 1px solid #38003C; background-color: #190028; margin-bottom: 25px; box-shadow: 0px 4px 15px rgba(0,0,0,0.3); }
        .card-header { background-color: #2A0040 !important; border-bottom: 1px solid #38003C !important; color: white !important; font-weight: bold; }
        .control-bar { background: #2A0040; border: 1px solid #38003C; padding: 15px; border-radius: 8px; margin-bottom: 20px; display: flex; align-items: center; gap: 20px; }
        .captain-hero-box { background: radial-gradient(circle at center, #2A0040 0%, #190028 100%); border: 1px solid #00FF85; border-radius: 12px; text-align: center; padding: 20px; box-shadow: 0 0 20px rgba(0, 255, 133, 0.15); display: flex; flex-direction: column; justify-content: center; align-items: center; min-height: 250px; }
        .matrix-score-big { font-size: 2.5rem; font-weight: 800; color: #00FF85; text-shadow: 0 0 10px rgba(0, 255, 133, 0.4); line-height: 1; margin-top: 10px; }
        .matrix-label { color: #888; text-transform: uppercase; font-size: 0.75rem; letter-spacing: 1.5px; margin-top: 5px; }
        .algo-text { color: #aaa; font-size: 0.85rem; font-style: italic; float: right; font-weight: normal;}
      "))),
      
      # --- SECTION 0: CONTROLS ---
      div(class = "control-bar",
          bs_icon("sliders", size = "1.5rem", style = "color: #00FF85;"),
          div(style = "flex-grow: 1;",
              sliderInput(ns("budget_slider"), "Team Budget (£m):", min = 80, max = 110, value = 100, step = 0.5, width = "100%", ticks = FALSE)
          ),
          div(style = "color: #ccc; font-size: 0.9rem;", "Strategy: 1-3-4-3 LP Optimization")
      ),
      
      # --- SECTION 1: KPI GAUGES ---
      layout_columns(
        col_widths = c(4, 4, 4), 
        fill = FALSE, 
        card(class = "pro-card", fill = FALSE, height = "200px", card_header("Projected Points"), plotlyOutput(ns("gauge_points"), height = "140px")),
        card(class = "pro-card", fill = FALSE, height = "200px", card_header("Team Cost (£m)"), plotlyOutput(ns("gauge_cost"), height = "140px")),
        card(class = "pro-card", fill = FALSE, height = "200px", card_header("Avg Team Form"), plotlyOutput(ns("gauge_form"), height = "140px"))
      ),
      
      # --- SECTION 2: CAPTAINCY MATRIX ---
      card(
        class = "pro-card",
        fill = FALSE, 
        min_height = "350px", 
        card_header(
          div(style="display:flex; justify-content:space-between; align-items:center;",
              uiOutput(ns("captain_header_label")),
              span(class="algo-text", "Metric: Form × (5 - Fixture Difficulty)")
          )
        ),
        layout_columns(
          col_widths = c(8, 4),
          fill = FALSE,
          div(style = "padding-right: 10px;", gt_output(ns("captain_table"))),
          div(class = "captain-hero-box",
              h6("⭐ SQUAD CAPTAIN", style="color:white; letter-spacing: 2px; margin-bottom: 15px;"),
              uiOutput(ns("top_captain_image")),
              h3(textOutput(ns("top_captain_name")), style="color: #fff; font-weight: bold; margin: 10px 0 0 0;"),
              div(textOutput(ns("top_captain_opp")), style="color: #04F5FF; font-weight: 500; font-size: 1.1rem;"),
              div(textOutput(ns("top_captain_score")), class = "matrix-score-big"),
              div("Matrix Score", class="matrix-label")
          )
        )
      ),
      
      # --- SECTION 3: THE OPTIMIZED TABLE ---
      card(
        class = "pro-card",
        fill = FALSE, 
        min_height = "600px", 
        card_header(uiOutput(ns("team_header_label"))),
        gt_output(ns("dream_team_table"))
      )
    )
  )
}

# --- SERVER COMPONENT ---
dreamTeamServer <- function(id, fpl_data, budget_input) {
  moduleServer(id, function(input, output, session) {
    
    # --- 0. DETERMINE CURRENT GAMEWEEK ---
    next_gw_label <- reactive({
      gw_num <- "Next"
      if(file.exists("data/fpl_raw.rds")) {
        raw <- readRDS("data/fpl_raw.rds")
        if("events" %in% names(raw)) {
          next_event <- raw$events %>% filter(finished == FALSE) %>% head(1)
          if(nrow(next_event) > 0) gw_num <- next_event$id
        }
      }
      return(gw_num)
    })
    
    # --- 1. RENDER DYNAMIC HEADERS (Removed "AI") ---
    output$team_header_label <- renderUI({
      span(bs_icon("robot"), paste0(" Optimized Starting XI (Gameweek ", next_gw_label(), ")"))
    })
    
    output$captain_header_label <- renderUI({
      span(bs_icon("rocket-takeoff-fill"), paste0(" Captaincy Recommendation (GW ", next_gw_label(), ")"))
    })
    
    # --- 2. CALL THE OPTIMIZER ---
    optimized_team <- reactive({
      req(fpl_data, input$budget_slider)
      if(!exists("optimize_team")) return(data.frame())
      optimize_team(fpl_data, budget = input$budget_slider)
    })
    
    # --- 3. AUTO-SAVE PREDICTION (This fixes the missing CSV issue) ---
    observe({
      # Wait until a team is generated
      req(optimized_team())
      
      # Get the current gameweek
      gw <- next_gw_label()
      
      # Ensure it's a valid number before saving
      if (is.numeric(gw) || (is.character(gw) && grepl("^[0-9]+$", gw))) {
        # CALL THE MANAGER SAVE FUNCTION
        if(exists("save_prediction")) {
          save_prediction(optimized_team(), as.numeric(gw))
        } else {
          print("⚠️ Warning: save_prediction() function not found.")
        }
      }
    })
    
    # --- 4. CALCULATE CAPTAINCY ---
    squad_with_matrix <- reactive({
      req(optimized_team())
      if(nrow(optimized_team()) == 0) return(NULL)
      
      df_squad <- optimized_team()
      if(!file.exists("data/fixture_ticker.rds")) return(df_squad)
      ticker <- readRDS("data/fixture_ticker.rds")
      
      df_squad %>%
        left_join(ticker %>% select(name, difficulty_1, opp_label_1), by = c("team_name" = "name")) %>%
        mutate(
          fixture_multiplier = 5 - as.numeric(difficulty_1),
          captain_score = as.numeric(form) * fixture_multiplier,
          player_image_tbl = paste0("<img src='", ifelse(is.na(photo_url) | photo_url == "", "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png", photo_url), "' style='height:30px; width:30px; object-fit: cover; border-radius:50%; border: 1px solid #00FF85;'>")
        ) %>%
        arrange(desc(captain_score))
    })
    
    # --- 5. RENDER OUTPUTS (Gauges & Tables) ---
    render_gauge <- function(value, min_val, max_val, color_hex, title_suffix = "") {
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)), value = value,
        title = list(text = ""), type = "indicator", mode = "gauge+number",
        gauge = list(
          axis = list(range = list(min_val, max_val), tickwidth = 1, tickcolor = "#888"),
          bar = list(color = color_hex), bgcolor = "rgba(0,0,0,0)", borderwidth = 2, bordercolor = "#333",
          steps = list(list(range = c(min_val, max_val), color = "#2A0040"))
        ),
        number = list(font = list(color = "white", size = 20), suffix = title_suffix)
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", margin = list(l=20, r=20, t=10, b=10), font = list(family = "Poppins", color = "white")) %>% config(displayModeBar = FALSE)
    }
    
    output$gauge_points <- renderPlotly({ req(squad_with_matrix()); val <- sum(squad_with_matrix()$predicted_points, na.rm=TRUE); render_gauge(val, 0, 100, "#00FF85") })
    output$gauge_cost <- renderPlotly({ req(squad_with_matrix()); val <- sum(squad_with_matrix()$cost, na.rm=TRUE); render_gauge(val, 0, input$budget_slider, "#E90052", "m") })
    output$gauge_form <- renderPlotly({ req(squad_with_matrix()); val <- round(mean(as.numeric(squad_with_matrix()$form), na.rm=TRUE), 1); render_gauge(val, 0, 10, "#04F5FF") })
    
    output$captain_table <- render_gt({
      req(squad_with_matrix())
      squad_with_matrix() %>% head(5) %>% select(player_image_tbl, web_name, team_name, form, opp_label_1, captain_score) %>% gt() %>%
        fmt_markdown(columns = player_image_tbl) %>%
        cols_label(player_image_tbl="", web_name="Player", team_name="Team", form="Form", opp_label_1="Opponent", captain_score="Score") %>%
        cols_align(align="center", columns=everything()) %>% cols_align(align="left", columns="web_name") %>%
        tab_options(table.width=pct(100), table.background.color="#190028", table.font.color="white", table.font.size=px(13), column_labels.background.color="#2A0040", column_labels.font.weight="bold", column_labels.border.bottom.color="#00FF85", table.border.top.color="#444", table_body.hlines.color="#333", data_row.padding=px(8)) %>%
        tab_style(style=list(cell_text(weight="bold", color="#00FF85", size=px(16))), locations=cells_body(columns=captain_score)) %>% opt_table_font(font="Poppins")
    })
    
    top_pick <- reactive({ req(squad_with_matrix()); squad_with_matrix() %>% head(1) })
    output$top_captain_name <- renderText({ top_pick()$web_name })
    output$top_captain_opp <- renderText({ paste("vs", top_pick()$opp_label_1) })
    output$top_captain_score <- renderText({ round(top_pick()$captain_score, 1) })
    output$top_captain_image <- renderUI({ req(top_pick()); url <- top_pick()$photo_url; if(is.na(url)|url=="") url <- "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png"; tags$img(src=url, style="width: 110px; border-radius: 50%; border: 4px solid #00FF85; box-shadow: 0 0 25px rgba(0,255,133,0.4); background:#190028;") })
    
    output$dream_team_table <- render_gt({
      req(squad_with_matrix())
      squad_sorted <- squad_with_matrix() %>%
        mutate(pos_rank = case_when(position == "GKP" ~ 1, position == "DEF" ~ 2, position == "MID" ~ 3, position == "FWD" ~ 4, TRUE ~ 5)) %>%
        arrange(pos_rank, desc(form))
      
      squad_sorted %>% 
        select(position, player_image_tbl, web_name, team_name, cost, form, predicted_points) %>% 
        gt() %>%
        fmt_markdown(columns = player_image_tbl) %>%
        cols_label(position="Pos", player_image_tbl="", web_name="Player", team_name="Team", cost="£", form="Form", predicted_points="xPts") %>%
        cols_align(align="center", columns=everything()) %>% cols_align(align="left", columns="web_name") %>%
        tab_options(table.width=pct(100), table.background.color="#190028", table.font.color="#ddd", table.font.size=px(12), column_labels.background.color="#2A0040", column_labels.font.weight="bold", column_labels.border.bottom.color="#04F5FF", table.border.top.color="#444", table_body.hlines.color="#333", data_row.padding=px(6)) %>%
        opt_table_font(font="Poppins") %>% data_color(columns=form, palette=c("#2A0040", "#E90052"))
    })
  })
}