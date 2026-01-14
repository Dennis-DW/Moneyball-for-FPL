
# --- LOAD DEPENDENCIES ---
if(file.exists("R/03_optimizer.R")) source("R/03_optimizer.R")
if(file.exists("R/06_prediction_manager.R")) source("R/06_prediction_manager.R")

# --- UI COMPONENT ---
dreamTeamUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- CSS STYLING ---
    tags$head(tags$style(HTML("
      /* 1. COMPACT TOP BAR */
      .control-panel {
        background: linear-gradient(90deg, #2A0040 0%, #190028 100%);
        border: 1px solid #38003C;
        border-radius: 12px;
        padding: 0 15px;
        margin-bottom: 5px; /* Minimal margin */
        display: flex;
        align-items: center;
        gap: 15px;
        height: 65px; /* Ultra Slim Height */
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      }
      
      /* New Digital Budget Input Styling */
      .budget-input-wrapper {
        flex: 1;
        border-right: 1px solid #444;
        padding-right: 20px;
        display: flex;
        align-items: center;
        gap: 15px;
      }
      
      .budget-label {
        color: #00FF85;
        font-weight: bold;
        font-size: 0.9rem;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* Customizing the Numeric Input */
      .neon-input input {
        background-color: #190028 !important;
        border: 2px solid #00FF85 !important;
        color: #ffffff !important;
        font-weight: 800 !important;
        font-size: 1.2rem !important;
        text-align: center !important;
        border-radius: 6px !important;
        height: 40px !important;
        width: 100px !important;
        box-shadow: 0 0 10px rgba(0, 255, 133, 0.2) !important;
      }
      .neon-input input:focus {
        box-shadow: 0 0 15px rgba(0, 255, 133, 0.5) !important;
        outline: none !important;
      }

      /* Micro-Gauges */
      .gauge-section {
        display: flex;
        gap: 10px;
        align-items: center;
        height: 100%;
      }
      .mini-gauge-wrapper {
        text-align: center;
        width: 90px;
        display: flex; 
        flex-direction: column; 
        align-items: center;
        justify-content: center;
      }
      .mini-gauge-label {
        color: #888; font-size: 0.55rem; text-transform: uppercase; margin-top: -5px; font-weight: bold;
      }

      /* 2. COMPACT CAPTAINCY */
      .captain-hero {
        background: radial-gradient(circle at center, #2A0040 0%, #190028 100%);
        border: 1px solid #00FF85;
        border-radius: 8px;
        padding: 5px;
        text-align: center;
        margin-bottom: 5px;
        height: 180px; 
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
      .captain-score-large { font-size: 1.4rem; font-weight: 800; color: #00FF85; line-height: 1; margin-top: 2px; }
      
      /* 3. GENERAL CARDS */
      .card { border: 1px solid #38003C; background-color: #190028; margin-bottom: 0px; border-radius: 8px; }
      .card-header { background-color: #2A0040; color: #fff; font-weight: bold; border-bottom: 1px solid #38003C; padding: 4px 10px; font-size: 0.8rem; }
    "))),
    
    # --- ROW 1: CONTROLS (Digital Input) ---
    div(class = "control-panel",
        # LEFT: Digital Budget Input
        div(class = "budget-input-wrapper",
            span(bs_icon("wallet2"), class="budget-label", " Team Budget (£m)"),
            div(class = "neon-input",
                numericInput(ns("budget_input"), label = NULL, value = 100, min = 80, max = 200, step = 0.1)
            )
        ),
        
        # RIGHT: Micro-Gauges
        div(class = "gauge-section",
            div(class = "mini-gauge-wrapper",
                plotlyOutput(ns("gauge_points"), height = "45px", width = "80px"),
                div(class = "mini-gauge-label", "Points")
            ),
            div(class = "mini-gauge-wrapper",
                plotlyOutput(ns("gauge_cost"), height = "45px", width = "80px"),
                div(class = "mini-gauge-label", "Cost")
            ),
            div(class = "mini-gauge-wrapper",
                plotlyOutput(ns("gauge_form"), height = "45px", width = "80px"),
                div(class = "mini-gauge-label", "Form")
            )
        )
    ),
    
    # --- ROW 2: MAIN CONTENT  ---
    layout_columns(
      col_widths = c(8, 4),
      fill = FALSE,
      gap = "8px",
      
      # LEFT: OPTIMIZED TEAM TABLE (Compressed)
      card(
        height = "400px", 
        card_header(uiOutput(ns("team_header_label"))),
        gt_output(ns("dream_team_table"))
      ),
      
      # RIGHT: CAPTAINCY DASHBOARD
      div(
        div(class = "captain-hero",
            div(style="color: #bbb; letter-spacing: 1px; font-size: 0.6rem; margin-bottom: 3px;", "★ SQUAD CAPTAIN"),
            uiOutput(ns("top_captain_image")),
            h5(textOutput(ns("top_captain_name")), style="color: #fff; font-weight: bold; margin: 4px 0 0 0; font-size: 0.9rem;"),
            div(textOutput(ns("top_captain_opp")), style="color: #04F5FF; font-size: 0.75rem; margin-bottom: 2px;"),
            div(textOutput(ns("top_captain_score")), class = "captain-score-large"),
            div("Matrix Score", style="font-size: 0.5rem; color: #666; text-transform: uppercase;")
        ),
        card(
          height = "255px", # Fits remaining space perfectly
          card_header("Alternative Options"),
          gt_output(ns("captain_table"))
        )
      )
    )
  )
}

# --- SERVER COMPONENT ---
dreamTeamServer <- function(id, fpl_data) { 
  moduleServer(id, function(input, output, session) {
    
    # 1. Helpers
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
    output$team_header_label <- renderUI({ span(bs_icon("robot"), paste0(" Starting XI (GW ", next_gw_label(), ")")) })
    
    # 2. Optimizer Logic (Using Numeric Input)
    optimized_team <- reactive({
      req(fpl_data, input$budget_input) 
      if(!exists("optimize_team")) return(data.frame())
      optimize_team(fpl_data, budget = input$budget_input)
    })
    
    # 3. Data Prep
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
          player_image_tbl = paste0("<img src='", ifelse(is.na(photo_url) | photo_url == "", "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png", photo_url), "' style='height:20px; width:20px; object-fit: cover; border-radius:50%; border: 1px solid #00FF85;'>")
        ) %>%
        arrange(desc(captain_score))
    })
    
    # 4. Micro-Gauges (45px Height)
    render_mini_gauge <- function(value, min_val, max_val, color_hex, suffix = "") {
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)), value = value, type = "indicator", mode = "gauge+number",
        gauge = list(
          axis = list(range = list(min_val, max_val), visible = FALSE),
          bar = list(color = color_hex, thickness = 0.25), 
          bgcolor = "rgba(255,255,255,0.05)", borderwidth = 0
        ),
        number = list(font = list(size = 14, color = "white", family = "Poppins"), suffix = suffix)
      ) %>%
        layout(margin = list(l=5, r=5, t=10, b=0), paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "white")) %>%
        config(displayModeBar = FALSE)
    }
    
    output$gauge_points <- renderPlotly({ req(optimized_team()); render_mini_gauge(round(sum(optimized_team()$predicted_points, na.rm=TRUE), 0), 0, 100, "#00FF85") })
    output$gauge_cost <- renderPlotly({ req(optimized_team()); render_mini_gauge(sum(optimized_team()$cost, na.rm=TRUE), 0, input$budget_input, "#E90052", "m") })
    output$gauge_form <- renderPlotly({ req(optimized_team()); render_mini_gauge(round(mean(as.numeric(optimized_team()$form), na.rm=TRUE), 1), 0, 10, "#04F5FF") })
    
    # 5. Render Tables (Ultra High Density)
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
        tab_options(
          table.width=pct(100), table.background.color="#190028", table.font.color="#ddd", table.font.size=px(10), 
          column_labels.background.color="#2A0040", column_labels.font.weight="bold", column_labels.padding=px(2),
          data_row.padding=px(1), # 1px Padding
          container.overflow.y = "auto"
        ) %>%
        opt_table_font(font="Poppins") %>% data_color(columns=form, palette=c("#2A0040", "#E90052"))
    })
    
    output$captain_table <- render_gt({
      req(squad_with_matrix())
      squad_with_matrix() %>% head(5) %>% select(web_name, opp_label_1, captain_score) %>% gt() %>%
        cols_label(web_name="Player", opp_label_1="vs", captain_score="Score") %>%
        cols_align(align="center", columns=everything()) %>%
        tab_options(table.width=pct(100), table.background.color="#190028", table.font.color="white", table.font.size=px(10), 
                    column_labels.hidden=FALSE, table.border.top.color="transparent", data_row.padding=px(2)) %>%
        tab_style(style=list(cell_text(weight="bold", color="#00FF85")), locations=cells_body(columns=captain_score))
    })
    
    # 6. Captain Hero Image (55px)
    top_pick <- reactive({ req(squad_with_matrix()); squad_with_matrix() %>% head(1) })
    output$top_captain_name <- renderText({ top_pick()$web_name })
    output$top_captain_opp <- renderText({ paste("vs", top_pick()$opp_label_1) })
    output$top_captain_score <- renderText({ round(top_pick()$captain_score, 1) })
    output$top_captain_image <- renderUI({ req(top_pick()); url <- top_pick()$photo_url; if(is.na(url)|url=="") url <- "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png"; tags$img(src=url, style="width: 55px; border-radius: 50%; border: 3px solid #00FF85; box-shadow: 0 0 10px rgba(0,255,133,0.3); background:#190028;") })
  })
}