
# --- IMPORT MODULES ---
source("modules/radar_chart.R")
source("modules/sunburst_chart.R")
source("modules/player_table.R")
source("modules/fixture_ticker.R")
source("modules/lollipop_chart.R") 

# --- UI COMPONENT ---
playerExplorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- CSS Styles ---
    tags$head(tags$style(HTML("
      /* KPI Boxes */
      .value-box { 
        height: 100px !important; 
        border: 1px solid #38003C;
        background: linear-gradient(145deg, #2A0040, #190028);
      }
      .value-box .value { font-size: 1.8rem !important; font-weight: 700; color: #fff; }
      
      /* Cards */
      .card { 
        border: 1px solid #38003C; 
        background-color: #190028; 
        margin-bottom: 15px; 
      }
      .card-header { 
        padding: 8px 15px !important; 
        background-color: #2A0040; 
        border-bottom: 1px solid #38003C; 
        font-size: 0.95rem;
        font-weight: bold; 
        color: #fff;
      }
      
      /* Ticker Scrollbar */
      .gt_table_container { overflow-y: auto; }
      
      /* Detailed Table Specifics */
      #explorer_module-analytics_table .gt_table_container {
        overflow-y: hidden !important; 
        height: auto !important;
      }
    "))),
    
    # --- ROW 1: KPIs ---
    layout_columns(
      col_widths = c(4, 4, 4), fill = FALSE,
      value_box(title = "Players Found", value = textOutput(ns("kpi_count")), showcase = bs_icon("people-fill"), theme = "primary"),
      value_box(title = "Avg Price", value = textOutput(ns("kpi_price")), showcase = bs_icon("currency-pound"), theme = "secondary"),
      value_box(title = "Top Form", value = textOutput(ns("kpi_form")), showcase = bs_icon("graph-up-arrow"), theme = "primary")
    ),
    
    # --- ROW 2: FIXTURES + PROFILE ---
    layout_columns(
      col_widths = c(5, 7), 
      fill = FALSE,
      card(height = "420px", card_header("Fixture Matrix (Next 5)"), div(style = "height: 380px; overflow-y: auto;", gt_output(ns("integrated_ticker")))),
      card(height = "420px", card_header("Cohort Profile"), plotlyOutput(ns("chart_radar"), height = "380px"))
    ),
    
    # --- ROW 3: VALUE + HIERARCHY ---
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      card(height = "400px", card_header("Value Gems (ROI)"), plotlyOutput(ns("chart_diverging"), height = "360px")),
      card(height = "400px", card_header("Points Distribution"), plotlyOutput(ns("chart_sunburst"), height = "360px"))
    ),
    
    # --- ROW 4: TABLE (LEFT) + GOAL SCORERS (RIGHT) ---
    layout_columns(
      col_widths = c(7, 5), # Split space 70% / 30%
      fill = FALSE,
      
      # LEFT: Detailed Table
      card(
        fill = FALSE,
        height = "auto",
        min_height = "350px",
        card_header("Detailed Scout Report"),
        gt_output(ns("analytics_table"))
      ),
      
      # RIGHT: Lollipop Chart (Top Goal Scorers)
      card(
        fill = FALSE,
        height = "auto",
        min_height = "350px",
        card_header("Top Goal Scorers"),
        plotlyOutput(ns("chart_lollipop"), height = "300px")
      )
    )
  )
}

# --- SERVER COMPONENT ---
playerExplorerServer <- function(id, fpl_data, team_filter, position_filter, status_filter, price_filter) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Filter Logic
    filtered_data <- reactive({
      df <- fpl_data
      if (team_filter() != "All Teams") df <- df %>% filter(team_name == team_filter())
      if (position_filter() != "All Positions") df <- df %>% filter(position == position_filter())
      if (status_filter() == "Available Only") df <- df %>% filter(chance_of_playing >= 75 | is.na(chance_of_playing))
      else if (status_filter() == "Doubts/Injured") df <- df %>% filter(chance_of_playing < 100)
      
      df %>% 
        filter(cost <= price_filter()) %>%
        mutate(
          form = as.numeric(form), cost = as.numeric(cost), total_points = as.numeric(total_points),
          selected_by_percent = as.numeric(selected_by_percent),
          expected_goal_involvements = if("expected_goal_involvements" %in% names(.)) as.numeric(expected_goal_involvements) else 0,
          value_ratio = round(total_points / cost, 1),
          player_image = paste0("<img src='", ifelse(is.na(photo_url) | photo_url == "", "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png", photo_url), "' style='height:35px; width:35px; object-fit: cover; border-radius:50%; border: 2px solid #00FF85;'>")
        ) %>% arrange(desc(form)) 
    })
    
    # 2. Render KPIs & Charts
    output$kpi_count <- renderText({ nrow(filtered_data()) })
    output$kpi_price <- renderText({ if(nrow(filtered_data()) > 0) paste0("£", round(mean(filtered_data()$cost, na.rm=TRUE), 1), "m") else "£0.0m" })
    output$kpi_form <- renderText({ if(nrow(filtered_data()) > 0) max(filtered_data()$form, na.rm=TRUE) else "0.0" })
    
    output$chart_radar <- renderPlotly({ req(nrow(filtered_data()) > 0); generate_radar_chart(filtered_data(), fpl_data) })
    output$chart_diverging <- renderPlotly({
      req(nrow(filtered_data()) > 0)
      top_players <- filtered_data() %>% head(15)
      avg_roi <- mean(top_players$value_ratio, na.rm=TRUE)
      plot_data <- top_players %>% mutate(diff = value_ratio - avg_roi, color_flag = ifelse(diff >= 0, "#00FF85", "#E90052")) %>% arrange(diff)
      plot_data$web_name <- factor(plot_data$web_name, levels = plot_data$web_name)
      plot_ly(plot_data, x = ~diff, y = ~web_name, type = 'bar', orientation = 'h', marker = list(color = ~color_flag, line = list(color = "white", width = 0.5)), hovertext = ~paste("Player:", web_name, "<br>ROI Diff:", round(diff, 1)), hoverinfo = "text") %>%
        layout(xaxis = list(title = "ROI", color = "#cccccc"), yaxis = list(title = "", color = "white"), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", margin = list(l=80, r=10, t=10, b=30), font = list(family = "Poppins")) %>% config(displayModeBar = FALSE)
    })
    output$chart_sunburst <- renderPlotly({ req(nrow(filtered_data()) > 0); generate_sunburst_chart(filtered_data()) })
    output$analytics_table <- render_gt({ req(nrow(filtered_data()) > 0); generate_player_table(filtered_data()) })
    
    # 3. RENDER FIXTURE TICKER
    ticker_data <- reactive({ if(!file.exists("data/fixture_ticker.rds")) return(NULL); readRDS("data/fixture_ticker.rds") })
    output$integrated_ticker <- render_gt({
      req(ticker_data())
      df <- ticker_data()
      if(team_filter() != "All Teams") df <- df %>% filter(name == team_filter())
      
      gt_tbl <- df %>% select(name, starts_with("opp_label"), starts_with("difficulty")) %>% gt() %>%
        cols_label(name = "Team", opp_label_1 = "Next", opp_label_2 = "+2", opp_label_3 = "+3", opp_label_4 = "+4", opp_label_5 = "+5") %>%
        cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "name") %>%
        cols_width(name ~ px(100), starts_with("opp") ~ px(70)) %>%
        tab_options(table.background.color = "#190028", table.font.color = "#FFFFFF", table.font.size = px(12), data_row.padding = px(6), column_labels.background.color = "#2A0040", column_labels.font.weight = "bold", column_labels.border.bottom.color = "#00FF85", table.border.top.color = "#444444", table_body.hlines.color = "#444444", table_body.vlines.color = "#333333") %>%
        opt_table_font(font = "Courier New") %>% tab_style(style = cell_borders(sides = "all", color = "#444444", weight = px(1)), locations = cells_body()) %>% tab_style(style = cell_text(color = "#00FF85", weight = "bold"), locations = cells_body(columns = name))
      
      for(i in 1:5) {
        diff_col <- paste0("difficulty_", i); label_col <- paste0("opp_label_", i)
        gt_tbl <- gt_tbl %>% tab_style(style = list(cell_fill(color = "#00FF85"), cell_text(color = "#190028", weight = "bold")), locations = cells_body(columns = all_of(label_col), rows = .data[[diff_col]] <= 2)) %>%
          tab_style(style = list(cell_fill(color = "#EBEBE4"), cell_text(color = "#190028", weight = "bold")), locations = cells_body(columns = all_of(label_col), rows = .data[[diff_col]] == 3)) %>%
          tab_style(style = list(cell_fill(color = "#E90052"), cell_text(color = "#FFFFFF", weight = "bold")), locations = cells_body(columns = all_of(label_col), rows = .data[[diff_col]] >= 4))
      }
      gt_tbl %>% cols_hide(columns = starts_with("difficulty"))
    })
    
    # 4. RENDER LOLLIPOP CHART (Imported Logic)
    output$chart_lollipop <- renderPlotly({
      req(nrow(filtered_data()) > 0)
      generate_lollipop_chart(filtered_data())
    })
  })
}