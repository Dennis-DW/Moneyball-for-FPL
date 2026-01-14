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
    tags$head(tags$style(HTML("
      /* 1. KPI STYLING */
      .kpi-container {
        display: flex;
        flex-direction: column;
        gap: 10px;
        height: 100%;
        justify-content: center;
      }
      
      .value-box-compact { 
        height: 85px !important; 
        background: linear-gradient(145deg, #2A0040, #190028) !important;
        border: 1px solid #38003C !important;
        border-left: 5px solid #00FF85 !important;
        border-radius: 10px !important;
        padding: 0 15px !important;
        display: flex !important;
        flex-direction: column !important;
        justify-content: center !important;
        position: relative;
        box-shadow: 0 4px 10px rgba(0,0,0,0.3);
      }
      
      .value-box-compact .value-title { 
        font-size: 0.7rem !important; color: #bbbbbb !important; 
        text-transform: uppercase; letter-spacing: 1px; margin-bottom: 2px; z-index: 2;
      }
      .value-box-compact .value-number { 
        font-size: 1.6rem !important; font-weight: 800 !important; color: #ffffff !important; 
        line-height: 1; z-index: 2;
      }
      .value-box-compact .value-icon {
        position: absolute; right: -5px; bottom: -10px; font-size: 3.5rem !important; 
        opacity: 0.15; color: #ffffff; z-index: 1;
      }

      /* 2. CARD STYLING */
      .card { 
        border: 1px solid #38003C; background-color: #190028; margin-bottom: 15px; 
        border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      }
      .card-header { 
        padding: 8px 12px !important; background-color: #2A0040; color: #00FF85; 
        font-weight: bold; font-size: 0.95rem; border-bottom: 1px solid #38003C;
      }
    "))),
    
    # --- ROW 1: KPIs + FIXTURES ---
    layout_columns(
      col_widths = c(2, 10),
      fill = FALSE,
      gap = "15px",
      
      # LEFT: KPIs
      div(class = "kpi-container",
          div(class = "value-box-compact",
              div(class = "value-title", "Players Found"),
              div(class = "value-number", textOutput(ns("kpi_count"))),
              div(class = "value-icon", bs_icon("people-fill"))
          ),
          div(class = "value-box-compact", style="border-left-color: #E90052 !important;",
              div(class = "value-title", "Avg Price"),
              div(class = "value-number", textOutput(ns("kpi_price"))),
              div(class = "value-icon", bs_icon("currency-pound"))
          ),
          div(class = "value-box-compact",
              div(class = "value-title", "Top Form"),
              div(class = "value-number", textOutput(ns("kpi_form"))),
              div(class = "value-icon", bs_icon("graph-up-arrow"))
          )
      ),
      
      # RIGHT: Fixture Matrix
      card(
        height = "350px", 
        card_header("Fixture Matrix (Next 5)"),
        div(style = "height: 260px; width: 100%;", DT::dataTableOutput(ns("integrated_ticker")))
      )
    ),
    
    # --- ROW 2: ANALYSIS ROW (Table | Scorers | Radar) ---
    layout_columns(
      col_widths = c(5, 3, 4), 
      fill = FALSE,
      gap = "15px",
      
      card(height = "500px", card_header("Scout Intelligence Report"), div(style = "overflow-y: auto; height: 400px;", gt_output(ns("analytics_table")))),
      card(height = "450px", card_header("Cohort Top Goal Scorers"), plotlyOutput(ns("chart_lollipop"), height = "400px")),
      card(height = "450px", card_header("Cohort Attribute Profile"), plotlyOutput(ns("chart_radar"), height = "400px"))
    ),
    
    # --- ROW 3: DEEP DIVE (Value Gems | Squad) ---
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      gap = "15px",
      
      # IMPROVED VALUE GEMS CHART
      card(height = "380px", card_header("Value Gems (ROI vs Average)"), plotlyOutput(ns("chart_diverging"), height = "330px")),
      
      card(height = "380px", card_header("Squad Points Distribution"), plotlyOutput(ns("chart_sunburst"), height = "330px"))
    )
  )
}

# --- SERVER COMPONENT ---
playerExplorerServer <- function(id, fpl_data, team_filter, position_filter, status_filter, price_filter) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Filter Logic
    filtered_data <- reactive({
      # --- SAFETY CHECKS ---
      req(fpl_data) 
      req(price_filter()) # Prevents crash if price input is empty/loading
      
      df <- fpl_data
      
      # Apply Filters
      if (team_filter() != "All Teams") df <- df %>% filter(team_name == team_filter())
      if (position_filter() != "All Positions") df <- df %>% filter(position == position_filter())
      if (status_filter() == "Available Only") df <- df %>% filter(chance_of_playing >= 75 | is.na(chance_of_playing))
      
      # Price Filter (Safe)
      df %>% 
        filter(cost <= price_filter()) %>%
        mutate(
          value_ratio = round(total_points / cost, 1),
          player_image = paste0("<img src='", ifelse(is.na(photo_url) | photo_url == '', 'https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png', photo_url), "' style='height:35px; width:35px; border-radius:50%; border: 2px solid #00FF85;'>")
        ) %>% arrange(desc(form)) 
    })
    
    # 2. Integrated Ticker
    output$integrated_ticker <- DT::renderDataTable({
      req(readRDS("data/fixture_ticker.rds"))
      df <- readRDS("data/fixture_ticker.rds")
      if(team_filter() != "All Teams") df <- df %>% filter(name == team_filter())
      
      DT::datatable(
        df %>% select(name, starts_with("opp_label"), starts_with("difficulty")),
        colnames = c("Team", "Next", "+2", "+3", "+4", "+5"),
        options = list(
          pageLength = 3, dom = 'tp', ordering = FALSE, autoWidth = FALSE, 
          columnDefs = list(list(width = '20%', targets = 0), list(width = '16%', targets = 1:5), list(visible = FALSE, targets = 6:10)),
          initComplete = DT::JS("function(settings, json) { $(this.api().table().container()).css({'background-color': '#190028', 'color': '#FFFFFF'}); $(this.api().table().header()).css({'background-color': '#2A0040', 'color': '#FFFFFF'}); }")
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = paste0("opp_label_", 1:5), valueColumns = paste0("difficulty_", 1:5),
          backgroundColor = DT::styleEqual(c(1, 2, 3, 4, 5), c("#00FF85", "#00FF85", "#EBEBE4", "#FF00FF", "#E90052")),
          color = "#190028", fontWeight = "bold", textAlign = "center"
        )
    })
    
    # 3. KPI Renders
    output$kpi_count <- renderText({ nrow(filtered_data()) })
    output$kpi_price <- renderText({ if(nrow(filtered_data()) > 0) paste0("£", round(mean(filtered_data()$cost, na.rm=TRUE), 1), "m") else "£0.0m" })
    output$kpi_form <- renderText({ if(nrow(filtered_data()) > 0) max(filtered_data()$form, na.rm=TRUE) else "0.0" })
    
    # 4. REDESIGNED VALUE GEMS CHART (Diverging)
    output$chart_diverging <- renderPlotly({
      req(nrow(filtered_data()) > 0)
      top_players <- filtered_data() %>% head(15)
      avg_roi <- mean(top_players$value_ratio, na.rm=TRUE)
      
      plot_data <- top_players %>% 
        mutate(
          diff = value_ratio - avg_roi, 
          color_flag = ifelse(diff >= 0, "#00FF85", "#E90052") 
        ) %>% 
        arrange(diff) 
      
      plot_data$web_name <- factor(plot_data$web_name, levels = plot_data$web_name)
      
      plot_ly(plot_data, x = ~diff, y = ~web_name, type = 'bar', orientation = 'h',
              marker = list(
                color = ~color_flag, 
                line = list(color = "white", width = 1), 
                opacity = 0.9
              ),
              hoverinfo = "text",
              hovertext = ~paste0(
                "<b>", web_name, "</b>",
                "<br>ROI Variance: ", round(diff, 2),
                "<br>True ROI: ", value_ratio
              )
      ) %>%
        layout(
          xaxis = list(
            title = "ROI Variance (vs Group Avg)", 
            color = "#cccccc",
            zeroline = TRUE, zerolinecolor = "#FFFFFF", zerolinewidth = 2, 
            gridcolor = "#333333"
          ), 
          yaxis = list(title = "", color = "white", tickfont = list(size = 12)), 
          paper_bgcolor = "rgba(0,0,0,0)", 
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l=100, r=20, t=10, b=40),
          showlegend = FALSE
        )
    })
    
    # 5. Other Charts
    output$chart_radar <- renderPlotly({ req(nrow(filtered_data()) > 0); generate_radar_chart(filtered_data(), fpl_data) })
    output$chart_sunburst <- renderPlotly({ req(nrow(filtered_data()) > 0); generate_sunburst_chart(filtered_data()) })
    output$analytics_table <- render_gt({ req(nrow(filtered_data()) > 0); generate_player_table(filtered_data()) })
    output$chart_lollipop <- renderPlotly({ req(nrow(filtered_data()) > 0); generate_lollipop_chart(filtered_data()) })
  })
}