library(shiny)
library(dplyr)
library(plotly)
library(bslib)
library(bsicons)

# --- UI COMPONENT ---
accuracyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- CSS ---
    tags$head(tags$style(HTML("
      .accuracy-card { border: 1px solid #38003C; background-color: #190028; margin-bottom: 20px; box-shadow: 0px 4px 15px rgba(0,0,0,0.3); }
      .card-header { background-color: #2A0040 !important; border-bottom: 1px solid #38003C !important; color: white !important; font-weight: bold; }
      .value-box { background: linear-gradient(145deg, #2A0040, #190028); border: 1px solid #38003C; }
      .value-box .value { color: #fff; font-size: 1.8rem; font-weight: bold; }
      .value-box .caption { color: #aaa; }
    "))),
    
    # --- METRICS ---
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      value_box(title = "Prediction Accuracy", value = textOutput(ns("accuracy_pct")), showcase = bs_icon("bullseye"), theme = "primary"),
      value_box(title = "Avg Deviation", value = textOutput(ns("avg_error")), showcase = bs_icon("activity"), theme = "secondary")
    ),
    
    # --- CHART ---
    card(
      class = "accuracy-card",
      full_screen = TRUE,
      card_header("Performance Tracker: Projection vs Reality"),
      plotlyOutput(ns("surplus_chart"), height = "450px")
    )
  )
}

# --- SERVER COMPONENT ---
accuracyServer <- function(id, fpl_data) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. AUTO-DETECT NEW FILES (Reactive Poll) ---
    # Checks for new CSV files every 1000ms (1 second)
    prediction_files <- reactivePoll(1000, session,
                                     checkFunc = function() {
                                       # Check the 'last modified' time of the latest csv file
                                       files <- list.files("data", pattern = "prediction_gw_.*\\.csv", full.names = TRUE)
                                       if (length(files) == 0) return(0)
                                       max(file.info(files)$mtime)
                                     },
                                     valueFunc = function() {
                                       list.files("data", pattern = "prediction_gw_.*\\.csv", full.names = TRUE)
                                     }
    )
    
    # --- 2. READ AND MERGE DATA ---
    comparison_data <- reactive({
      
      # A. Load Predictions (Triggered by file change)
      files <- prediction_files()
      if (length(files) == 0) return(NULL)
      
      predictions <- lapply(files, function(f) {
        d <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
        if(is.null(d)) return(NULL)
        if(!"id" %in% names(d)) return(NULL)
        if(!"gameweek" %in% names(d)) d$gameweek <- as.numeric(gsub("\\D", "", basename(f)))
        d %>% select(id, gameweek, predicted_points)
      }) %>% bind_rows()
      
      if(nrow(predictions) == 0) return(NULL)
      
      # B. Load Actuals (Safely)
      # We assume Actuals might not exist yet or file might be missing
      actuals <- NULL
      if(file.exists("data/fpl_raw.rds")) {
        try({
          raw <- readRDS("data/fpl_raw.rds")
          if("history" %in% names(raw)) {
            actuals <- raw$history %>% select(id = element, gameweek = round, actual_points = total_points)
          }
        }, silent = TRUE)
      }
      
      # C. Merge
      if(!is.null(actuals)) {
        merged_df <- predictions %>%
          left_join(actuals, by = c("id", "gameweek")) %>%
          # If actual points are NA, it means the game hasn't happened yet.
          # We keep it as NA so we don't plot a misleading "0" line.
          mutate(actual_points = ifelse(is.na(actual_points), NA, actual_points))
      } else {
        # If no history data at all, just show predictions
        merged_df <- predictions %>% mutate(actual_points = NA)
      }
      
      # D. Summarize
      summary_df <- merged_df %>%
        group_by(gameweek) %>%
        summarise(
          predicted_total = sum(predicted_points, na.rm = TRUE),
          actual_total = sum(actual_points, na.rm = TRUE), 
          # We only consider a week "complete" if we have actual data points (not NA)
          is_complete = any(!is.na(actual_points))
        ) %>%
        arrange(gameweek)
      
      return(summary_df)
    })
    
    # --- 3. RENDER CHART ---
    output$surplus_chart <- renderPlotly({
      df <- comparison_data()
      
      # Blank state if no data
      if (is.null(df)) {
        return(plot_ly() %>% 
                 layout(title = "No predictions found yet. Run the Dream Team optimizer first.", 
                        xaxis = list(visible = F), yaxis = list(visible = F),
                        paper_bgcolor = "#190028", plot_bgcolor = "#190028",
                        font = list(color = "white", family = "Poppins")))
      }
      
      p <- plot_ly(df, x = ~gameweek) %>%
        # 1. Predicted Line (Always Visible)
        add_trace(
          y = ~predicted_total, type = 'scatter', mode = 'lines+markers',
          name = 'Predicted', line = list(color = '#04F5FF', width = 3, dash = 'dot'),
          marker = list(color = '#04F5FF', size = 8),
          hovertemplate = '<b>GW %{x}</b><br>Predicted: %{y:.1f} pts<extra></extra>'
        ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Gameweek", color = "#cccccc", showgrid = FALSE, dtick = 1),
          yaxis = list(title = "Total Points", color = "#cccccc", gridcolor = "#333333"),
          legend = list(orientation = "h", x = 0.3, y = 1.1, font = list(color = "white")),
          margin = list(l=50, r=20, t=40, b=40), font = list(family = "Poppins")
        ) %>% config(displayModeBar = FALSE)
      
      # 2. Add Actuals (Only if data exists)
      if(any(df$is_complete)) {
        completed_data <- df %>% filter(is_complete)
        p <- p %>% 
          add_trace(
            data = completed_data,
            y = ~actual_total, type = 'scatter', mode = 'lines+markers',
            name = 'Actual', line = list(color = '#FFFFFF', width = 3),
            marker = list(color = '#FFFFFF', size = 10),
            hovertemplate = '<b>GW %{x}</b><br>Actual: %{y} pts<extra></extra>'
          ) %>%
          add_trace(
            data = completed_data,
            y = ~predicted_total, type = 'scatter', mode = 'none', fill = 'tozeroy',
            fillcolor = 'rgba(0, 255, 133, 0.1)', showlegend = FALSE, hoverinfo = 'skip'
          )
      }
      
      return(p)
    })
    
    # --- 4. KPIs ---
    output$accuracy_pct <- renderText({
      df <- comparison_data()
      if (is.null(df)) return("N/A")
      completed <- df %>% filter(is_complete)
      if(nrow(completed) == 0) return("Pending Results...")
      
      acc <- 1 - (abs(sum(completed$actual_total) - sum(completed$predicted_total)) / sum(completed$actual_total))
      paste0(round(max(0, acc) * 100, 1), "%")
    })
    
    output$avg_error <- renderText({
      df <- comparison_data()
      if (is.null(df)) return("N/A")
      completed <- df %>% filter(is_complete)
      if(nrow(completed) == 0) return("Pending Results...")
      
      avg_diff <- mean(completed$actual_total - completed$predicted_total)
      sign <- ifelse(avg_diff > 0, "+", "")
      paste0(sign, round(avg_diff, 1), " pts")
    })
  })
}