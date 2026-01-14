# modules/components/decisions_chart.R
library(plotly)
library(gt)
library(dplyr)
library(tidyr)

# --- 1. SEASON EFFICIENCY ---
render_season_captaincy <- function(picks_df, fpl_data) {
  req(picks_df, fpl_data)
  
  picks_df <- as.data.frame(picks_df)
  
  if(!"gw_points" %in% names(picks_df)) {
    validate("Data needs refreshing to see historical points.")
  }
  
  # 1. Prepare Data
  season_analysis <- picks_df %>%
    group_by(manager) %>%
    summarise(
      # Actual: Points from chosen captain (multiplier > 1)
      total_actual = sum(gw_points[multiplier > 1] * multiplier[multiplier > 1], na.rm = TRUE),
      # Optimal: Max points in squad * 2
      total_optimal = sum(tapply(gw_points, event, max, na.rm=TRUE) * 2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      points_lost = total_optimal - total_actual,
      efficiency = (total_actual / total_optimal) * 100
    ) %>%
    arrange(desc(total_actual))
  
  season_analysis$label <- paste0("<b>", season_analysis$total_actual, "</b>")
  season_analysis$hover <- paste0(
    "<b>", season_analysis$manager, "</b><br>",
    "Actual: ", season_analysis$total_actual, "<br>",
    "Optimal: ", season_analysis$total_optimal, "<br>",
    "Lost: -", season_analysis$points_lost, " pts"
  )
  
  # 2. Render Chart
  plot_ly(season_analysis) %>%
    # Background Bar (Potential)
    add_trace(
      y = ~reorder(manager, total_actual), 
      x = ~total_optimal,
      type = "bar", 
      orientation = "h", 
      name = "Missed Potential",
      marker = list(
        color = "rgba(255, 255, 255, 0.05)", 
        line = list(color = "#444", width = 1)
      ),
      hoverinfo = "text",
      hovertext = ~paste0("Max Possible: ", total_optimal)
    ) %>%
    # Foreground Bar (Actual)
    add_trace(
      y = ~reorder(manager, total_actual), 
      x = ~total_actual,
      type = "bar", 
      orientation = "h", 
      name = "Secured Points",
      marker = list(
        color = ~ifelse(efficiency >= 80, "#00FF85", ifelse(efficiency >= 60, "#04F5FF", "#E90052")), # Color based on success
        line = list(color = "white", width = 1)
      ),
      text = ~label,
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "black", weight = "bold"),
      hoverinfo = "text", 
      hovertext = ~hover
    ) %>%
    layout(
      title = list(text = "Captaincy Efficiency (Actual vs Max)", font = list(size = 14, color = "white")),
      barmode = "overlay", 
      xaxis = list(
        title = "Total Captain Points", 
        color = "#aaa", 
        gridcolor = "#333", 
        zeroline = FALSE
      ),
      yaxis = list(
        title = "", 
        color = "white", 
        tickfont = list(size = 11)
      ),
      paper_bgcolor = "rgba(0,0,0,0)", 
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Poppins", color = "white"),
      showlegend = FALSE, 
      margin = list(l = 100, r = 20, t = 40, b = 40)
    ) %>% config(displayModeBar = FALSE)
}

# --- 2. GAMEWEEK REPORT TABLE (High Contrast) ---
render_gw_captaincy_table <- function(picks_df, fpl_data, target_gw) {
  req(picks_df, fpl_data, target_gw)
  
  gw_picks <- picks_df %>% 
    filter(event == as.numeric(target_gw)) %>%
    as.data.frame()
  
  if(nrow(gw_picks) == 0) return(gt(data.frame(Message = "No Data")))
  
  # Join with player names
  full_data <- gw_picks %>%
    left_join(fpl_data %>% select(id, web_name), by = c("element" = "id")) %>%
    mutate(gw_points = replace_na(gw_points, 0))
  
  gw_table <- full_data %>%
    group_by(manager) %>%
    summarise(
      Cap_Name = web_name[which.max(multiplier)],
      Cap_Pts  = gw_points[which.max(multiplier)] * multiplier[which.max(multiplier)],
      Best_Name = web_name[which.max(gw_points)],
      Best_Pts  = max(gw_points) * 2, 
      .groups = "drop"
    ) %>%
    mutate(
      Difference = Best_Pts - Cap_Pts,
      Status = ifelse(Difference == 0, "✅ Optimal", paste0("❌ -", Difference))
    ) %>%
    arrange(desc(Cap_Pts))
  
  # Render Table
  gw_table %>%
    select(manager, Cap_Name, Cap_Pts, Best_Name, Status) %>%
    gt() %>%
    cols_label(
      manager = "Manager",
      Cap_Name = "Captain",
      Cap_Pts = "Pts",
      Best_Name = "Optimal Pick",
      Status = "Result"
    ) %>%
    tab_header(
      title = paste0("Gameweek ", target_gw, " Report")
    ) %>%
    tab_options(
      table.width = pct(100),
      table.background.color = "#190028",
      table.font.color = "white",
      table.font.size = px(12),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#2A0040",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      data_row.padding = px(6)
    ) %>%
    # Highlight Captain Points (Green Scale)
    data_color(
      columns = c(Cap_Pts),
      colors = scales::col_numeric(
        palette = c("#190028", "#00FF85"),
        domain = NULL
      )
    ) %>%
    # Highlight Result Text (Manual Logic)
    tab_style(
      style = cell_text(color = "#00FF85", weight = "bold"),
      locations = cells_body(columns = Status, rows = grepl("Optimal", Status))
    ) %>%
    tab_style(
      style = cell_text(color = "#E90052", weight = "bold"),
      locations = cells_body(columns = Status, rows = grepl("❌", Status))
    ) %>%
    cols_align(align = "center", columns = -manager)
}