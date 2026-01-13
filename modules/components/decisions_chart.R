# modules/components/decisions_chart.R


# --- 1. SEASON AGGREGATE CHART ---
render_season_captaincy <- function(picks_df, fpl_data) {
  req(picks_df, fpl_data)
  
  picks_df <- as.data.frame(picks_df)
  
  # Ensure gw_points exists
  if(!"gw_points" %in% names(picks_df)) {
    validate("Data needs refreshing to see historical points. Please clear 'data/' folder and restart.")
  }
  
  # Use PRE-FETCHED gw_points
  # Note: multiplier > 1 means Captain (2x) 
  season_analysis <- picks_df %>%
    group_by(manager) %>%
    summarise(
      # Actual Captain Points (using the points stored in picks_df)
      total_actual = sum(gw_points[multiplier > 1] * multiplier[multiplier > 1], na.rm = TRUE),
      
      # Optimal Captain Points (Max score in squad * 2)
      total_optimal = sum(tapply(gw_points, event, max, na.rm=TRUE) * 2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(points_lost = total_optimal - total_actual) %>%
    arrange(desc(total_actual)) %>%
    as.data.frame()
  
  season_analysis$label <- paste0(season_analysis$total_actual, " pts")
  season_analysis$hover <- paste0(
    "<b>", season_analysis$manager, "</b><br>",
    "Actual: ", season_analysis$total_actual, "<br>",
    "Max Possible: ", season_analysis$total_optimal, "<br>",
    "Lost: ", season_analysis$points_lost
  )
  
  plot_ly(season_analysis) %>%
    add_trace(
      y = ~reorder(manager, total_actual), x = ~total_optimal,
      type = "bar", orientation = "h", name = "Potential",
      marker = list(color = "rgba(255, 255, 255, 0.1)", line = list(color = "#888", width = 1)),
      hoverinfo = "skip"
    ) %>%
    add_trace(
      y = ~reorder(manager, total_actual), x = ~total_actual,
      type = "bar", orientation = "h", name = "Actual",
      marker = list(
        color = ~ifelse(points_lost == 0, "#00FF85", "#E90052"),
        line = list(color = "white", width = 1)
      ),
      text = season_analysis$label,
      textposition = "auto", textfont = list(color = "white"),
      hoverinfo = "text", hovertext = season_analysis$hover
    ) %>%
    layout(
      title = list(text = "Season Captaincy Efficiency", font = list(size = 14, color = "white")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      barmode = "overlay", 
      xaxis = list(title = "Total Points", color = "white", gridcolor = "#333"),
      yaxis = list(title = "", color = "white"), 
      font = list(family = "Poppins", color = "white"),
      showlegend = FALSE, margin = list(l = 100)
    ) %>% config(displayModeBar = FALSE)
}

# --- 2. GAMEWEEK TABLE ---
render_gw_captaincy_table <- function(picks_df, fpl_data, target_gw) {
  req(picks_df, fpl_data, target_gw)
  
  gw_picks <- picks_df %>% 
    filter(event == as.numeric(target_gw)) %>%
    as.data.frame()
  
  shiny::validate(
    shiny::need(nrow(gw_picks) > 0, paste("No data available for Gameweek", target_gw))
  )
  
  # Join with names (FPL Data) 
  # Points are already in picks_df as 'gw_points'
  full_data <- gw_picks %>%
    left_join(fpl_data %>% select(id, web_name), by = c("element" = "id")) %>%
    mutate(gw_points = replace_na(gw_points, 0))
  
  gw_table <- full_data %>%
    group_by(manager) %>%
    summarise(
      Cap_Name = web_name[which.max(multiplier)],
      # Actual points calculation (handling TC)
      Cap_Pts  = gw_points[which.max(multiplier)] * multiplier[which.max(multiplier)],
      
      Best_Name = web_name[which.max(gw_points)],
      Best_Pts  = max(gw_points) * 2, # Assume standard 2x for comparison
      .groups = "drop"
    ) %>%
    mutate(Difference = Best_Pts - Cap_Pts) %>%
    arrange(desc(Cap_Pts))
  
  # Render Table
  gw_table %>%
    select(manager, Cap_Name, Cap_Pts, Best_Name, Best_Pts, Difference) %>%
    gt() %>%
    cols_label(
      manager = "Manager",
      Cap_Name = "Captain",
      Cap_Pts = "Pts",
      Best_Name = "Optimal",
      Best_Pts = "Max",
      Difference = "Lost"
    ) %>%
    tab_header(
      title = paste0("Gameweek ", target_gw, " Report"),
      subtitle = "Captaincy vs Optimal"
    ) %>%
    tab_options(
      table.width = pct(100),
      table.background.color = "#190028",      
      heading.background.color = "#2A0040",    
      column_labels.background.color = "#2A0040",
      table.font.color = "white",
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(12),
      table.font.size = px(13),
      data_row.padding = px(4),
      heading.padding = px(5),
      column_labels.padding = px(5),
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      heading.border.bottom.style = "none",
      column_labels.border.bottom.style = "none",
      table_body.border.bottom.style = "none",
      table_body.hlines.style = "solid",
      table_body.hlines.color = "#444444" 
    ) %>%
    data_color(
      columns = c(Cap_Pts),
      method = "numeric",
      palette = c("#190028", "#00FF85")
    ) %>%
    data_color(
      columns = c(Difference),
      method = "numeric",
      palette = c("#190028", "#E90052")
    ) %>%
    cols_align(align = "left", columns = manager) %>%
    cols_align(align = "center", columns = -manager) %>%
    opt_table_font(font = google_font("Poppins"))
}