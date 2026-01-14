
generate_radar_chart <- function(filtered_df, full_df) {
  
  if(nrow(filtered_df) == 0) return(NULL)
  
  # --- 1. Limit to Top 5 Players ---
  plot_data <- head(filtered_df, 5)
  
  # --- 2. Helper to Safely Get Columns ---
  get_col <- function(df, col_name) {
    if(col_name %in% names(df)) return(as.numeric(df[[col_name]]))
    return(0)
  }
  
  # --- 3. Dynamic Scaling (League Max) ---
  max_vals <- list(
    form = max(get_col(full_df, "form"), na.rm=TRUE),
    ict  = max(get_col(full_df, "ict_index"), na.rm=TRUE),
    xgi  = max(get_col(full_df, "expected_goal_involvements"), na.rm=TRUE),
    val  = max(get_col(full_df, "total_points") / get_col(full_df, "cost"), na.rm=TRUE),
    ppg  = max(get_col(full_df, "points_per_game"), na.rm=TRUE)
  )
  
  # Prevent division by zero
  max_vals <- lapply(max_vals, function(x) if(x == 0) 1 else x)
  
  # --- 4. Initialize Chart ---
  p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
  
  # Axis Definitions
  labels <- c("Form", "ICT Index", "Threat", "Value", "Reliability")
  labels_closed <- c(labels, labels[1])
  
  # Neon Palette
  colors <- c("#00FF85", "#E90052", "#04F5FF", "#FFFF00", "#FF00FF")
  
  # --- 5. Add Player Traces ---
  for(i in 1:nrow(plot_data)) {
    player <- plot_data[i, ]
    
    # Raw Values
    raw_vals <- list(
      form = get_col(player, "form"),
      ict  = get_col(player, "ict_index"),
      xgi  = get_col(player, "expected_goal_involvements"),
      val  = get_col(player, "total_points") / get_col(player, "cost"),
      ppg  = get_col(player, "points_per_game")
    )
    
    # Scaled Values (0-100)
    scaled <- c(
      (raw_vals$form / max_vals$form) * 100,
      (raw_vals$ict  / max_vals$ict)  * 100,
      (raw_vals$xgi  / max_vals$xgi)  * 100,
      (raw_vals$val  / max_vals$val)  * 100,
      (raw_vals$ppg  / max_vals$ppg)  * 100
    )
    scaled_closed <- c(scaled, scaled[1])
    
    # Tooltip
    txt <- paste0(
      "<b>", player$web_name, "</b><br>",
      "Form: ", round(raw_vals$form, 1), "<br>",
      "ICT: ", round(raw_vals$ict, 1), "<br>",
      "xGI: ", round(raw_vals$xgi, 2), "<br>",
      "Value: ", round(raw_vals$val, 1), "<br>",
      "PPG: ", round(raw_vals$ppg, 1)
    )
    
    p <- p %>% add_trace(
      r = scaled_closed,
      theta = labels_closed,
      name = player$web_name,
      line = list(color = colors[i], width = 2.5),
      marker = list(color = colors[i], size = 7, line = list(color = "white", width = 1)),
      fill = 'toself',
      fillcolor = paste0(substr(colors[i], 1, 7), "33"), # ~20% opacity
      hoverinfo = 'text',
      text = txt
    )
  }
  
  # --- 6. Add League Average (Dashed Line) ---
  avg_vals <- c(
    mean(get_col(full_df, "form"), na.rm=TRUE),
    mean(get_col(full_df, "ict_index"), na.rm=TRUE),
    mean(get_col(full_df, "expected_goal_involvements"), na.rm=TRUE),
    mean(get_col(full_df, "total_points") / get_col(full_df, "cost"), na.rm=TRUE),
    mean(get_col(full_df, "points_per_game"), na.rm=TRUE)
  )
  
  scaled_avg <- c(
    (avg_vals[1] / max_vals$form) * 100,
    (avg_vals[2] / max_vals$ict) * 100,
    (avg_vals[3] / max_vals$xgi) * 100,
    (avg_vals[4] / max_vals$val) * 100,
    (avg_vals[5] / max_vals$ppg) * 100
  )
  scaled_avg_closed <- c(scaled_avg, scaled_avg[1])
  
  p <- p %>% add_trace(
    r = scaled_avg_closed,
    theta = labels_closed,
    name = 'League Avg',
    line = list(color = '#888888', width = 2, dash = 'dashdot'),
    marker = list(size = 0),
    fill = 'none',
    hoverinfo = 'skip'
  )
  
  # --- 7. Layout & Styling ---
  p %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE, 
        range = c(0, 100), 
        showticklabels = FALSE,
        gridcolor = "#333333",
        gridwidth = 1
      ),
      angularaxis = list(
        tickfont = list(color = "#E0E0E0", size = 12, family = "Poppins"),
        gridcolor = "#333333",
        linecolor = "#333333"
      ),
      bgcolor = "rgba(0,0,0,0)"
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    showlegend = TRUE,
    legend = list(
      orientation = "h", 
      x = 0.5, y = -0.1, 
      xanchor = "center", 
      font = list(color = "white", size = 10),
      bgcolor = "rgba(0,0,0,0)"
    ),
    margin = list(t=30, b=30, l=40, r=40)
  ) %>%
    config(displayModeBar = FALSE)
}