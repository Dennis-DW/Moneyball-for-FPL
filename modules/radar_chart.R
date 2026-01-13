library(scales) 

generate_radar_chart <- function(filtered_df, full_df) {
  
  if(nrow(filtered_df) == 0) return(NULL)
  
  # --- 1. Limit to Top 5 Players ---
  # Radar charts get messy with too many shapes. 
  # take the top 5 from the current filtered view.
  plot_data <- head(filtered_df, 5)
  
  # --- 2. Define Helper Functions ---
  get_col <- function(df, col_name) {
    if(col_name %in% names(df)) return(as.numeric(df[[col_name]]))
    return(0)
  }
  
  # --- 3. Calculate League Maximums (For Scaling 0-100) ---
  max_vals <- list(
    form = max(get_col(full_df, "form"), na.rm=TRUE),
    ict  = max(get_col(full_df, "ict_index"), na.rm=TRUE),
    xgi  = max(get_col(full_df, "expected_goal_involvements"), na.rm=TRUE),
    val  = max(get_col(full_df, "total_points") / get_col(full_df, "cost"), na.rm=TRUE),
    ppg  = max(get_col(full_df, "points_per_game"), na.rm=TRUE)
  )
  
  # --- 4. Setup Chart ---
  p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
  
  # Define Axis Labels
  labels <- c("Form", "ICT Index", "Threat (xGI)", "Value", "Reliability")
  labels <- c(labels, labels[1]) # Close loop
  
  # Custom Neon Palette for Players
  colors <- c("#00FF85", "#E90052", "#04F5FF", "#FFFF00", "#FF00FF")
  
  # --- 5. Loop Through Each Player and Add Trace ---
  for(i in 1:nrow(plot_data)) {
    player <- plot_data[i, ]
    
    # Calculate Player Metrics
    p_vals <- list(
      form = get_col(player, "form"),
      ict  = get_col(player, "ict_index"),
      xgi  = get_col(player, "expected_goal_involvements"),
      val  = get_col(player, "total_points") / get_col(player, "cost"),
      ppg  = get_col(player, "points_per_game")
    )
    
    # Scale to 0-100
    scale_val <- function(val, max_v) {
      if(max_v == 0) return(0)
      pmin((val / max_v) * 100, 100)
    }
    
    scaled_vals <- c(
      scale_val(p_vals$form, max_vals$form),
      scale_val(p_vals$ict,  max_vals$ict),
      scale_val(p_vals$xgi,  max_vals$xgi),
      scale_val(p_vals$val,  max_vals$val),
      scale_val(p_vals$ppg,  max_vals$ppg)
    )
    scaled_vals <- c(scaled_vals, scaled_vals[1]) # Close loop
    
    # Tooltip Text
    tooltip <- c(
      paste0("<b>Form:</b> ", round(p_vals$form, 1)),
      paste0("<b>ICT:</b> ", round(p_vals$ict, 1)),
      paste0("<b>xGI:</b> ", round(p_vals$xgi, 2)),
      paste0("<b>Value:</b> ", round(p_vals$val, 1)),
      paste0("<b>PPG:</b> ", round(p_vals$ppg, 1)),
      ""
    )
    
    # Add Player Trace
    p <- p %>% add_trace(
      r = scaled_vals,
      theta = labels,
      name = player$web_name,
      line = list(color = colors[i], width = 3),
      marker = list(color = colors[i], size = 6),
      fill = 'toself',
      fillcolor = paste0(substr(colors[i], 1, 7), "33"), 
      hoverinfo = 'text',
      text = paste0("<b>", player$web_name, "</b><br>", tooltip)
    )
  }
  
  # --- 6. Add League Average (Background Reference) ---
  avg_league <- list(
    form = mean(get_col(full_df, "form"), na.rm=TRUE),
    ict  = mean(get_col(full_df, "ict_index"), na.rm=TRUE),
    xgi  = mean(get_col(full_df, "expected_goal_involvements"), na.rm=TRUE),
    val  = mean(get_col(full_df, "total_points") / get_col(full_df, "cost"), na.rm=TRUE),
    ppg  = mean(get_col(full_df, "points_per_game"), na.rm=TRUE)
  )
  
  league_vals <- c(
    scale_val(avg_league$form, max_vals$form),
    scale_val(avg_league$ict,  max_vals$ict),
    scale_val(avg_league$xgi,  max_vals$xgi),
    scale_val(avg_league$val,  max_vals$val),
    scale_val(avg_league$ppg,  max_vals$ppg)
  )
  league_vals <- c(league_vals, league_vals[1])
  
  p <- p %>% add_trace(
    r = league_vals,
    theta = labels,
    name = 'League Avg',
    line = list(color = '#666666', width = 2, dash = 'dash'),
    marker = list(size = 0),
    fill = 'none',
    hoverinfo = 'skip'
  )
  
  # --- 7. Final Layout ---
  p %>% layout(
    polar = list(
      radialaxis = list(visible = TRUE, range = c(0, 100), tickfont = list(color = "#666"), gridcolor = "#333"),
      angularaxis = list(tickfont = list(color = "white", size = 11, bold = TRUE), gridcolor = "#333"),
      bgcolor = "rgba(0,0,0,0)"
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    showlegend = TRUE,
    legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center", font = list(color = "white")),
    margin = list(t=40, b=40, l=40, r=40),
    font = list(family = "Poppins")
  ) %>%
    config(displayModeBar = FALSE)
}