
render_position_radar <- function(picks_df, fpl_data) {
  req(picks_df, fpl_data)
  
  # 1. Column Detection & Validation
  if(!"gw_points" %in% names(picks_df)) return(NULL)
  possible_names <- c("element_type", "position", "pos", "type")
  found_col <- intersect(names(fpl_data), possible_names)[1]
  if (is.na(found_col)) return(NULL)
  
  # 2. Prepare Data (Calculate % of Points per Position)
  radar_data <- picks_df %>%
    left_join(fpl_data %>% select(id, pos_code = all_of(found_col)), by = c("element" = "id")) %>%
    mutate(
      position = case_when(
        as.character(pos_code) %in% c("1", "GKP", "Goalkeeper") ~ "GKP",
        as.character(pos_code) %in% c("2", "DEF", "Defender")    ~ "DEF",
        as.character(pos_code) %in% c("3", "MID", "Midfielder")  ~ "MID",
        as.character(pos_code) %in% c("4", "FWD", "Forward")     ~ "FWD",
        TRUE ~ "UNK"
      )
    ) %>%
    filter(position != "UNK") %>%
    group_by(manager, position) %>%
    summarise(pos_points = sum(gw_points, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(
      total_points = sum(pos_points),
      pct = (pos_points / total_points) * 100
    ) %>%
    ungroup() %>%
    select(manager, position, pct)
  
  # 3. Reshape for Plotly (Ensure all positions exist)
  radar_data <- radar_data %>%
    complete(manager, position = c("GKP", "DEF", "MID", "FWD"), fill = list(pct = 0)) %>%
    arrange(manager, factor(position, levels = c("GKP", "DEF", "MID", "FWD")))
  
  # 4. Color Palette (Distinct Neon)
  colors <- c("#00FF85", "#E90052", "#04F5FF", "#FFFF00", "#FF00FF")
  managers <- unique(radar_data$manager)
  
  # 5. Render Chart
  fig <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
  
  for(i in seq_along(managers)) {
    mgr <- managers[i]
    mgr_data <- radar_data %>% filter(manager == mgr)
    
    # Close the loop
    r_vals <- c(mgr_data$pct, mgr_data$pct[1])
    theta_vals <- c(mgr_data$position, mgr_data$position[1])
    
    color_hex <- colors[(i - 1) %% length(colors) + 1]
    
    fig <- fig %>%
      add_trace(
        r = r_vals,
        theta = theta_vals,
        name = mgr,
        line = list(color = color_hex, width = 3),
        marker = list(color = color_hex, size = 6, line = list(color = "white", width = 1)),
        fill = 'toself',
        fillcolor = paste0(substr(color_hex, 1, 7), "22"), # Very transparent (Hex + "22")
        hoverinfo = "text",
        text = paste0("<b>", mgr, "</b><br>", theta_vals, ": ", round(r_vals, 1), "%")
      )
  }
  
  fig %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, max(radar_data$pct, na.rm=TRUE) + 5),
          showticklabels = FALSE, # Clean look
          gridcolor = "#333",
          linecolor = "#333"
        ),
        angularaxis = list(
          tickfont = list(color = "white", size = 12, bold = TRUE),
          gridcolor = "#444",
          linecolor = "#444"
        ),
        bgcolor = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center", font = list(color = "white")),
      margin = list(t = 30, b = 40, l = 40, r = 40)
    ) %>% config(displayModeBar = FALSE)
}