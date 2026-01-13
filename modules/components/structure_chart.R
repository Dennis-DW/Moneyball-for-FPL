# modules/components/structure_chart.R


render_position_radar <- function(picks_df, fpl_data) {
  req(picks_df, fpl_data)
  
  if(!"gw_points" %in% names(picks_df)) return(NULL)
  
  # 1. Column Detection
  possible_names <- c("element_type", "position", "pos", "type")
  found_col <- intersect(names(fpl_data), possible_names)[1]
  
  if (is.na(found_col)) validate("Could not find player position column in FPL data.")
  
  # 2. Prepare Data
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
    group_by(manager, position) %>%
    summarise(pos_points = sum(gw_points, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(
      total_points = sum(pos_points),
      pct = (pos_points / total_points) * 100
    ) %>%
    ungroup() %>%
    select(manager, position, pct)
  
  # 3. Reshape
  radar_data <- radar_data %>%
    complete(manager, position = c("GKP", "DEF", "MID", "FWD"), fill = list(pct = 0)) %>%
    arrange(manager, factor(position, levels = c("GKP", "DEF", "MID", "FWD")))
  
  # 4. Render Radar Chart
  fig <- plot_ly(type = 'scatterpolar', fill = 'toself')
  
  managers <- unique(radar_data$manager)
  
  for(mgr in managers) {
    mgr_data <- radar_data %>% filter(manager == mgr)
    r_vals <- c(mgr_data$pct, mgr_data$pct[1])
    theta_vals <- c(mgr_data$position, mgr_data$position[1])
    
    fig <- fig %>%
      add_trace(
        r = r_vals,
        theta = theta_vals,
        name = mgr,
        hoverinfo = "text",
        text = paste0(mgr, "<br>", theta_vals, ": ", round(r_vals, 1), "%")
      )
  }
  
  fig %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, max(radar_data$pct, na.rm = TRUE) + 5),
          ticksuffix = "%",
          color = "#AAA", # Lighter text
          gridcolor = "#444"
        ),
        bgcolor = "rgba(0,0,0,0)",
        gridshape = "linear"
      ),
      title = list(text = ""), 
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Poppins", color = "white"),
      showlegend = TRUE,
      # OPTIMIZED LAYOUT
      legend = list(orientation = "h", x = 0, y = -0.15), 
      margin = list(t = 20, b = 20, l = 30, r = 30)       
    ) %>% config(displayModeBar = FALSE)
}