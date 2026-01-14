# modules/components/league_chart.R


# --- 1. PERFORMANCE HEATMAP  ---
render_league_performance_heatmap <- function(history_df, metric = "Vs League Average") {
  req(history_df)
  history_df <- as.data.frame(history_df)
  
  # 1. Prepare Data
  heatmap_data <- history_df %>%
    ungroup() %>%
    mutate(
      manager = as.character(manager),
      event = as.numeric(event),
      gw_points = as.numeric(points)
    ) %>%
    # A. Calculate LEAGUE Context (Per Gameweek)
    group_by(event) %>%
    mutate(
      gw_avg = mean(gw_points),
      gw_max = max(gw_points),
      is_winner = (gw_points == gw_max), # League Winner (Star)
      diff_vs_avg = gw_points - gw_avg,
      gw_rank = rank(-gw_points, ties.method = "min")
    ) %>% 
    ungroup() %>%
    # B. Calculate MANAGER Context (Per Manager)
    group_by(manager) %>%
    mutate(
      season_min = min(gw_points),
      is_low = (gw_points == season_min) # Season Low (Marker)
    ) %>%
    ungroup()
  
  # 2. Determine Metric and Colors
  # 'display_value' column to the dataframe to prevent length errors
  if (metric == "Vs League Average") {
    heatmap_data$display_value <- heatmap_data$diff_vs_avg
    
    # Calculate symmetric range for diverging colors
    max_val <- max(abs(heatmap_data$display_value), na.rm = TRUE)
    z_min <- -max_val
    z_max <- max_val
    colors <- list(c(0, "#E90052"), c(0.5, "#190028"), c(1, "#00FF85"))
    
    heatmap_data$hover_label <- paste0("Vs Avg: ", ifelse(heatmap_data$diff_vs_avg > 0, "+", ""), round(heatmap_data$diff_vs_avg, 1))
  } else {
    heatmap_data$display_value <- heatmap_data$gw_points
    
    z_min <- 0
    z_max <- max(heatmap_data$display_value, na.rm = TRUE)
    colors <- list(c(0, "#190028"), c(1, "#00FF85"))
    
    heatmap_data$hover_label <- paste0("Score: ", heatmap_data$gw_points)
  }
  
  # 3. Text and Tooltips
  heatmap_data$cell_text <- as.character(heatmap_data$gw_points)
  
  heatmap_data$tooltip <- paste0(
    "<b>", heatmap_data$manager, "</b><br>",
    "GW: ", heatmap_data$event, "<br>",
    "Score: ", heatmap_data$gw_points, "<br>",
    "Vs Avg: ", round(heatmap_data$diff_vs_avg, 1), "<br>",
    ifelse(heatmap_data$is_low, "<b>⚠ SEASON LOW</b><br>", ""),
    "Rank: #", heatmap_data$gw_rank
  )
  
  # 4. Filter Datasets for Markers
  winners_only <- heatmap_data %>% filter(is_winner)
  lows_only <- heatmap_data %>% filter(is_low)
  
  # 5. Render Plot (Using explicit add_heatmap to avoid Tibble errors)
  plot_ly() %>%
    add_heatmap(
      data = heatmap_data,
      x = ~event, 
      y = ~manager, 
      z = ~display_value, 
      zmin = z_min, 
      zmax = z_max,
      colorscale = colors,
      hoverinfo = "text", 
      text = ~tooltip, 
      showscale = FALSE
    ) %>%
    # A. Add Score Numbers
    add_annotations(
      data = heatmap_data,
      x = ~event,
      y = ~manager,
      text = ~cell_text,
      showarrow = FALSE,
      font = list(color = "white", size = 10)
    ) %>%
    # B. Add "League Winner" Star (Gold ★)
    add_text(
      data = winners_only, 
      x = ~event, y = ~manager, text = "★",
      textfont = list(color = "#FFD700", size = 20), 
      hoverinfo = "skip"
    ) %>%
    # C. Add "Season Low" Marker (Cyan ▼)
    add_text(
      data = lows_only, 
      x = ~event, y = ~manager, text = "▼",
      textfont = list(color = "#00FFFF", size = 20), 
      hoverinfo = "skip"
    ) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)", 
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "Gameweek", gridcolor = "#333", color = "white", dtick = 1),
      yaxis = list(title = "", gridcolor = "#333", color = "white", automargin = TRUE),
      font = list(color = "white", family = "Poppins"), 
      margin = list(t = 10, r = 10, b = 50, l = 150)
    ) %>% config(displayModeBar = FALSE)
}

# --- 2. OWNERSHIP BAR (Unchanged) ---
render_ownership_bar <- function(picks_df, fpl_data) {
  req(picks_df, fpl_data)
  
  latest_gw <- max(picks_df$event, na.rm = TRUE)
  current_picks <- picks_df %>% filter(event == latest_gw)
  
  possible_names <- c("element_type", "position", "pos", "type")
  found_col <- intersect(names(fpl_data), possible_names)[1]
  
  if (is.na(found_col)) {
    meta <- fpl_data %>% select(id, web_name) %>% mutate(pos_display = "UNK")
  } else {
    meta <- fpl_data %>% select(id, web_name, pos_display = all_of(found_col))
  }
  
  total_mgrs <- n_distinct(current_picks$manager_id)
  
  ownership_data <- current_picks %>%
    left_join(meta, by = c("element" = "id")) %>%
    group_by(web_name, pos_display) %>%
    summarise(
      count = n_distinct(manager_id),
      managers_list = paste(unique(manager), collapse = ", "), 
      .groups = "drop"
    ) %>%
    mutate(pct = (count / total_mgrs) * 100) %>%
    arrange(desc(pct)) %>% head(20)
  
  plot_ly(
    data = ownership_data, y = ~reorder(web_name, pct), x = ~pct, type = 'bar', orientation = 'h',
    marker = list(color = '#00FF85', line = list(color = '#190028', width = 1)),
    hoverinfo = "text",
    text = ~paste0("<b>", web_name, "</b> (", pos_display, ")<br>Owned by: ", round(pct, 1), "%<br>Managers: ", managers_list)
  ) %>%
    layout(
      title = list(text = "The Template (Current GW)", font = list(size = 14, color = "white")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "Ownership %", color = "white", gridcolor = "#333"),
      yaxis = list(title = "", color = "white"),
      font = list(family = "Poppins", color = "white"), margin = list(l = 100)
    ) %>% config(displayModeBar = FALSE)
}

# --- 3. SIMILARITY MATRIX  ---
render_similarity_heatmap <- function(picks_df) {
  req(picks_df)
  
  # --- Filter for Latest Gameweek Only ---
  latest_gw <- max(picks_df$event, na.rm = TRUE)
  
  matrix_df <- picks_df %>%
    filter(event == latest_gw) %>% 
    select(element, manager) %>%
    distinct() %>% 
    mutate(owned = 1) %>%
    pivot_wider(names_from = manager, values_from = owned, values_fill = 0)
  
  # 2. Calculate Similarity (Intersection)
  mat <- as.matrix(matrix_df %>% select(-element))
  
  if(ncol(mat) < 2) return(NULL)
  
  sim_matrix <- t(mat) %*% mat
  mgr_names <- colnames(sim_matrix)
  
  # 3. Create Annotation Grid
  grid <- expand.grid(y = mgr_names, x = mgr_names) 
  grid$val <- mapply(function(r, c) sim_matrix[r, c], grid$y, grid$x)
  
  # 4. Render Heatmap
  plot_ly(
    x = mgr_names, y = mgr_names, z = sim_matrix,
    type = "heatmap",
    colorscale = list(c(0, "#2A0040"), c(1, "#00FF85")), 
    hoverinfo = "text",
    text = matrix(paste0("Shared Players: ", sim_matrix), nrow = nrow(sim_matrix)),
    showscale = FALSE
  ) %>%
    add_annotations(
      x = grid$x, y = grid$y, text = grid$val,
      showarrow = FALSE,
      font = list(color = "white", size = 14, weight = "bold")
    ) %>%
    layout(
      xaxis = list(title = "", tickfont = list(color = "white"), side = "bottom"),
      yaxis = list(title = "", tickfont = list(color = "white"), autorange = "reversed"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l=50, r=50, b=50, t=10)
    ) %>% config(displayModeBar = FALSE)
}

# --- 4. BENCH EFFICIENCY (Unchanged) ---
render_bench_efficiency <- function(picks_df, fpl_data) {
  req(picks_df, fpl_data)
  
  if(!"gw_points" %in% names(picks_df)) return(NULL) 
  
  bench_data <- picks_df %>%
    filter(multiplier == 0) %>%
    left_join(fpl_data %>% select(id, web_name), by = c("element" = "id")) %>%
    group_by(manager) %>%
    summarise(
      total_bench_pts = sum(gw_points, na.rm = TRUE),
      worst_mistake_player = web_name[which.max(gw_points)],
      worst_mistake_pts = max(gw_points, na.rm = TRUE),
      worst_mistake_gw = event[which.max(gw_points)],
      .groups = "drop"
    ) %>%
    arrange(desc(total_bench_pts))
  
  bench_data <- bench_data %>%
    mutate(
      tooltip = paste0(
        "<b>", manager, "</b><br>",
        "Total Bench Pts: ", total_bench_pts, "<br>",
        "Worst Mistake: ", worst_mistake_player, " (", worst_mistake_pts, " pts in GW", worst_mistake_gw, ")"
      )
    )
  
  plot_ly(bench_data) %>%
    add_segments(
      y = ~reorder(manager, total_bench_pts), yend = ~reorder(manager, total_bench_pts),
      x = 0, xend = ~total_bench_pts,
      line = list(color = "#888", width = 1),
      showlegend = FALSE, hoverinfo = "skip"
    ) %>%
    add_markers(
      y = ~reorder(manager, total_bench_pts), x = ~total_bench_pts,
      marker = list(
        color = ~total_bench_pts,
        colorscale = list(c(0, "#00FF85"), c(1, "#E90052")), 
        size = 14,
        line = list(color = "white", width = 2)
      ),
      hoverinfo = "text", hovertext = ~tooltip,
      showlegend = FALSE
    ) %>%
    layout(
      title = list(text = "The Regret Meter (Points Left on Bench)", font = list(size = 14, color = "white")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "Total Points Lost", color = "white", gridcolor = "#333"),
      yaxis = list(title = "", color = "white"),
      font = list(family = "Poppins", color = "white"),
      margin = list(l = 100)
    ) %>% config(displayModeBar = FALSE)
}

# --- 5. TIMELINE OF PAIN  ---
render_bench_timeline <- function(picks_df, fpl_data) {
  req(picks_df, fpl_data)
  if(!"gw_points" %in% names(picks_df)) return(NULL)
  
  # 1. Prepare Data
  timeline_data <- picks_df %>%
    filter(multiplier == 0) %>% # Filter for bench
    group_by(manager, event) %>%
    summarise(
      bench_pts = sum(gw_points, na.rm=TRUE),
      top_benched_id = element[which.max(gw_points)],
      top_benched_pts = max(gw_points, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    filter(bench_pts > 0) %>%
    left_join(fpl_data %>% select(id, web_name), by = c("top_benched_id" = "id"))
  
  # 2. Add "Jitter" / Offset logic
  # Assign a specific numeric offset to each manager based on alphabetical order
  # This creates distinct "lanes" within each gameweek column
  managers <- sort(unique(timeline_data$manager))
  n_mgrs <- length(managers)
  
  # Create a small offset map: e.g., -0.15, -0.05, 0.05, 0.15
  offsets <- seq(from = -0.15, to = 0.15, length.out = n_mgrs)
  manager_offsets <- setNames(offsets, managers)
  
  timeline_data <- timeline_data %>%
    mutate(
      # The plotted X value is Gameweek + Manager's Offset
      plot_x = event + manager_offsets[manager]
    )
  
  # 3. Tooltip
  timeline_data$hover <- paste0(
    "<b>", timeline_data$manager, "</b><br>",
    "GW: ", timeline_data$event, "<br>",
    "Total Bench: ", timeline_data$bench_pts, " pts<br>",
    "Culprit: ", timeline_data$web_name, " (", timeline_data$top_benched_pts, ")"
  )
  
  # 4. Render Bubble Chart
  plot_ly(timeline_data, x = ~plot_x, y = ~bench_pts, color = ~manager,
          type = 'scatter', mode = 'markers',
          marker = list(
            # Adjust sizeref to scale bubbles nicely (standardize size)
            size = ~bench_pts, 
            sizeref = 0.1, 
            sizemode = 'area', 
            opacity = 0.9, # Slightly more opaque
            line = list(width = 1, color = "white")
          ),
          text = ~hover, hoverinfo = "text"
  ) %>%
    layout(
      title = list(text = "Timeline of Pain (Weekly Bench Errors)", font = list(size = 14, color = "white")),
      xaxis = list(
        title = "Gameweek", 
        color = "white", 
        gridcolor = "#333", 
        zeroline = FALSE,
        dtick = 1, # Force integer ticks
        tickmode = "linear"
      ),
      yaxis = list(
        title = "Points on Bench", 
        color = "white", 
        gridcolor = "#333", 
        zeroline = FALSE
      ),
      paper_bgcolor = "rgba(0,0,0,0)", 
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Poppins", color = "white"),
      showlegend = TRUE,
      legend = list(x = 1, y = 1, xanchor = 'right') 
    ) %>% config(displayModeBar = FALSE)
}