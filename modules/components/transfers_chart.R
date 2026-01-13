# modules/components/transfers_chart.R

# --- 1. HELPER: Fetch Points for a Specific GW ---
# still need this to look up the actual scores of the players involved in the transfer.
fetch_gw_points_lookup <- function(gw) {
  url <- paste0("https://fantasy.premierleague.com/api/event/", gw, "/live/")
  
  tryCatch({
    resp <- GET(url, timeout(10))
    if (status_code(resp) != 200) return(NULL)
    
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    # Extract points if available
    if ("stats" %in% names(data$elements)) {
      data$elements$stats %>%
        select(total_points) %>%
        mutate(id = data$elements$id) %>%
        rename(points = total_points)
    } else {
      NULL
    }
  }, error = function(e) {
    return(NULL)
  })
}

# --- 2. MAIN RENDERER ---
render_transfer_efficiency <- function(transfers_df, fpl_data) {
  req(transfers_df, fpl_data)
  
  # Ensure we have data to work with
  if (nrow(transfers_df) == 0) return(NULL)
  
  # A. PREPARE POINTS MAP
  # identify which Gameweeks are involved to minimize API calls
  unique_gws <- sort(unique(transfers_df$event))
  gw_points_map <- list()
  
  withProgress(message = 'Scoring Market Moves...', value = 0, {
    n_gws <- length(unique_gws)
    
    for(j in seq_along(unique_gws)) {
      gw <- unique_gws[j]
      incProgress(1 / n_gws, detail = paste("Analysing GW", gw))
      
      pts <- fetch_gw_points_lookup(gw)
      if(!is.null(pts)) {
        gw_points_map[[as.character(gw)]] <- pts
      }
    }
  })
  
  # B. CALCULATE NET SCORES (IN vs OUT)
  # Helper function to look up points from our downloaded map
  get_pts <- function(ids, events) {
    mapply(function(id, gw) {
      p_df <- gw_points_map[[as.character(gw)]]
      if(is.null(p_df)) return(0)
      
      val <- p_df$points[p_df$id == id]
      if(length(val) == 0) return(0)
      return(val)
    }, ids, events)
  }
  
  # Apply the lookup
  transfers_df$pts_in <- get_pts(transfers_df$element_in, transfers_df$event)
  transfers_df$pts_out <- get_pts(transfers_df$element_out, transfers_df$event)
  
  # C. ENRICH WITH PLAYER NAMES
  transfers_df <- transfers_df %>%
    left_join(fpl_data %>% select(id, name_in = web_name), by = c("element_in" = "id")) %>%
    left_join(fpl_data %>% select(id, name_out = web_name), by = c("element_out" = "id"))
  
  # D. AGGREGATE BY MANAGER
  analysis <- transfers_df %>%
    mutate(net_gain = pts_in - pts_out) %>%
    group_by(manager) %>%
    summarise(
      total_gain = sum(net_gain, na.rm = TRUE),
      transfers_made = n(),
      
      # Identify the single best transfer move
      best_move_gain = max(net_gain, na.rm = TRUE),
      best_in = name_in[which.max(net_gain)],
      best_out = name_out[which.max(net_gain)],
      .groups = "drop"
    ) %>%
    mutate(
      best_move_desc = paste0(best_in, " for ", best_out, " (+", best_move_gain, ")")
    ) %>%
    arrange(desc(total_gain))
  
  # E. RENDER CHART
  # Color logic: Green for positive gain, Red for negative loss
  analysis$color <- ifelse(analysis$total_gain >= 0, "#00FF85", "#E90052")
  
  plot_ly(analysis) %>%
    add_trace(
      x = ~total_gain,
      y = ~reorder(manager, total_gain),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~color, 
        line = list(color = "white", width = 1)
      ),
      text = ~paste0(ifelse(total_gain > 0, "+", ""), total_gain),
      textposition = "auto",
      textfont = list(color = "white", weight = "bold"),
      hoverinfo = "text",
      hovertext = ~paste0(
        "<b>", manager, "</b><br>",
        "Net Transfer Pts: ", total_gain, "<br>",
        "Moves Made: ", transfers_made, "<br>",
        "Best Move: ", best_move_desc
      )
    ) %>%
    layout(
      title = list(text = "Transfer Market Efficiency (Immediate Impact)", font = list(size = 14, color = "white")),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "Net Points Gained/Lost", color = "white", gridcolor = "#333"),
      yaxis = list(title = "", color = "white"),
      font = list(family = "Poppins", color = "white"),
      margin = list(l = 100),
      showlegend = FALSE
    ) %>% config(displayModeBar = FALSE)
}