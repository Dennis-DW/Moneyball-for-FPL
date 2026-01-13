# modules/components/league_table.R

render_league_standings_table <- function(standings, history) {
  
  standings <- as.data.frame(standings)
  history   <- as.data.frame(history)
  
  req(nrow(standings) > 0, nrow(history) > 0)
  
  # Determine latest gameweek
  latest_gw <- suppressWarnings(max(history$event, na.rm = TRUE))
  req(is.finite(latest_gw))
  
  # Calculate Rank Change (Up/Down Arrows)
  rank_logic <- history %>%
    group_by(event) %>%
    mutate(gw_rank = rank(-total_points, ties.method = "first")) %>%
    ungroup() %>%
    filter(event %in% c(latest_gw, latest_gw - 1)) %>%
    group_by(manager) %>%
    summarise(
      current_rank = gw_rank[event == latest_gw],
      prev_rank = {
        r <- gw_rank[event == (latest_gw - 1)]
        if (length(r) == 0) current_rank else r
      },
      change = prev_rank - current_rank,
      .groups = "drop"
    ) %>%
    mutate(
      change = ifelse(is.na(change), 0, change),
      change_icon = case_when(
        change > 0 ~ paste0("↑ ", change),
        change < 0 ~ paste0("↓ ", abs(change)),
        TRUE       ~ "–"
      )
    )
  
  # Calculate Form (Last 5 GWs Sparkline)
  form_data <- history %>%
    arrange(manager, event) %>%
    group_by(manager) %>%
    mutate(gw_points = total_points - lag(total_points, default = first(total_points))) %>%
    slice_tail(n = 5) %>%
    summarise(Form = list(gw_points), .groups = "drop")
  
  # Build and Style the Table
  standings %>%
    left_join(rank_logic, by = c("player_name" = "manager")) %>%
    left_join(form_data,  by = c("player_name" = "manager")) %>%
    mutate(Form = ifelse(is.na(Form), list(numeric()), Form)) %>%
    select(rank, change_icon, change, player_name, entry_name, points, Form) %>%
    gt() %>%
    gt_plt_sparkline(
      Form,
      type = "shaded",
      palette = c("white", "white", "#00FF85", "#E90052", "white"),
      label = FALSE
    ) %>%
    # Color logic for Up/Down arrows
    tab_style(
      cell_text(color = "#00FF85", weight = "bold"),
      cells_body(change_icon, rows = change > 0)
    ) %>%
    tab_style(
      cell_text(color = "#E90052", weight = "bold"),
      cells_body(change_icon, rows = change < 0)
    ) %>%
    tab_style(
      cell_text(color = "#888888"),
      cells_body(change_icon, rows = change == 0)
    ) %>%
    cols_hide(change) %>%
    cols_label(
      rank = "#",
      change_icon = "±",
      player_name = "Manager",
      entry_name = "Team",
      points = "Total"
    ) %>%
   
    tab_options(
      table.background.color = "#190028",
      column_labels.background.color = "#38003C",
      column_labels.border.bottom.color = "#00FF85",
      column_labels.border.bottom.width = px(2),
      
      # FONT SIZE REDUCED
      table.font.size = px(12),   
      column_labels.font.size = px(13), 
      
      # PADDING REDUCED
      data_row.padding = px(5)    
    ) %>%
    opt_table_font(font = "Poppins")
}