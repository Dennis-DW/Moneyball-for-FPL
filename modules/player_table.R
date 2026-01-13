library(gt)
library(dplyr)

# --- MAIN TABLE FUNCTION ---
generate_player_table <- function(df) {
  
  if (nrow(df) == 0) return(gt(data.frame(Message = "No players found.")))
  
  # Select Columns
  cols_to_show <- c("player_image", "web_name", "team_name", "position", 
                    "cost", "selected_by_percent", "form", 
                    "value_ratio", "total_points")
  
  if("expected_goal_involvements" %in% names(df)) {
    cols_to_show <- c(cols_to_show, "expected_goal_involvements")
  }
  
  # Generate Table
  t <- df %>%
    select(any_of(cols_to_show)) %>%
    gt() %>%
    
    # --- FORMATTING ---
    fmt_markdown(columns = player_image) %>%
    cols_label(
      player_image = "", web_name = "Player", team_name = "Team", position = "Pos",
      cost = "Price", selected_by_percent = "Owned", form = "Form", 
      value_ratio = "Value", total_points = "Pts", expected_goal_involvements = "xGI"
    ) %>%
    
    # --- WIDTHS (Compact) ---
    cols_width(
      player_image ~ px(40),
      web_name ~ px(110), 
      team_name ~ px(90), 
      position ~ px(50),
      
      # Tighter Data Columns (Requested)
      form ~ px(50),
      value_ratio ~ px(50),
      total_points ~ px(50),
      expected_goal_involvements ~ px(50),
      
      # Remaining columns (Cost/Owned) get default or remaining space
      cost ~ px(60),
      selected_by_percent ~ px(60)
    ) %>%
    
    # --- COLORS ---
    data_color(columns = form, palette = c("#2A0040", "#E90052")) %>%
    data_color(columns = total_points, palette = c("#2A0040", "#00FF85")) %>%
    data_color(columns = value_ratio, palette = c("#2A0040", "#04F5FF")) %>%
    
    # --- STYLING (SUPER COMPACT) ---
    tab_options(
      table.width = pct(100), 
      table.background.color = "#190028", 
      table.font.color = "#FFFFFF",
      table.font.size = px(11),
      
      column_labels.background.color = "#38003C", 
      column_labels.font.weight = "bold",
      column_labels.font.size = px(12),
      
      data_row.padding = px(3),
      
      table.border.top.color = "#444444",
      table_body.hlines.color = "#333333",
      table.border.bottom.color = "#444444"
    ) %>%
    opt_table_font(font = "Poppins") %>%
    fmt_number(columns = any_of(c("cost", "selected_by_percent", "form", "value_ratio", "expected_goal_involvements")), decimals = 1) %>%
    
    # --- INTERACTIVITY ---
    opt_interactive(
      use_search = FALSE, 
      use_filters = FALSE, 
      use_sorting = TRUE, 
      use_highlight = TRUE, 
      use_page_size_select = FALSE,
      page_size_default = 4
    )
  
  return(t)
}