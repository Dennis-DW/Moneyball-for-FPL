library(plotly)
library(dplyr)

generate_sunburst_chart <- function(filtered_df) {
  
  # 1. Safety Check
  if(nrow(filtered_df) == 0) return(NULL)
  
  # 2. Data Preparation
  # a single dataframe with 'ids', 'labels', 'parents', 'values', and 'colors'
  # The hierarchy is: Position -> Team -> Player
  
  # --- LEVEL 1: POSITIONS (Roots) ---
  pos_summary <- filtered_df %>%
    group_by(position) %>%
    summarise(
      val = sum(as.numeric(total_points), na.rm=TRUE),
      form_avg = mean(as.numeric(form), na.rm=TRUE)
    ) %>%
    mutate(
      ids = position,
      labels = position,
      parents = "",
      color_val = form_avg
    )
  
  # --- LEVEL 2: TEAMS (Branches) ---
  # Note: A team (e.g., Arsenal) exists in multiple positions (DEF, MID), 
  # so create unique IDs like "DEF-Arsenal"
  team_summary <- filtered_df %>%
    group_by(position, team_name) %>%
    summarise(
      val = sum(as.numeric(total_points), na.rm=TRUE),
      form_avg = mean(as.numeric(form), na.rm=TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      ids = paste(position, team_name, sep = "-"),
      labels = team_name,
      parents = position,
      color_val = form_avg
    )
  
  # --- LEVEL 3: PLAYERS (Leaves) ---
  player_summary <- filtered_df %>%
    mutate(
      ids = paste(position, team_name, web_name, sep="-"), # Unique ID
      labels = paste0(web_name, " (", total_points, ")"),
      parents = paste(position, team_name, sep = "-"),
      val = as.numeric(total_points),
      color_val = as.numeric(form)
    ) %>%
    select(ids, labels, parents, val, color_val)
  
  # --- 3. Combine All Levels ---
  sunburst_data <- bind_rows(
    pos_summary %>% select(ids, labels, parents, val, color_val),
    team_summary %>% select(ids, labels, parents, val, color_val),
    player_summary
  )
  
  # --- 4. Plot ---
  plot_ly(
    sunburst_data,
    ids = ~ids,
    labels = ~labels,
    parents = ~parents,
    values = ~val,
    type = 'sunburst',
    branchvalues = 'total',
    
    # Color Logic: Red (Cold Form) to Green (Hot Form)
    marker = list(
      colors = ~color_val,
      colorscale = list(c(0, "#E90052"), c(0.5, "#FFFF00"), c(1, "#00FF85")),
      cmin = 0,
      cmax = 10, # Cap form coloring at 10.0
      showscale = TRUE,
      colorbar = list(title = "Form", tickfont = list(color = "white"), titlefont = list(color = "white"))
    ),
    
    hoverinfo = "label+value+percent parent",
    textinfo = "label"
  ) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t=0, l=0, r=0, b=0),
      font = list(family = "Poppins", color = "white")
    )
}