
generate_lollipop_chart <- function(filtered_df) {
  
  # 1. Safety Check
  if(nrow(filtered_df) == 0) return(NULL)
  
  # 2. Data Preparation
  # Top 10 Goal Scorers from the current selection
  top_scorers <- filtered_df %>%
    arrange(desc(goals_scored)) %>%
    head(10) %>%
    # Sort ascending so the highest value is at the top of the chart
    arrange(goals_scored)
  
  # Ensure names are factors for correct ordering in the plot
  top_scorers$web_name <- factor(top_scorers$web_name, levels = top_scorers$web_name)
  
  # 3. Plotting
  plot_ly(
    top_scorers, 
    x = ~goals_scored, 
    y = ~web_name, 
    type = 'scatter', 
    mode = 'markers',
    
    # The "Head" of the lollipop (Green Dot)
    marker = list(
      color = "#00FF85", 
      size = 12, 
      line = list(color = "white", width = 1)
    ),
    
    # Tooltip
    hovertext = ~paste0("<b>", web_name, "</b><br>Goals: ", goals_scored),
    hoverinfo = "text"
  ) %>%
    
    # The "Stick" of the lollipop (Cyan Line)
    add_segments(
      x = 0, 
      xend = ~goals_scored, 
      y = ~web_name, 
      yend = ~web_name, 
      line = list(color = "#04F5FF", width = 2), 
      showlegend = FALSE
    ) %>%
    
    # Styling
    layout(
      xaxis = list(
        title = "Goals Scored", 
        color = "#cccccc", 
        showgrid = TRUE, 
        gridcolor = "#333333",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "", 
        color = "white", 
        gridcolor = "#333333"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 100, r = 20, t = 10, b = 40),
      font = list(family = "Poppins", color = "white")
    ) %>%
    config(displayModeBar = FALSE)
}