# --- THEME CONSTANTS ---
BG_COLOR <- "#2A0040"
NEON_CYAN <- "#04F5FF"
NEON_GREEN <- "#00FF85"
TEXT_COLOR <- "#FFFFFF"
GRID_COLOR <- "#444444"

# --- 1. RENDER SKILL BARS ---
chart_bars <- function(data) {
  data$metric <- factor(data$metric, levels = rev(c("Overall", "Attacking", "Shooting", "Passing", "Vision", "Defense")))
  
  ggplot(data, aes(x = metric, y = score)) +
    geom_col(aes(y = 1), fill = "#333333", width = 0.6) +
    geom_col(fill = NEON_CYAN, width = 0.6) +
    geom_text(aes(label = paste0(round(score * 100), "%"), y = score + 0.02), 
              color = "white", size = 3.5, hjust = 0, fontface = "bold") +
    coord_flip() +
    ylim(0, 1.15) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = BG_COLOR, color = BG_COLOR),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      panel.grid = element_blank(),
      axis.text.y = element_text(color = "white", size = 11, face = "bold", margin = margin(r = 15)),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(20, 30, 20, 10)
    )
}

# --- 2. RENDER HISTORY ---
chart_history <- function(data) {
  data$pt_col <- ifelse(data$points >= 10, NEON_GREEN, ifelse(data$points >= 3, NEON_CYAN, "#666666"))
  avg_pts <- mean(data$points, na.rm = TRUE)
  data$tooltip_text <- paste0("<b>GW ", data$gameweek, "</b><br>Points: ", data$points, "<br>Mins: ", data$minutes)
  
  ggplot(data, aes(x = gameweek, y = points, text = tooltip_text)) +
    geom_hline(yintercept = avg_pts, linetype = "dashed", color = GRID_COLOR) +
    geom_segment(aes(x = gameweek, xend = gameweek, y = 0, yend = points), color = "white", alpha = 0.8) +
    geom_point(aes(color = pt_col), size = 4) +
    scale_color_identity() + 
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = BG_COLOR, color = BG_COLOR),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      text = element_text(color = TEXT_COLOR),
      axis.text = element_text(color = "#AAAAAA", size = 10),
      panel.grid.major = element_line(color = "#333333"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(20, 20, 10, 10)
    ) + labs(x = "Gameweek", y = NULL)
}

# --- 3. RENDER VIOLIN PLOT (Faceted & Styled) ---
chart_violin <- function(data) {
  target_data <- data %>% filter(is_target == TRUE)
  
  ggplot(data, aes(x = "", y = value)) +
    
    # 1. Violin Shape (Sleek Purple)
    geom_violin(fill = "#4B0070", color = NA, alpha = 0.8, trim = FALSE) +
    
    # 2. Jitter Points (Subtle Context)
    geom_jitter(color = "white", alpha = 0.1, width = 0.15, size = 1) +
    
    # 3. Target Player (Glowing Ring Effect)
    geom_point(data = target_data, color = "white", size = 5) +       # Outer Ring
    geom_point(data = target_data, color = NEON_CYAN, size = 3) +     # Inner Dot
    
    # 4. Label (Floating above point)
    geom_text(data = target_data, aes(label = value), color = "white", vjust = -2, fontface = "bold", size = 4) +
    
    # 5. Faceting (CRITICAL: Scales = free allows CS and xG to coexist)
    facet_wrap(~metric, scales = "free", nrow = 1) +
    
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = BG_COLOR, color = BG_COLOR),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      text = element_text(color = TEXT_COLOR),
      
      # Hide X-axis completely
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      
      # Header Styling
      strip.text = element_text(color = NEON_CYAN, face = "bold", size = 11, margin = margin(b = 10)),
      strip.background = element_blank(),
      
      plot.margin = margin(20, 20, 20, 20)
    )
}