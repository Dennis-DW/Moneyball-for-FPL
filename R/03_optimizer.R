library(tidyverse)
library(lpSolve)

optimize_team <- function(data, budget = 100) {
  
  print(" [Optimizer] Starting 1-3-4-3 Strategy...")
  
  # 1. PREPARE DATA
  df <- data %>%
    filter(chance_of_playing_next_round >= 75 | is.na(chance_of_playing_next_round)) %>%
    filter(!is.na(ep_next)) %>%
    mutate(
      predicted_points = as.numeric(as.character(ep_next)),
      cost = as.numeric(as.character(cost)),
      position = as.character(position),
      # Binary Flags for LP Solver
      is_gkp = ifelse(position == "GKP", 1, 0),
      is_def = ifelse(position == "DEF", 1, 0),
      is_mid = ifelse(position == "MID", 1, 0),
      is_fwd = ifelse(position == "FWD", 1, 0)
    ) %>%
    filter(!is.na(predicted_points), !is.na(cost))
  
  # 2. CONSTRAINTS MATRIX
  # Columns: Players
  # Rows: [Total Count, Budget, GKP, DEF, MID, FWD]
  constraints_matrix <- rbind(
    rep(1, nrow(df)),       # Total players must be 11
    df$cost,                # Total cost must be <= budget
    df$is_gkp,              # GK count
    df$is_def,              # DEF count
    df$is_mid,              # MID count
    df$is_fwd               # FWD count
  )
  
  # 3. SET STRATEGY (1-3-4-3)
  # Directions: "=" means exact, "<=" means max limit
  directions <- c("=", "<=", "=", "=", "=", "=")
  
  # Limits: 11 Players, Budget, 1 GK, 3 DEF, 4 MID, 3 FWD
  limits <- c(11, budget, 1, 3, 4, 3) 
  
  # 4. TEAM RULE (Max 3 per team)
  team_list <- unique(df$team_name)
  for(team in team_list) {
    team_flag <- ifelse(df$team_name == team, 1, 0)
    constraints_matrix <- rbind(constraints_matrix, team_flag)
    directions <- c(directions, "<=")
    limits <- c(limits, 3)
  }
  
  # 5. RUN SOLVER
  # "max" -> Maximize predicted points
  # all.bin = TRUE -> Variables are binary (0 = don't pick, 1 = pick)
  result <- lp("max", df$predicted_points, constraints_matrix, directions, limits, all.bin = TRUE)
  
  if(result$status == 0) {
    best_team <- df[which(result$solution == 1), ]
    print(paste("[Optimizer] 1-3-4-3 Squad Found! Projected Pts:", sum(best_team$predicted_points)))
    return(best_team)
  } else {
    print("❌ [Optimizer] Failed. Could not fit 1-3-4-3 within constraints.")
    return(data.frame()) 
  }
}