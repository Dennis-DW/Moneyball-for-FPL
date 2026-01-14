library(tidyverse)
library(stringr)

clean_fpl_data <- function() {
  # 1. Load Raw data
  if (!file.exists("data/fpl_raw.rds")) stop("No raw data found")
  
  raw <- readRDS("data/fpl_raw.rds")
  print("cleaning data...")
  
  # --- GET CURRENT GAMEWEEK ---
  current_gw_num <- raw$events %>% 
    filter(is_current == TRUE) %>% 
    pull(id)
  
  if(length(current_gw_num) == 0) {
    current_gw_num <- raw$events %>% 
      filter(is_next == TRUE) %>% 
      pull(id)
  }
  
  if(length(current_gw_num) == 0) current_gw_num <- max(raw$events$id)
  
  print(paste("Detected Gameweek:", current_gw_num))
  
  # --- PROCESS TEAMS ---
  teams_clean <- raw$teams %>% 
    select(id, team_name = name, short_name, strength)
  
  # --- PROCESS PLAYERS ---
  players_clean <- raw$elements %>% 
    select(
      id,
      web_name,
      first_name,
      second_name,
      team_code = team,
      position_id = element_type,
      now_cost,
      
      # Core Stats
      total_points,
      event_points,
      points_per_game,          
      form,
      ep_next,
      
      # Underlying Data
      minutes,                  
      goals_scored,             
      assists,                  
      clean_sheets,             
      expected_goals,           
      expected_assists,         
      expected_goal_involvements, 
      ict_index,
      
      # --- ADDED THESE CRITICAL STATS ---
      influence,
      creativity,
      threat,
      # ----------------------------------
      
      # Market & Status
      selected_by_percent,
      transfers_in_event,       
      chance_of_playing_next_round,
      chance_of_playing_this_round, 
      news,                     
      photo
    ) %>% 
    mutate(
      cost = now_cost / 10,
      form = as.numeric(form),
      ep_next = as.numeric(ep_next),
      points_per_game = as.numeric(points_per_game),
      selected_by_percent = as.numeric(selected_by_percent),
      expected_goals = as.numeric(expected_goals),
      expected_assists = as.numeric(expected_assists),
      ict_index = as.numeric(ict_index),
      transfers_in_event = as.numeric(transfers_in_event),
      
      # --- CONVERT STRINGS TO NUMERIC ---
      influence = as.numeric(influence),
      creativity = as.numeric(creativity),
      threat = as.numeric(threat),
      # ----------------------------------
      
      # Map Position IDs
      position = case_when(
        position_id == 1 ~ "GKP",
        position_id == 2 ~ "DEF",
        position_id == 3 ~ "MID",
        position_id == 4 ~ "FWD"
      ),
      
      photo_url = paste0(
        "https://resources.premierleague.com/premierleague/photos/players/110x140/p",
        str_replace(photo, ".jpg", ".png")
      ),
      
      chance_of_playing = ifelse(
        is.na(chance_of_playing_next_round),
        100,
        chance_of_playing_next_round
      )
    )
  
  # --- FINALIZE ---
  final_df <- players_clean %>% 
    left_join(teams_clean, by = c("team_code" = "id")) %>% 
    mutate(event = current_gw_num) %>% 
    select(
      id,
      web_name,
      team_name,
      position,
      cost,
      form,
      total_points,
      points_per_game,      
      ep_next,
      event,
      minutes,              
      goals_scored,         
      assists,              
      clean_sheets,         
      expected_goals,       
      expected_assists,     
      ict_index,
      
      # --- ENSURE THESE ARE IN THE FINAL DATA ---
      influence,
      creativity,
      threat,
      # ------------------------------------------
      
      transfers_in_event,   
      news,                 
      everything()
    )
  
  # --- SAVE ---
  saveRDS(final_df, "data/fpl_clean.rds")
  print(paste("Success! Saved", nrow(final_df), "players for GW", current_gw_num))
  return(final_df)
}