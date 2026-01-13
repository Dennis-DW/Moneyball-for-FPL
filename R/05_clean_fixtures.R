library(tidyverse)

clean_fixture_data <- function() {
  if (!file.exists("data/fpl_raw.rds")) stop("No raw data found")
  raw <- readRDS("data/fpl_raw.rds")
  
  # --- FIND NEXT UNPLAYED GAMEWEEK ---
  # look for the first event where finished is FALSE
  next_gw_num <- raw$events %>% 
    filter(finished == FALSE) %>% 
    slice_head(n = 1) %>% 
    pull(id)
  
  # Fallback if season is over
  if(length(next_gw_num) == 0) next_gw_num <- max(raw$events$id)
  
  print(paste("Generating Ticker starting from GW:", next_gw_num))
  
  # --- PROCESS FIXTURES ---
  fixtures <- raw$fixtures %>%
    filter(event >= next_gw_num) %>% 
    select(event, team_h, team_a, team_h_difficulty, team_a_difficulty)
  
  # Create Perspectives
  home_games <- fixtures %>%
    select(event, team = team_h, opponent = team_a, difficulty = team_h_difficulty) %>%
    mutate(is_home = TRUE)
  
  away_games <- fixtures %>%
    select(event, team = team_a, opponent = team_h, difficulty = team_a_difficulty) %>%
    mutate(is_home = FALSE)
  
  # Combine & Label
  full_schedule <- bind_rows(home_games, away_games) %>%
    left_join(raw$teams %>% select(id, name, short_name), by = c("team" = "id")) %>%
    left_join(raw$teams %>% select(id, opp_short_name = short_name), by = c("opponent" = "id")) %>%
    arrange(team, event) %>%
    mutate(opp_label = paste0(opp_short_name, " (", ifelse(is_home, "H", "A"), ")"))
  
  # --- PIVOT TO WIDE FORMAT ---
  ticker_clean <- full_schedule %>%
    group_by(name) %>%
    slice_head(n = 5) %>% # Take next 5 games
    mutate(game_seq = row_number()) %>% 
    ungroup() %>%
    select(name, game_seq, opp_label, difficulty)
  
  ticker_wide <- ticker_clean %>%
    pivot_wider(
      id_cols = name,
      names_from = game_seq,
      values_from = c(opp_label, difficulty),
      names_sep = "_"
    )
  
  saveRDS(ticker_wide, "data/fixture_ticker.rds")
  print("Success! Ticker data saved.")
  return(ticker_wide)
}