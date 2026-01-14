
# --- 1. GET NEXT 5 FIXTURES ---
logic_get_fixtures <- function(player_team_id, fixtures_df) {
  if(is.null(fixtures_df)) return(data.frame(label="No Data", difficulty=3))
  
  fixtures_df %>%
    filter((team_h == player_team_id | team_a == player_team_id), finished == FALSE) %>%
    head(5) %>%
    mutate(
      is_home = (team_h == player_team_id),
      difficulty = ifelse(is_home, team_h_difficulty, team_a_difficulty),
      label = paste0(ifelse(is_home, "(H)", "(A)")) 
    ) %>%
    select(label, difficulty)
}

# --- 2. PREP SKILL BAR DATA ---
logic_get_radar <- function(player_id, all_players_df) {
  target <- all_players_df %>% filter(id == player_id)
  if(nrow(target) == 0) return(NULL)
  
  pos <- target$position
  peers <- all_players_df %>% 
    filter(position == pos, minutes > 0) %>%
    select(id, goals = goals_scored, assists, influence, creativity, threat, ict_index) %>%
    mutate(across(-id, as.numeric))
  
  ranked <- peers %>%
    mutate(across(-id, percent_rank)) %>%
    filter(id == player_id) %>%
    pivot_longer(cols = -id, names_to = "metric", values_to = "score") %>%
    mutate(metric = recode(metric,
                           "goals" = "Shooting", "assists" = "Passing", "influence" = "Defense",
                           "creativity" = "Vision", "threat" = "Attacking", "ict_index" = "Overall"
    ))
  
  return(ranked)
}

# --- 3. FIND SIMILAR PLAYERS ---
logic_get_similar <- function(player_id, all_players_df) {
  target <- all_players_df %>% filter(id == player_id)
  if(nrow(target) == 0) return(NULL)
  
  target_cost <- as.numeric(target$now_cost)
  pos <- target$position
  
  candidates <- all_players_df %>%
    filter(position == pos, id != player_id, minutes > 0) %>%
    mutate(
      cost = as.numeric(now_cost),
      inf = as.numeric(influence),
      cre = as.numeric(creativity),
      thr = as.numeric(threat)
    ) %>%
    filter(cost <= target_cost)
  
  if(nrow(candidates) == 0) return(NULL)
  
  tgt_stats <- c(as.numeric(target$influence), as.numeric(target$creativity), as.numeric(target$threat))
  
  candidates$dist <- apply(candidates[, c("inf", "cre", "thr")], 1, function(x) {
    sum((x - tgt_stats)^2)
  })
  
  candidates %>% 
    arrange(dist) %>% 
    head(3) %>% 
    mutate(
      clean_code = str_remove(photo, "\\.jpg"),
      photo_url = paste0("https://resources.premierleague.com/premierleague/photos/players/40x40/p", clean_code, ".png")
    ) %>%
    select(web_name, team_name, cost, photo_url)
}

# --- 4. CALCULATE RANK ---
logic_get_rank <- function(player_id, all_players_df) {
  target <- all_players_df %>% filter(id == player_id)
  if(nrow(target) == 0) return(list(ict_rank = "-", total = 0))
  
  pos <- target$position
  
  peers <- all_players_df %>% 
    filter(position == pos) %>%
    mutate(ict_index = as.numeric(ict_index)) %>%
    arrange(desc(ict_index)) %>%
    mutate(rank = row_number())
  
  p_rank <- peers %>% filter(id == player_id) %>% pull(rank)
  total <- nrow(peers)
  
  return(list(ict_rank = p_rank, total = total))
}

# --- 5. VIOLIN DATA PREP (Updated: CS instead of Mins) ---
logic_get_violin <- function(player_id, all_players_df) {
  target <- all_players_df %>% filter(id == player_id)
  if(nrow(target) == 0) return(NULL)
  
  pos <- target$position
  
  # Get Peers (Same Position)
  peers <- all_players_df %>%
    filter(position == pos, minutes > 0) %>%
    select(id, web_name, 
           expected_goals, expected_assists, expected_goal_involvements, 
           clean_sheets, goals_scored, assists) %>%
    mutate(
      xG = as.numeric(expected_goals),
      xA = as.numeric(expected_assists),
      xGI = as.numeric(expected_goal_involvements),
      CS = as.numeric(clean_sheets),
      Goals = as.numeric(goals_scored),
      Assists = as.numeric(assists),
      is_target = (id == player_id)
    ) %>%
    # SAFETY FIX: Replace NA with 0 to prevent plotting errors
    mutate(across(c(xG, xA, xGI, CS, Goals, Assists), ~replace_na(., 0))) %>%
    pivot_longer(cols = c("xG", "xA", "xGI", "CS", "Goals", "Assists"), 
                 names_to = "metric", values_to = "value")
  
  # Factor Order: CS first (Defense), then Attacking stats
  peers$metric <- factor(peers$metric, levels = c("CS", "Goals", "Assists", "xG", "xA", "xGI"))
  
  return(peers)
}