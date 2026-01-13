library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# --- 1. GET LEAGUE STANDINGS ---
get_league_standings_rds <- function(league_id, force_refresh = FALSE) {
  file_path <- paste0("data/league_standings_", league_id, ".rds")
  
  if (file.exists(file_path) && !force_refresh) {
    return(readRDS(file_path))
  }
  
  url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/", league_id, "/standings/")
  
  tryCatch({
    resp <- GET(url, timeout(10))
    stopifnot(status_code(resp) == 200)
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    standings <- data$standings$results %>%
      select(rank, entry, player_name, entry_name, total, event_total) %>%
      rename(manager_id = entry, points = total, gw_points = event_total)
    
    output <- list(data = standings, league_name = data$league$name, updated = Sys.time())
    
    if (!dir.exists("data")) dir.create("data")
    saveRDS(output, file_path)
    return(output)
  }, error = function(e) {
    message("Standings fetch failed: ", e$message)
    return(NULL)
  })
}

# --- 2. GET LEAGUE HISTORY ---
get_league_history_rds <- function(league_id, standings_df, force_refresh = FALSE) {
  file_path <- paste0("data/league_history_", league_id, ".rds")
  
  if (file.exists(file_path) && !force_refresh) {
    return(readRDS(file_path))
  }
  
  all_history <- list()
  tryCatch({
    for (i in seq_along(standings_df$manager_id)) {
      mgr_id <- standings_df$manager_id[i]
      mgr_name <- standings_df$player_name[i]
      
      resp <- GET(paste0("https://fantasy.premierleague.com/api/entry/", mgr_id, "/history/"), timeout(10))
      if (status_code(resp) == 200) {
        h <- fromJSON(content(resp, "text", encoding = "UTF-8"))
        if(!is.null(h$current)) {
          all_history[[i]] <- h$current %>%
            select(event, total_points, points, rank) %>%
            mutate(manager = mgr_name, manager_id = mgr_id)
        }
      }
      Sys.sleep(0.1)
    }
    output <- list(data = bind_rows(all_history), updated = Sys.time())
    saveRDS(output, file_path)
    return(output)
  }, error = function(e) { return(NULL) })
}

# --- 3. GET LEAGUE TRANSFERS (NEW: For Transfer Efficiency Chart) ---
get_league_transfers_rds <- function(league_id, standings_df, force_refresh = FALSE) {
  file_path <- paste0("data/league_transfers_", league_id, ".rds")
  
  if (file.exists(file_path) && !force_refresh) {
    return(readRDS(file_path))
  }
  
  all_transfers <- list()
  message("🔄 Fetching transfer history for all managers...")
  
  tryCatch({
    for (i in seq_along(standings_df$manager_id)) {
      mgr_id <- standings_df$manager_id[i]
      mgr_name <- standings_df$player_name[i]
      
      # Fetch full transfer history for manager
      url <- paste0("https://fantasy.premierleague.com/api/entry/", mgr_id, "/transfers/")
      resp <- GET(url, timeout(10))
      
      if (status_code(resp) == 200) {
        t_data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
        
        # Check if they have made transfers
        if (length(t_data) > 0 && is.data.frame(t_data)) {
          all_transfers[[i]] <- t_data %>%
            select(event, element_in, element_out, time) %>%
            mutate(manager = mgr_name, manager_id = mgr_id)
        }
      }
      Sys.sleep(0.1) # Be polite to API
    }
    
    # Bind all rows
    final_df <- bind_rows(all_transfers)
    
    if (nrow(final_df) == 0) return(NULL)
    
    output <- list(data = final_df, updated = Sys.time())
    if (!dir.exists("data")) dir.create("data")
    saveRDS(output, file_path)
    return(output)
    
  }, error = function(e) { 
    message("Transfer fetch failed: ", e$message)
    return(NULL) 
  })
}

# --- 4. GET ALL LEAGUE PICKS (FULL SEASON + POINTS) ---
get_league_picks_rds <- function(league_id, standings_df, history_df, force_refresh = FALSE) {
  
  # 1. Determine "Latest Active GW"
  latest_gw <- max(history_df$event, na.rm = TRUE)
  if(!is.finite(latest_gw)) return(NULL)
  
  # 2. Check for Cumulative Cache
  file_path <- paste0("data/league_picks_cumulative_", league_id, ".rds")
  cached_data <- NULL
  
  if (file.exists(file_path)) {
    cached_obj <- readRDS(file_path)
    cached_data <- cached_obj$data
    last_fetched_gw <- max(cached_data$event, na.rm = TRUE)
    
    # Check if we need to refresh
    if (!force_refresh && last_fetched_gw >= latest_gw) {
      message("✅ Using cached picks up to GW ", last_fetched_gw)
      return(cached_obj)
    }
    message("🔄 Updating picks cache (Current: ", last_fetched_gw, " -> Target: ", latest_gw, ")")
  } else {
    message("📡 Fetching full picks history (GW 1 to ", latest_gw, ")...")
    last_fetched_gw <- 0
  }
  
  # 3. Fetch only MISSING Gameweeks
  new_picks_list <- list()
  gws_to_fetch <- (last_fetched_gw + 1):latest_gw
  
  if(length(gws_to_fetch) > 0) {
    for (gw in gws_to_fetch) {
      message("   ... fetching GW ", gw)
      
      # A. FETCH LIVE POINTS FOR THIS GW (Lookup Table)
      live_url <- paste0("https://fantasy.premierleague.com/api/event/", gw, "/live/")
      gw_player_points <- NULL
      
      tryCatch({
        l_resp <- GET(live_url, timeout(10))
        if (status_code(l_resp) == 200) {
          l_data <- fromJSON(content(l_resp, "text", encoding = "UTF-8"))
          if (!is.null(l_data$elements)) {
            if("stats" %in% names(l_data$elements)) {
              gw_player_points <- l_data$elements$stats %>%
                select(total_points) %>%
                mutate(id = l_data$elements$id) 
            }
          }
        }
      }, error = function(e) { message("     Warning: Could not fetch live points for GW ", gw) })
      
      # B. FETCH MANAGER PICKS
      for (i in seq_along(standings_df$manager_id)) {
        mgr_id <- standings_df$manager_id[i]
        mgr_name <- standings_df$player_name[i]
        if (is.na(mgr_id)) next
        
        tryCatch({
          url <- paste0("https://fantasy.premierleague.com/api/entry/", mgr_id, "/event/", gw, "/picks/")
          resp <- GET(url, timeout(5))
          
          if (status_code(resp) == 200) {
            p_data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
            if (!is.null(p_data$picks)) {
              
              # Base Picks DF
              df <- p_data$picks %>%
                select(element, multiplier, is_captain, is_vice_captain) %>%
                mutate(
                  manager = mgr_name, 
                  manager_id = mgr_id, 
                  event = gw
                )
              
              # Merge Points if available
              if (!is.null(gw_player_points)) {
                df <- df %>%
                  left_join(gw_player_points, by = c("element" = "id")) %>%
                  rename(gw_points = total_points) # Store as gw_points
              } else {
                df$gw_points <- 0 # Fallback
              }
              
              new_picks_list[[paste(gw, mgr_id)]] <- df
            }
          }
        }, error = function(e) { NULL })
      }
      Sys.sleep(0.1) # Be polite to API
    }
  }
  
  # 4. Merge New Data with Cache
  new_data <- bind_rows(new_picks_list)
  
  if (!is.null(cached_data)) {
    if (!"gw_points" %in% names(cached_data)) cached_data$gw_points <- NA
    final_df <- bind_rows(cached_data, new_data) %>% distinct(manager_id, event, element, .keep_all = TRUE)
  } else {
    final_df <- new_data
  }
  
  if (nrow(final_df) == 0) return(NULL)
  
  output <- list(data = final_df, updated = Sys.time())
  if (!dir.exists("data")) dir.create("data")
  saveRDS(output, file_path)
  
  return(output)
}