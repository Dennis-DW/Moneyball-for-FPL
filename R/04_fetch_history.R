library(httr)
library(jsonlite)
library(tidyverse)

# Function to fetch history for a SINGLE player
fetch_player_history <- function(player_id) {
  
  # 1. Construct URL
  url <- paste0("https://fantasy.premierleague.com/api/element-summary/", player_id, "/")
  
  # 2. "Safe Mode" Fetching
  tryCatch({
    
    # Set a timeout of 10 seconds
    resp <- GET(url, timeout(10))
    
    if (status_code(resp) != 200) {
      warning(paste("API Error: Status", status_code(resp)))
      return(data.frame()) 
    }
    
    # 3. Parse Data
    json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if (!is.null(json$history)) {
      history <- json$history %>%
        select(
          gameweek = round,
          points = total_points,
          opponent_code = opponent_team,
          value = value,
          minutes = minutes
        ) %>%
        mutate(player_id = player_id)
      
      return(history)
    } else {
      return(data.frame()) 
    }
    
  }, error = function(e) {
    message(paste("⚠️ Network Error caught:", e$message))
    return(data.frame()) 
  })
}
