library(httr)
library(jsonlite)
library(tidyverse)

# function to fetch raw data from EPL
fetch_fpl_data <- function(){
  print("connecting to FPL API...")
  
  # 1. Main endpoint (players, team, events)
  url_static <- "https://fantasy.premierleague.com/api/bootstrap-static/"
  resp_static <- GET(url_static)
  json_static <- fromJSON(content(resp_static, "text"))
  
  # 2. fixtures endpoint (schedule & Difficulty)
  url_fixtures <- "https://fantasy.premierleague.com/api/fixtures/"
  resp_fixtures <- GET(url_fixtures)
  json_fixtures <- fromJSON(content(resp_fixtures, "text"))
  
  print("Data fetched successfully")
  
  return(list(
    elements=json_static$elements,
    teams=json_static$teams,
    events= json_static$events,
    fixtures=json_fixtures
  ))
}

# function save data locally
update_local_data <- function(){
  data <- fetch_fpl_data()
  
  if (!dir.exists("data")) dir.create("data")
  saveRDS(data, file = "data/fpl_raw.rds")
  print("Data saved to data/fpl_raw.rds")
}