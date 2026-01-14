library(shiny)
library(bslib)
library(tidyverse)
library(gt)
library(ggplot2)
library(bsicons)
library(gtExtras)
library(dplyr)
library(tidyr)


# --- CRITICAL LOAD ORDER ---
library(httr)      
library(jsonlite)
library(plotly) 

# --- 1. LOAD HELPERS (Background Logic) ---
source("R/03_optimizer.R")
source("R/04_fetch_history.R")
source("R/06_prediction_manager.R")
source("R/10_scout_logic.R")
source("R/09_scout_charts.R")
source("R/07_league_fetcher.R") 

# --- 2. LOAD MODULES (Main Wrappers Only) ---
source("modules/history_chart.R")
source("modules/player_explorer.R")
source("modules/accuracy_chart.R")
source("modules/dream_team.R")
source("modules/fixture_ticker.R")
source("modules/league_analysis.R") 

# --- 3. LOAD DATA ---
fpl_data <- tryCatch({
  readRDS("data/fpl_clean.rds")
}, error = function(e) {
  NULL
})

# --- 4. THEME CONFIGURATION ---
pl_dark_theme <- bs_theme(
  version = 5, 
  bg = "#190028", 
  fg = "#FFFFFF", 
  primary = "#00FF85", 
  secondary = "#E90052", 
  base_font = font_google("Poppins"), 
  heading_font = font_google("Oswald")
)