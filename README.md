# EPL Fantasy Dashboard 

![Dashboard Cover](www/cover_image.png)


A professional-grade R Shiny dashboard for Fantasy Premier League (FPL) managers. This tool leverages the FPL API, Linear Programming optimization, and advanced data visualization to provide a competitive edge.

##  Features

### 1.  Dream Team Optimizer
- **Algorithm**: Uses `lpSolve` for linear optimization.
- **Strategy**: Generates the optimal "1-3-4-3" squad based on predicted points (`ep_next`) and budget constraints.
- **Constraints**: Handles budget caps, max 3 players per team, and formation rules.

### 2.  Player Explorer
- **Deep Dive**: Filter players by Team, Position, Availability, and Price.
- **Advanced Metrics**: Visualize `xGI` (Expected Goal Involvement), `ICT Index`, and Form.
- **Visuals**: Radar charts for attribute comparison and Sunburst charts for points distribution.

### 3.  League Analysis
- **Mini-League Tracking**: Analyze your private leagues.
- **Rival Watch**: Track live standings, transfer activity, and captaincy choices of your rivals.
- **"Regret Meter"**: Visualizes points left on the bench for every manager in the league.
- **Transfer Efficiency**: Calculates the net point gain/loss from transfer market moves.

### 4.  Fixture Ticker
- **Difficulty Matrix**: Color-coded view of the next 5 gameweeks.
- **FDR**: Uses official Fixture Difficulty Ratings to highlight easy/hard runs.

### 5.  Performance Tracking
- **Accuracy Check**: Compares the Optimizer's predictions vs. Actual results to track model performance over time.
- **Season History**: Interactive sparklines and violin plots to compare player consistency against the league average.

##  Installation & Setup

### 1. Clone the Repository
    ```bash
    git clone https://github.com/Dennis-DW/Moneyball-for-FPL
    ```

### 2. Install R Dependencies
    Run the following R code to install dependencies:
    ```r
    install.packages(c(
      "shiny", "bslib", "tidyverse", "gt", "ggplot2", 
      "bsicons", "gtExtras", "dplyr", "tidyr", "httr", 
      "jsonlite", "plotly", "lpSolve", "scales", "DT"
    ))
    ```

### 3. Configure League ID
To analyze **your specific mini-league**, you must update the League ID:
1. Open `modules/league_analysis_server.R`.
2. Find the line: `target_id <- "1167592"`.
3. Replace `"1167592"` with your FPL League ID (found in the URL of your league standings page).

##  Usage Workflow

1.  **Update Data**:
    Fetch the latest stats from the FPL API before every Gameweek.
    ```r
    source("update_data.R")
    ```
    *This creates/updates `.rds` files in the `data/` folder.*

2.  **Launch Dashboard**:
    ```r
    shiny::runApp()
    ```

##  Project Structure

- **`global.R` / `server.R` / `ui.R`**: Core Shiny application files.
- **`R/`**: Backend scripts for data fetching (`01_fetch_data.R`), cleaning (`02_clean_data.R`), and optimization (`03_optimizer.R`).
- **`modules/`**: UI/Server modules for specific tabs (e.g., `dream_team.R`, `player_explorer.R`).
- **`data/`**: Local storage for API data (ignored by Git).
- **`www/`**: Custom assets (CSS, images).