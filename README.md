# EPL Fantasy Dashboard

A comprehensive R Shiny dashboard for Fantasy Premier League (FPL) managers. This tool provides advanced analytics, squad optimization, and league tracking capabilities to help you make data-driven decisions.

## Features

- **Player Explorer**: Filter and sort players by form, price, position, and underlying stats (xGI, ICT Index). Includes visual profiles and fixture tickers.
- **Dream Team Optimizer**: Uses Linear Programming (`lpSolve`) to generate the optimal squad for the upcoming gameweek based on predicted points and budget constraints (1-3-4-3 formation strategy).
- **League Analysis**: Deep dive into mini-leagues, featuring:
    - Live standings with form sparklines.
    - Transfer market efficiency analysis.
    - Captaincy decision tracking.
    - "Regret Meter" for points left on the bench.
- **Fixture Ticker**: Visualizes fixture difficulty for the next 5 gameweeks.
- **Season History**: Interactive charts tracking player performance over the season.
- **Accuracy Check**: Tracks the performance of the optimizer's predictions against actual results.

## Installation

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Dennis-DW/Moneyball-for-FPL
    ```

2.  **Install required R packages:**
    Run the following R code to install dependencies:
    ```r
    install.packages(c(
      "shiny", "bslib", "tidyverse", "gt", "ggplot2", 
      "bsicons", "gtExtras", "dplyr", "tidyr", "httr", 
      "jsonlite", "plotly", "lpSolve", "scales"
    ))
    ```

## Usage

1.  **Update Data:**
    Before running the app, fetch the latest FPL data to ensure stats are current:
    ```r
    source("update_data.R")
    ```
    This script fetches data from the FPL API, cleans it, and saves it to the `data/` directory.

2.  **Run the App:**
    ```r
    shiny::runApp()
    ```

## Project Structure

-   **`global.R`, `server.R`, `ui.R`**: Main Shiny application files.
-   **`R/`**: Backend logic for fetching data, cleaning, optimization, and league management.
-   **`modules/`**: Modularized Shiny components for different dashboard sections.
-   **`data/`**: Stores fetched `.rds` and `.csv` files (generated locally).
-   **`www/`**: Custom CSS styles.