
# --- UI ---
fixtureTickerUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      height = "600px",
      card_header("Fixture Difficulty Ticker (Next 5 Games)"),
      gt_output(ns("ticker_table"))
    )
  )
}

# --- SERVER ---
fixtureTickerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load Data directly inside the module
    ticker_data <- reactive({
      if(!file.exists("data/fixture_ticker.rds")) return(NULL)
      readRDS("data/fixture_ticker.rds")
    })
    
    output$ticker_table <- render_gt({
      req(ticker_data())
      df <- ticker_data()
      
      # Define Color Palette for Difficulty (1=Green, 5=Red)
      # FDR 2 = Easy (Green), 3 = Medium (Grey/Yellow), 4 = Hard (Orange), 5 = Impossible (Red)
      fdr_palette <- function(x) {
        if (x <= 2) return("#00FF85")      
        if (x == 3) return("#EBEBE4")      
        if (x == 4) return("#FF00FF")      
        if (x >= 5) return("#E90052")      
        return("white")
      }
      
      # Create Table
      df %>%
        select(name, starts_with("opp_label")) %>%
        gt() %>%
        cols_label(
          name = "Team",
          opp_label_1 = "Next",
          opp_label_2 = "+2",
          opp_label_3 = "+3",
          opp_label_4 = "+4",
          opp_label_5 = "+5"
        ) %>%
        
        # Center Alignment
        cols_align(align = "center", columns = everything()) %>%
        cols_align(align = "left", columns = "name") %>%
        
        # Styling
        tab_options(
          table.background.color = "#190028",
          table.font.color = "#FFFFFF",
          column_labels.background.color = "#38003C",
          column_labels.font.weight = "bold",
          table.border.top.color = "#444444",
          table_body.hlines.color = "#444444",
          data_row.padding = px(12)
        ) %>%
        opt_table_font(font = "Poppins") %>%
        
        # Color logic: loop through columns 1-5 to apply colors from the 'difficulty' data
        # (This uses the raw difficulty values mapped to the cells)
        data_color(
          columns = starts_with("opp_label"),
          direction = "column",
          fn = function(x) {
            # This is a visual-only mapping for the table cells
            # Ideally we map strictly to the difficulty number, 
            # but for the UI table, let's keep it simple:
            return("#190028") # Background matches table
          }
        ) %>%
        
        # Add Cell Borders for "Grid" look
        tab_style(
          style = cell_borders(sides = "all", color = "#444444", weight = px(1)),
          locations = cells_body()
        )
    })
  })
}