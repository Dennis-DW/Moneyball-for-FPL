# --- UI ---
fixtureTickerUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      height = "380px",
      card_header("Fixture Difficulty Ticker (Next 5 Games)"),
      DT::dataTableOutput(ns("ticker_table"))
    )
  )
}

# --- SERVER ---
fixtureTickerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ticker_data <- reactive({ if(!file.exists("data/fixture_ticker.rds")) return(NULL); readRDS("data/fixture_ticker.rds") })
    
    output$ticker_table <- DT::renderDataTable({
      req(ticker_data())
      df <- ticker_data()
      
      DT::datatable(
        df %>% select(name, starts_with("opp_label"), starts_with("difficulty")),
        colnames = c("Team", "Next", "+2", "+3", "+4", "+5"),
        options = list(
          pageLength = 3, dom = 'tp', # Exactly 3 rows per page
          columnDefs = list(list(visible = FALSE, targets = 6:10))
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = paste0("opp_label_", 1:5),
          valueColumns = paste0("difficulty_", 1:5),
          backgroundColor = DT::styleEqual(c(1, 2, 3, 4, 5), c("#00FF85", "#00FF85", "#EBEBE4", "#FF00FF", "#E90052")),
          color = "#190028", fontWeight = "bold"
        )
    })
  })
}