



# --- UI COMPONENT ---
historyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Player Scout Profile"),
      layout_columns(
        col_widths = c(4, 8), 
        
        # --- LEFT: PROFILE & INTEL ---
        div(
          selectizeInput(ns("player_select"), "Search Player:", choices = NULL, multiple = FALSE),
          
          # 1. Profile Image
          div(
            style = "text-align: center; margin-top: 15px; margin-bottom: 20px;",
            uiOutput(ns("player_image_display")),
            br(),
            h3(textOutput(ns("player_name_label")), style = "color: white; font-weight: bold; margin: 5px 0 0 0;"),
            div(textOutput(ns("player_team_label")), style = "color: #00FF85; font-size: 16px; font-weight: 500;")
          ),
          
          # 2. Manager Intel Grid
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 5px; text-align: center;",
            div(style = "background: #2A0040; padding: 10px; border-radius: 10px; border: 1px solid #38003C;",
                h6("Form", style = "color: #cccccc; margin: 0; font-size: 12px;"),
                h4(textOutput(ns("stat_form")), style = "color: #E90052; font-weight: bold; margin: 0;")),
            div(style = "background: #2A0040; padding: 10px; border-radius: 10px; border: 1px solid #38003C;",
                h6("Ownership", style = "color: #cccccc; margin: 0; font-size: 12px;"),
                h4(textOutput(ns("stat_selected")), style = "color: #04F5FF; font-weight: bold; margin: 0;")),
            div(style = "background: #2A0040; padding: 10px; border-radius: 10px; border: 1px solid #38003C;",
                h6("Exp Pts", style = "color: #cccccc; margin: 0; font-size: 12px;"),
                h4(textOutput(ns("stat_ep")), style = "color: #00FF85; font-weight: bold; margin: 0;"))
          )
        ),
        
        # --- RIGHT: INTERACTIVE SPARKLINE CHART ---
        div(
          style = "padding: 10px;",
          sliderInput(ns("gw_range"), "Filter Gameweeks:", min = 1, max = 38, value = c(1, 38), step = 1, width = "100%", ticks = FALSE),
          plotlyOutput(ns("history_plot"), height = "300px")
        )
      )
    )
  )
}

# --- SERVER COMPONENT ---
historyServer <- function(id, all_players_df) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      choices <- setNames(all_players_df$id, paste0(all_players_df$web_name, " (", all_players_df$team_name, ")"))
      updateSelectizeInput(session, "player_select", choices = choices, server = TRUE)
    })
    
    current_player <- reactive({
      req(input$player_select)
      all_players_df %>% filter(id == input$player_select)
    })
    
    observeEvent(input$player_select, {
        if(exists("fetch_player_history")) {
        history_data <- fetch_player_history(input$player_select)
        if(nrow(history_data) > 0) {
          max_gw <- max(history_data$gameweek)
          updateSliderInput(session, "gw_range", max = max_gw, value = c(1, max_gw))
        }
      }
    })
    
    output$player_image_display <- renderUI({
      req(current_player())
      raw_url <- current_player()$photo_url
      img_url <- ifelse(is.na(raw_url) | raw_url == "" | grepl("Photo-Missing", raw_url), 
                        "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png", raw_url)
      tags$img(src = img_url, width = "130px", style = "border: 4px solid #00FF85; border-radius: 50%; box-shadow: 0px 0px 20px #00FF85; background-color: #190028;")
    })
    
    output$player_name_label <- renderText({ req(current_player()); current_player()$web_name })
    output$player_team_label <- renderText({ req(current_player()); paste(current_player()$team_name, "|", current_player()$position) })
    output$stat_form <- renderText({ req(current_player()); current_player()$form })
    output$stat_selected <- renderText({ req(current_player()); paste0(current_player()$selected_by_percent, "%") })
    output$stat_ep <- renderText({ req(current_player()); current_player()$ep_next })
    
    output$history_plot <- renderPlotly({
      req(input$player_select)
      
      # Data Check
      if(!exists("fetch_player_history")) return(NULL)
      history_data <- fetch_player_history(input$player_select)
      
      # FALLBACK: If no history exists (e.g. data cleaning issue), create dummy points based on form
      # This allows the "Sparkline" to visualize the Form even without match data
      if (nrow(history_data) == 0) {
        # Create 5 fake points based on current form for visualization
        curr_form <- as.numeric(current_player()$form)
        history_data <- data.frame(
          gameweek = 1:5,
          points = rep(curr_form, 5)
        )
      }
      
      filtered_data <- history_data %>%
        filter(gameweek >= input$gw_range[1] & gameweek <= input$gw_range[2])
      
      # --- THE SPARKLINE CHART ---
      p <- ggplot(filtered_data, aes(x = gameweek, y = points)) +
        
        # 1. Area Glow (Cyan - The "Spark" Atmosphere)
        geom_area(fill = "#04F5FF", alpha = 0.15) +
        
        # 2. Main Trend Line (Cyan - The Sparkline itself)
        geom_line(color = "#04F5FF", linewidth = 1.2) + 
        
        # 3. Data Points (Green - The "Money" points)
        geom_point(aes(text = paste("GW:", gameweek, "<br>Points:", points)), 
                   color = "#190028", fill = "#00FF85", size = 3, shape = 21, stroke = 2) +
        
        # 4. Styling
        theme_minimal() +
        labs(x = "Gameweek", y = "Points") +
        theme(
          plot.background = element_rect(fill = "#190028", color = NA),
          panel.background = element_rect(fill = "#190028", color = NA),
          text = element_text(color = "white", family = "sans"),
          axis.text = element_text(color = "#888888"),
          axis.title = element_text(color = "#cccccc", size = 10),
          panel.grid.major = element_line(color = "#333333", linewidth = 0.2),
          panel.grid.minor = element_blank()
        )
      
      ggplotly(p, tooltip = "text") %>%
        plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(
          paper_bgcolor = "#190028",
          plot_bgcolor = "#190028",
          font = list(color = "white"),
          xaxis = list(showgrid = FALSE, zeroline = FALSE),
          yaxis = list(gridcolor = "#333333", zeroline = FALSE)
        )
    })
  })
}