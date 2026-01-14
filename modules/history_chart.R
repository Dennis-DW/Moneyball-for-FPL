# --- UI COMPONENT ---
historyUI <- function(id) {
  ns <- NS(id)
  
  css_styles <- "
    .scout-card { background: #2A0040; border: 1px solid rgba(255,255,255,0.1); border-radius: 12px; padding: 10px; margin-bottom: 12px; }
    .stat-box { background: #190028; border: 1px solid #333; border-radius: 8px; padding: 5px; text-align: center; }
    .section-title { font-family: 'Segoe UI'; font-size: 10px; color: #888; margin-bottom: 5px; text-transform: uppercase; letter-spacing: 1px; }
    .fixture-box { width: 100%; height: 25px; display: flex; align-items: center; justify-content: center; border-radius: 4px; font-weight: bold; font-size: 10px; color: #000; margin-right: 2px; }
    
    /* PROFESSIONAL TABLE STYLES (Flexbox) */
    .sim-table { width: 100%; border-spacing: 0 8px; border-collapse: separate; }
    .sim-row { background: rgba(255,255,255,0.03); transition: all 0.2s; }
    .sim-row:hover { background: rgba(255,255,255,0.1); transform: translateX(2px); }
    
    .sim-cell-left { padding: 8px; border-top-left-radius: 8px; border-bottom-left-radius: 8px; }
    .sim-cell-right { padding: 8px; text-align: right; border-top-right-radius: 8px; border-bottom-right-radius: 8px; color: #00FF85; font-weight: bold; font-size: 13px; }
    
    .player-lockup { display: flex; align-items: center; gap: 10px; }
    .lockup-img { width: 36px; height: 36px; border-radius: 50%; border: 2px solid #333; background: #000; }
    .lockup-info { display: flex; flex-direction: column; justify-content: center; line-height: 1.1; }
    .lockup-name { font-size: 12px; font-weight: 700; color: white; }
    .lockup-team { font-size: 10px; color: #888; text-transform: uppercase; }

    h2 { font-size: 20px !important; margin: 0 !important; }
  "
  
  tagList(
    tags$head(tags$style(HTML(css_styles))),
    div(class = "container-fluid", style = "padding: 0;",
        div(style = "margin-bottom: 10px;", selectizeInput(ns("player_select"), label = NULL, choices = NULL, width = "100%", options = list(placeholder = "Search player..."))),
        layout_columns( col_widths = c(3, 9),
                        div(class = "scout-card", uiOutput(ns("profile_ui"))),
                        div(
                          div(class = "scout-card", style = "padding: 5px 10px;", uiOutput(ns("fixture_ticker"))),
                          div(class = "scout-card", 
                              div(class = "section-title", "PERFORMANCE PROFILE"),
                              layout_columns(col_widths = c(8, 4), 
                                             plotlyOutput(ns("plot_history"), height = "280px"), 
                                             plotOutput(ns("plot_bars"), height = "280px"))
                          ),
                          div(class = "scout-card", 
                              div(style="display:flex; justify-content:space-between; align-items:center;",
                                  div(class = "section-title", "LEAGUE COMPARISON (VIOLIN)"),
                                  uiOutput(ns("report_header_img"))
                              ),
                              layout_columns(col_widths = c(12),
                                             plotOutput(ns("plot_violin"), height = "280px")
                              ),
                              uiOutput(ns("scout_report_card"))
                          )
                        )
        )
    )
  )
}

# --- SERVER COMPONENT ---
historyServer <- function(id, all_players_df, fixtures_df) {
  moduleServer(id, function(input, output, session) {
    
    observe({ choices <- setNames(all_players_df$id, paste0(all_players_df$web_name, " (", all_players_df$team_name, ")")); updateSelectizeInput(session, "player_select", choices = choices, server = TRUE) })
    current_player <- reactive({ req(input$player_select); all_players_df %>% filter(id == input$player_select) })
    
    output$profile_ui <- renderUI({
      req(current_player()); p <- current_player()
      sim_html <- "Loading..."
      if(exists("logic_get_similar")) {
        sims <- logic_get_similar(p$id, all_players_df)
        if(!is.null(sims)) {
          rows <- apply(sims, 1, function(x) {
            paste0(
              "<tr class='sim-row'>",
              "<td class='sim-cell-left'><div class='player-lockup'><img src='", x['photo_url'], "' class='lockup-img'><div class='lockup-info'><span class='lockup-name'>", x['web_name'], "</span><span class='lockup-team'>", x['team_name'], "</span></div></div></td>",
              "<td class='sim-cell-right'>£", as.numeric(x['cost'])/10, "m</td>",
              "</tr>"
            )
          })
          sim_html <- paste0("<table class='sim-table'>", paste(rows, collapse=""), "</table>")
        } else { sim_html <- "No matches found" }
      }
      raw_url <- p$photo_url
      img_url <- ifelse(is.na(raw_url)|raw_url==""|grepl("Photo-Missing", raw_url), "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png", raw_url)
      
      tagList(
        div(style="text-align:center", tags$img(src=img_url, width="100px", style="border:3px solid #04F5FF; border-radius:50%; background:#000;"), br(), h2(p$web_name, style="color:white"), h6(paste(p$team_name, "|", p$position))),
        br(),
        div(style="display:grid; grid-template-columns:1fr 1fr; gap:5px", div(class="stat-box", h6("Form"), h3(p$form, style="color:#E90052")), div(class="stat-box", h6("Price"), h3(paste0("£", p$cost, "m"), style="color:white"))),
        br(),
        div(class="section-title", "SIMILAR OPTIONS"),
        HTML(sim_html)
      )
    })
    
    output$report_header_img <- renderUI({ req(current_player()); raw_url <- current_player()$photo_url; img_url <- ifelse(is.na(raw_url)|raw_url==""|grepl("Photo-Missing", raw_url), "https://fantasy.premierleague.com/dist/img/shirts/standard/shirt_0.png", raw_url); tags$img(src=img_url, width="30px", style="border-radius:50%; border:1px solid #555;") })
    
    output$scout_report_card <- renderUI({
      req(current_player()); p <- current_player()
      rank_info <- list(ict_rank = "-", total = 0)
      if(exists("logic_get_rank")) { rank_info <- logic_get_rank(p$id, all_players_df) }
      div(style="padding-left:10px; padding-top:10px; color:#ccc; font-size:11px; text-align:right;",
          span("ICT Rank: ", strong(style="color:#00FF85;", paste0("#", rank_info$ict_rank, " / ", rank_info$total))),
          span(style="margin-left:15px;", "Form Rank: ", strong(style="color:#04F5FF;", "Top 10%"))
      )
    })
    
    output$fixture_ticker <- renderUI({ req(current_player(), fixtures_df); if(!exists("logic_get_fixtures")) return(NULL); fix <- logic_get_fixtures(current_player()$team_code, fixtures_df); div(style="display:flex; align-items:center", span("NEXT 5:", style="font-size:10px; color:#aaa; margin-right:10px; font-weight:bold"), lapply(1:nrow(fix), function(i) { bg <- case_when(fix$difficulty[i]<=2 ~ "#00FF85", fix$difficulty[i]==3 ~ "#CCCCCC", TRUE ~ "#E90052"); div(class="fixture-box", style=paste0("background-color:", bg), fix$label[i]) })) })
    
    output$plot_bars <- renderPlot({ 
      req(current_player()); if(!exists("logic_get_radar")) return(NULL) 
      data <- logic_get_radar(current_player()$id, all_players_df)
      if(is.null(data)) return(NULL)
      chart_bars(data) 
    })
    
    output$plot_history <- renderPlotly({
      req(current_player()); if(!exists("fetch_player_history")) return(NULL)
      data <- fetch_player_history(current_player()$id)
      if(nrow(data)==0) data <- data.frame(gameweek=1:5, points=0, minutes=0)
      p <- chart_history(data)
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE) %>% layout(plot_bgcolor = "#2A0040", paper_bgcolor = "#2A0040", xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    })
    
    output$plot_violin <- renderPlot({
      req(current_player())
      if(!exists("logic_get_violin")) return(NULL)
      data <- logic_get_violin(current_player()$id, all_players_df)
      if(is.null(data)) return(NULL)
      chart_violin(data)
    })
  })
}