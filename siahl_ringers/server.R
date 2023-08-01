# server.R
library(shiny)
library(shinyMobile)
library(gt)
library(tidyverse)

source('siahl-utils.R')

wlt_color <- function(score, comparison) {
    if (score > comparison) return("green")
    if (score < comparison) return("red")
    return("orange")
}
score_display <- function(score_number, score_color) {
    div(style = "text-align: center;",
        div(style = paste0("color: white; background-color: ", score_color,
                           "; padding: 5px; display: inline-block; font-size: 4em"),
            score_number))
}

# move this to sourced file (must be identical to ui.R)
all_teams <- readRDS(file = "all_teams-Current.RDS")
teams <- all_teams %>% 
    count(Division, Team) %>% 
    mutate(div_team = paste("Division", Division, "-", Team)) 

all_games <- readRDS(file = "games_stats-Current.RDS") %>% 
    mutate(home_away = ifelse(Home == Team, "Home", "Away"),
           opponent = ifelse(Home == Team, Away, Home),
           desc = sprintf("Game %s - %s (%s) vs %s %d-%d (Div %s)",
                          Game,
                          Team, home_away, opponent,
                          `Home Goals`, `Away Goals`,
                          Division))


shinyServer(function(input, output, session) {
    game_data <- reactiveVal()

    # Get the game_id parameter from the URL
    observe({
        query <- parseQueryString(session$clientData$url_search)
        game_id <- query[['game_id']]
        
        # Load game data if a valid game_id parameter is found
        if (!is.null(game_id) && check_valid_game_id(game_id)) {
            # Get the game info
            withProgress(message = 'Fetching game data. Please wait...', value = 0, {
                
            game_info <- game_info(game_id)
            
            game_data(game_info)
            # Set the input field to the current game_id
            updateTextInput(session, "game_id", value = game_id)
            })
        }
    })
    
    # Update game data when the an example game is chosen
    observeEvent(input$example_game, {
        if (input$example_game == "not_selected") return()
        game_id <- input$example_game
        
        # Progress message
        withProgress(message = 'Fetching game data. Please wait...', value = 0, {
            
            # Check if game_id is valid
            if(check_valid_game_id(game_id)) {
                # Get the game info
                game_info <- game_info(game_id)
                
                game_data(game_info)
                # Set the input field to the current game_id
                updateTextInput(session, "game_id", value = game_id)
                
            } else {
                game_data(NULL)
                f7Dialog(title = "Invalid Game ID", text = "You shouldn't see this error here :( Please enter a valid game ID.")
            }
        })
    })
    
    # Update game data when a team game is chosen
    observeEvent(input$team_games, {
        if(str_detect(input$team_games, "^Choose")) return()
        
        #if selection is a team name - then we populate
        if (str_detect(input$team_games, "^Division.* - ")) {
            team <- teams %>% filter(div_team == input$team_games)
            stopifnot(nrow(team) == 1)
            
            choose_text <- sprintf("Choose from %s (Div %s) games",
                                   team$Team, team$Division)
            new_menu <- c(choose_text, "Select different team")
            
            games <- all_games %>% 
                filter(Division == team$Division,
                       Team == team$Team) 
                
            
            updateSelectInput(session, "team_games",
                              choices = c(new_menu, games$desc))
            
            
            
        } else if(input$team_games == "Select different team") {
            updateSelectInput(session, "team_games",
                              choices = c("Choose Division/Team", teams$div_team)
            )
        } else if(str_detect(input$team_games, "^Game")) {
            game_id <- str_replace(input$team_games, "^Game (\\S+) .*", "\\1")
            
             # Progress message
            withProgress(message = 'Fetching game data. Please wait...', value = 0, {
                
                # Check if game_id is valid
                if(check_valid_game_id(game_id)) {
                    # Get the game info
                    game_info <- game_info(game_id)
                    
                    game_data(game_info)
                    # Set the input field to the current game_id
                    updateTextInput(session, "game_id", value = game_id)
                    
                } else {
                    game_data(NULL)
                    f7Dialog(title = "Invalid Game ID", text = "You shouldn't see this error here :( Please enter a valid game ID.")
                }
            })
        }
    })
    
    # Update game data when the submit button is clicked
    observeEvent(input$submit_btn, {
        game_id <- input$game_id
        
        # Progress message
        withProgress(message = 'Fetching game data. Please wait...', value = 0, {
            
            # Check if game_id is valid
            if(check_valid_game_id(game_id)) {
                # Get the game info
                game_info <- game_info(game_id)
                
                game_data(game_info)
            } else {
                game_data(NULL)
                f7Dialog(title = "Invalid Game ID", text = "Please enter a valid game ID.")
            }
        })
    })
    
    output$away_header <- renderUI({
        req(game_data())
        "Away"
    })
    
    output$home_header <- renderUI({
        req(game_data())
        "Home"
    })
    
    output$away_team_name <- renderUI({
        req(game_data())
        game_data()$v_team
    })
    
    output$home_team_name <- renderUI({
        req(game_data())
        game_data()$h_team
    })
    
    
    # output$away_team_name <- renderUI({
    #     req(game_data())
    #     h3(game_data()$v_team, align = "center")
    # })
    # 
    # output$home_team_name <- renderUI({
    #     req(game_data())
    #     h3(game_data()$h_team, align = "center")
    # })
    
    output$away_team_score <- renderUI({
        req(game_data())
        score_number <- game_data()$v_goals
        score_color <- wlt_color(score_number, game_data()$h_goals)
        score_display(score_number, score_color)
    })
    
    output$home_team_score <- renderUI({
        req(game_data())
        score_number <- game_data()$h_goals
        score_color <- wlt_color(score_number, game_data()$v_goals)
        score_display(score_number, score_color)
    })    
    output$away_team_adj_info <- renderUI({
        req(game_data())
        score_number <- game_data()$vg_adj
        score_color <- wlt_color(score_number, game_data()$hg_adj)
        score_display(score_number, score_color)
    })
    
    output$home_team_adj_info <- renderUI({
        req(game_data())
        score_number <- game_data()$hg_adj
        score_color <- wlt_color(score_number, game_data()$vg_adj)
        score_display(score_number, score_color)
    })
    
    output$score_label <- renderUI({
        req(game_data())
        "Score"
    })
    
    output$adjusted_label <- renderUI({
        req(game_data())
        "Adj* Score"
     })
    
    # Display the update time
    output$update_time <- renderText({
        req(game_data())
        paste("Last update:", game_data()$update_time)
    })
    
    # Display the start time
    output$start_time <- renderUI({
        req(game_data())
        tagList(
            div(style = " vertical-align: text-top;",
                strong("Puck drop:"), 
                br(),
                format(game_data()$start_time, "%d-%b-%Y %H:%M %Z")) 
        )        
    })
    
    # display division
    output$game_division <- renderUI({
        req(game_data())
        tagList(
            div(style = " vertical-align: text-top;",
                strong("Division:"), 
                br(),
                game_data()$division) 
        )        
    })
    
    # Display the game status
    output$game_status <- renderUI({
        req(game_data())
        game_status <- game_data()$game_status
        tagList(
            div(style = paste0("color: white; background-color: ",
                               if(game_status == "Completed") {"green"}
                               else if(game_status == "Not started") {"gray"}
                               else {"orange"}, 
                               "; padding: 5px; display: inline-block;",
                               " vertical-align: text-bottom;"),
                paste0("Game ", game_status))
        )
    })
    
    # Display the game status
    output$affect <- renderUI({
        req(game_data())
        if(game_data()$game_status == "Completed") {
            hgoals <- game_data()$h_goals
            vgoals <- game_data()$v_goals
            hg_adj <- game_data()$hg_adj
            vg_adj <- game_data()$vg_adj
            outcome <- wlt_outcome(hgoals, vgoals)
            out_adj <- wlt_outcome(hg_adj, vg_adj)
            if (hgoals == hg_adj && 
                vgoals == vg_adj) {
                msg <- "No discoveable ringer affect"
                fcol <- "green"
            } else if(outcome == out_adj) {
                msg <- "Ringer scoring did not affect standings points"
                fcol <- "black"
            } else {
                msg <- HTML(paste0("<div>",
                                   "<b>Ringers affected the outcome of this game.</b>",
                                   "The Adjusted score indicates the home team ",
                                   "<b>", game_data()$h_team, 
                                   " <u>", outcome, "</u></b>",
                                   " would have changed to a ",
                                   "<b><u>", out_adj, "</u></b>.",
                                   "</div>"))
                
                      # sprintf("The Adjusted score (ringer involved goals removed) implies that, without ringers, 
                      #         the home team (%s)  %s would have changed to a %s.",
                      #          game_data()$h_team, outcome, out_adj)))
        
                fcol <- "red"
            }
        } else {
            msg <- " "
        }
        tagList(
            div(style = paste0("color: ", 
                               fcol, 
                               "; text-align: center; padding: 5px; display: inline-block;",
                               " vertical-align: text-bottom;"),
                msg)
        )
    })
    
    output$ringers_label <- renderUI({
        req(game_data())
        "Ringers"
    })
    
    gt_small_font <- function(x, ...) {gt(x, ...) %>% tab_options(table.font.size = "small")}
    output$away_team_ringers <- gt::render_gt({
        req(game_data())
        game_data()$v_ringers %>% 
            gt_small_font()
    })
    
    output$home_team_ringers <- gt::render_gt({
        req(game_data())
        game_data()$h_ringers  %>% 
            gt_small_font()
    })
    
    output$scoring_label <- renderUI({
        req(game_data())
        tags$p("Scoring", style = "text-align:right")
    })
    
    output$scoring <- gt::render_gt({
        req(game_data())
        game_data()$scoring %>% 
            gt_small_font() %>%
            sub_missing(
                columns = everything()
            ) %>% 
            tab_style(
                style = cell_fill(color = "yellow"),
                locations = cells_body(
                    rows = game_data()$scoring$adj == "*"
                )
            )
    })
    
    output$explanation <- renderUI({
        req(game_data())
        p("*Adjusted Score is the result of removing goals
          highlighted in yellow that involved players from the Ringers
          list, based on their play in more advanced divisions (see Div column).")
    })
})

