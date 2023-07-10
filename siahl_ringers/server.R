# server.R
library(shiny)
library(shinyMobile)
library(gt)

source('../siahl-utils.R')

wlt_color <- function(score, comparison) {
    if (score > comparison) return("green")
    if (score < comparison) return("red")
    return("orange")
}
score_display <- function(score_number, score_color) {
    div(style = "text-align: center;",
        div(style = paste0("color: white; background-color: ", score_color,
                           "; padding: 5px; display: inline-block;"),
            score_number))
}

shinyServer(function(input, output) {
    game_data <- reactiveVal()
    
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
        h4("Away", align = "center")
    })
    
    output$home_header <- renderUI({
        req(game_data())
        h4("Home", align = "center")
    })
    
    output$away_team_name <- renderUI({
        req(game_data())
        h5(game_data()$v_team, align = "center")
    })
    
    output$home_team_name <- renderUI({
        req(game_data())
        h5(game_data()$h_team, align = "center")
    })
    
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
        h6("Score", align = "right")
    })
    
    output$adjusted_label <- renderUI({
        req(game_data())
        h6("Adjusted*", align = "right")
     })
    
    # Display the update time
    output$update_time <- renderText({
        req(game_data())
        paste("Last update:", game_data()$update_time)
    })
    
    # Display the start time
    output$start_time <- renderUI({
        req(game_data())
        puck_drop = paste("Puck drop:", 
              format(game_data()$start_time, "%d-%b-%Y %H:%M %Z"))
        h6(puck_drop)
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
    
    
    output$game_division <- renderUI({
        req(game_data())
        h6(game_data()$division)
    })
    
    
    
    output$ringers_label <- renderUI({
        req(game_data())
        h6("Ringers", align = "right")
    })
    
    output$away_team_ringers <- gt::render_gt({
        req(game_data())
        gt(game_data()$v_ringers)
    })
    
    output$home_team_ringers <- gt::render_gt({
        req(game_data())
        gt(game_data()$h_ringers)
    })
    
    output$scoring_label <- renderUI({
        req(game_data())
        tags$p("Scoring", style = "text-align:right")
    })
    
    output$scoring <- gt::render_gt({
        req(game_data())
        gt(game_data()$scoring)
    })
    
    
})
