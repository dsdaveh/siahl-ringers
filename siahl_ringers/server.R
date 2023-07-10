# server.R
library(shiny)
library(shinyMobile)
library(gt)

source('../siahl-utils.R')

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
    
    output$away_team_info <- renderUI({
        req(game_data())
        tagList(
            h4("Away"),
            h5(game_data()$v_team),
            div(style = paste0("color: white; background-color: ", 
                               if(game_data()$v_goals > game_data()$h_goals) {"green"} 
                               else if(game_data()$v_goals < game_data()$h_goals) {"red"} 
                               else {"orange"}, "; padding: 5px; display: inline-block;"), game_data()$v_goals)
        )
    })
    
    output$home_team_info <- renderUI({
        req(game_data())
        tagList(
            h4("Home"),
            h5(game_data()$h_team),
            div(style = paste0("color: white; background-color: ", 
                               if(game_data()$h_goals > game_data()$v_goals) {"green"} 
                               else if(game_data()$h_goals < game_data()$v_goals) {"red"} 
                               else {"orange"}, "; padding: 5px; display: inline-block;"), game_data()$h_goals)
        )
    })
    
    output$away_team_adj_info <- renderUI({
        req(game_data())
        tagList(
            div(style = paste0("color: white; background-color: ", 
                               if(game_data()$vg_adj > game_data()$hg_adj) {"green"} 
                               else if(game_data()$vg_adj < game_data()$hg_adj) {"red"} 
                               else {"orange"}, "; padding: 5px; display: inline-block;"), game_data()$vg_adj)
        )
    })
    
    output$home_team_adj_info <- renderUI({
        req(game_data())
        tagList(
            div(style = paste0("color: white; background-color: ", 
                               if(game_data()$hg_adj > game_data()$vg_adj) {"green"} 
                               else if(game_data()$hg_adj < game_data()$vg_adj) {"red"} 
                               else {"orange"}, "; padding: 5px; display: inline-block;"), game_data()$hg_adj)
        )
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
                game_status)
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
