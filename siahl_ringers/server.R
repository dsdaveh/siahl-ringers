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
    
    output$away_team_ringers <- renderUI({
        req(game_data())
        gt::gt(game_data()$v_ringers) %>% 
            gt::as_raw_html()
    })
    
    output$home_team_ringers <- renderUI({
        req(game_data())
        gt::gt(game_data()$h_ringers) %>% 
            gt::as_raw_html()
    })
    

    
})
