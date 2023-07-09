library(shiny)
library(shinyMobile)
library(DT)

source('../siahl-utils.R')


shinyServer(function(input, output) {
    game_data <- reactive({
        req(input$submit_btn)
message("game_data", input$game_id)
        
        isolate({
            game_id <- input$game_id
message(class(game_id))
            
            # Get the game info
            game_info <- game_info(game_id)
            
            # Create a dataframe to be displayed as a table
            df <- data.frame(
                Team = c(game_info$v_team, game_info$h_team),
                Score = c(game_info$v_goals, game_info$h_goals)
            )
            
            df$Score <- sprintf('<div style="color: white; background-color: %s; padding: 5px;">%s</div>', 
                                ifelse(df$Score > max(df$Score), "green", ifelse(df$Score < max(df$Score), "red", "orange")),
                                df$Score)
            df
        })
    })
    
    output$hockey_info <- renderDT({
        datatable(game_data(), escape = FALSE, 
                  options = list(autoWidth = TRUE, dom = 't'))
    }, server = FALSE)
    
    # Display the update time
    output$update_time <- renderText({
        paste("Last update:", game_data()$update_time)
    })
})
