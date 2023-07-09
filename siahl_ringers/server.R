library(shiny)
library(shinyMobile)
library(DT)

source('../siahl-utils.R')

shinyServer(function(input, output) {
    game_data <- reactiveVal()
    
message("in game_data")
    observeEvent(input$submit_btn, {
        print("Submit button clicked.") # Debug line
        
        game_id <- input$game_id
message(game_id, check_valid_game_id(game_id))
        
        # Check if game_id is valid
        if(check_valid_game_id(game_id)) {
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
            game_data(df)
        } else {
            
message('not valid game_id')
            game_data(NULL)
            f7Dialog(title = "Invalid Game ID", text = "Please enter a valid game ID.")
        }
    })
    
    output$hockey_info <- renderDT({
        req(game_data())
        datatable(game_data(), escape = FALSE, 
                  options = list(autoWidth = TRUE, dom = 't'))
    }, server = FALSE)
    
    # Display the update time
    output$update_time <- renderText({
        req(game_data())
        paste("Last update:", game_data()$update_time)
    })
})
