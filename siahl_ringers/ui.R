# ui.R
library(shiny)
library(shinyMobile)

f7Page(
    title = "Hockey Game Info",
    f7SingleLayout(
        navbar = f7Navbar(title = "Hockey Game Info", color = "blue"),
        f7Card(
            f7Row(
                f7Col(
                    f7Text(inputId = "game_id", label = "Game ID", placeholder = "Enter Game ID"),
                    f7Button(inputId = "submit_btn", label = "Submit", fill = TRUE, color = "blue"),
                    width = 12
                )
            ),
            f7Row(
                f7Col(uiOutput("score_label"), width = 2),
                f7Col(uiOutput("away_team_info"), width = 5),
                f7Col(uiOutput("home_team_info"), width = 5)
            ),
            f7Row(
                f7Col(uiOutput("adjusted_label"), width = 2),
                f7Col(uiOutput("away_team_adj_info"), width = 5),
                f7Col(uiOutput("home_team_adj_info"), width = 5)
            ),
            verbatimTextOutput("update_time")
        )
    )
)
