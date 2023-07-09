library(shiny)
library(shinyMobile)
library(gt)

f7Page(
    title = "Hockey Game Ringer Effects",
    options = list(
        theme = "ios",
        dark = FALSE,
        color = "teal"),
    f7SingleLayout(
        navbar = f7Navbar(title = "Hockey Game Ringer Effects", color = "blue"),
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
            f7Row(
                f7Col(uiOutput("ringers_label"), width = 2),
                f7Col(gt::gt_output("away_team_ringers"), width = 5),
                f7Col(gt::gt_output("home_team_ringers"), width = 5)
            ),
            f7Row(
                f7Col(uiOutput("scoring_label"), width = 2),
                f7Col(gt::gt_output("scoring"), width = 10)
            ),
            verbatimTextOutput("update_time")
        )
    )
)