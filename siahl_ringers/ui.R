library(shiny)
library(shinyMobile)
library(gt)

flex3 <- c("flex: 2;", "flex: 5;", "flex: 5;" )

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
                f7Col(uiOutput("start_time")),
                f7Col(), # leave this empty for alignment
                f7Col(uiOutput("game_division"))
            ),
            f7Row(
                f7Col(), # leave this empty for alignment
                f7Col(uiOutput("game_status")),
                f7Col() # leave this empty for alignment
            ),
            f7Row(
                f7Col(style = flex3[1]), # leave this empty for alignment
                f7Col(uiOutput("away_header"), style = flex3[2]),
                f7Col(uiOutput("home_header"), style = flex3[3])
            ),
            f7Row(
                f7Col(style = flex3[1]), # leave this empty for alignment
                f7Col(uiOutput("away_team_name"), style = flex3[2]),
                f7Col(uiOutput("home_team_name"), style = flex3[3])
            ),
            f7Row(
                f7Col(style = flex3[1], uiOutput("score_label")),
                f7Col(style = flex3[2], uiOutput("away_team_score")),
                f7Col(style = flex3[3], uiOutput("home_team_score"))
            ),
            f7Row(
                f7Col(uiOutput("adjusted_label"), style = flex3[1]),
                f7Col(uiOutput("away_team_adj_info"), style = flex3[2]),
                f7Col(uiOutput("home_team_adj_info"), style = flex3[3])
            ),
            f7Row(
                f7Col(uiOutput("ringers_label"), style = flex3[1]),
                f7Col(gt::gt_output("away_team_ringers"), style = flex3[2]),
                f7Col(gt::gt_output("home_team_ringers"), style = flex3[3])
            ),
            f7Row(
                f7Col(gt::gt_output("scoring"))
            ),
            f7Row(
                f7Col(uiOutput("explanation")),
            ),
            verbatimTextOutput("update_time")
        )
    )
)