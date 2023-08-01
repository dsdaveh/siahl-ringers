library(shiny)
library(shinyMobile)
library(gt)
library(tidyverse)

eg_games <- read_csv('affected_games.csv', show_col_types = FALSE) %>% 
    head(10) %>% 
    mutate(game_label = sprintf("Game %s %s vs %s (Home)  %d goals involving ringers",
                                Game, Away, Home, n_ringer_goals)
    )

all_teams <- readRDS(file = "all_teams-Current.RDS")
teams <- all_teams %>% 
    count(Division, Team) %>% 
    mutate(div_team = paste("Division", Division, "-", Team)) 

flex3 <- c("flex: 2;", "flex: 5;", "flex: 5;" )

f7Page(
    title = "Hockey Game Ringer Effects",
    options = list(
        theme = "ios",
        dark = FALSE,
        color = "teal"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    f7SingleLayout(
        navbar = f7Navbar(title = "Hockey Game Ringer Effects", color = "blue"),
        f7Card(
            f7Row(
                f7Col(
                    f7Text(inputId = "game_id", label = "Game ID", placeholder = "Enter Game ID or select below"),
                    f7Button(inputId = "submit_btn", label = "Submit", fill = TRUE, color = "blue"),
                    width = 12
                )
            ),
            f7Row(tags$small(".", style = "color: white")), #vertical spacing
            f7Row(
                f7Col(uiOutput("start_time")),
                f7Col(uiOutput("game_status")),
                f7Col(uiOutput("game_division"))
            ),
            f7Row(
                f7Col(uiOutput("affect")) 
            ),

            f7Row(
                f7Col(style = flex3[1]), # leave this empty for alignment
                f7Col(uiOutput("away_header"), style = flex3[2], class = "team-header"),
                f7Col(uiOutput("home_header"), style = flex3[3], class = "team-header")
            ),
            f7Row(
                f7Col(style = flex3[1]), # leave this empty for alignment
                f7Col(uiOutput("away_team_name"), style = flex3[2], class = "team-name"),
                f7Col(uiOutput("home_team_name"), style = flex3[3], class = "team-name")
            ),
            f7Row(
                class = "row-flex",
                f7Col(style = flex3[1], uiOutput("score_label"), class = "col-label"),
                f7Col(style = flex3[2], uiOutput("away_team_score")),
                f7Col(style = flex3[3], uiOutput("home_team_score"))
            ),
            f7Row(tags$small(".", style = "color: white")), #vertical spacing
            f7Row(
                class = "row-flex",
                f7Col(uiOutput("adjusted_label"), style = flex3[1], class = "col-label"),
                f7Col(uiOutput("away_team_adj_info"), style = flex3[2]),
                f7Col(uiOutput("home_team_adj_info"), style = flex3[3])
            ),
            f7Row(
                f7Col(uiOutput("ringers_label"), style = flex3[1], class = "col-label"),
                f7Col(gt::gt_output("away_team_ringers"), style = flex3[2]),
                f7Col(gt::gt_output("home_team_ringers"), style = flex3[3])
            ),
            f7Row(
                f7Col(gt::gt_output("scoring"))
            ),
            f7Row(
                f7Col(uiOutput("explanation"))
            ),
            f7Row(
                f7Col(verbatimTextOutput("update_time"))
            ),
            f7Row(
                selectInput("example_game", "(optional) Example Games", width = '100%',
                            choices = setNames(
                                c("not_selected", eg_games$Game), 
                                c("Choose from examples of ringer influenced games", eg_games$game_label)) 
                )
            ),
            f7Row(
                selectInput("team_games", "(optional) Select from games by team", width = '100%',
                            choices = c("Choose Division/Team", teams$div_team)
                )
            ),
            f7Row(
                f7Col(
                    tags$p("For more context about this app, see"),
                    tags$a("An open letter to Sharks Ice Adult Hockey League Management",
                           href = "http://rpubs.com/dsdaveh/ringerwip")
                )
            )
        )
    )
)