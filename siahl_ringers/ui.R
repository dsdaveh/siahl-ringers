library(shiny)
library(shinyMobile)
library(DT)

shinyUI(f7Page(
    title = "Hockey Game Info",
    f7SingleLayout(
        navbar = f7Navbar(title = "Hockey Game Info",
                          color = "blue"),
        f7Card(
            f7Text(inputId = "game_id", label = "Enter game ID", placeholder = "Enter game ID here"),
            actionButton(inputId = "submit_btn", label = "Submit")
        ),
        f7Card(
            DTOutput('hockey_info')
        ),
        f7Card(
            textOutput("update_time")
        )
    )
))
