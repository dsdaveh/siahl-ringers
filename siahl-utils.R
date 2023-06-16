library(janitor)

# Define the URL
if (! exists("base_url")) {
    base_url <- "https://stats.sharksice.timetoscore.com/"
}


promote_header <- function(df) { 
    names(df) <- df[1,]
    df[-1, ] 
}

scorecard_goals <- function(box_score, home = TRUE, remove_ringer_goals = FALSE, player_stats = players) {
    ringers <- numeric()
    if (remove_ringer_goals) {
        stopifnot(exists("player_stats") && nrow(player_stats) > 0)
        division = box_score %>% scorecard_division()
        team = box_score %>% scorecard_teamname(home = home)
        ringers <- all_teams %>% 
            filter(Division == division,
                   Team == team,
                   ringer_count > 0  ) %>% 
            pull(`#`)
    }
    list_num = ifelse(home, 2, 1)
    scoring <- box_score %>% 
        read_html() %>% 
        html_elements(".d25l table")  %>% 
        .[list_num] %>% 
        html_table() %>% 
        .[[1]] %>% 
        promote_header() %>% 
        janitor::clean_names() %>% 
        filter(! goal %in% ringers,
               ! ass %in% ringers,
               ! ass_2 %in% ringers)
}

scorecard_division <- function(box_score) {
    box_score %>% 
        read_html() %>% 
        html_elements("tr:nth-child(3) td")  %>% 
        .[1] %>% 
        as.character() %>% 
        str_extract('Adult Division \\w+') %>%
        str_replace('Adult Division ', '')
}

scorecard_teamname <- function(box_score, home = TRUE) {
    hv_key = ifelse(home, 'Home', 'Visitor')
    box_score %>% 
        read_html() %>% 
        html_elements("td:nth-child(3) table")  %>% 
        .[1] %>% 
        html_table() %>% 
        .[[1]] %>% 
        janitor::clean_names() %>% 
        filter(team_name == hv_key) %>% 
        pull(team_name_2)
}
