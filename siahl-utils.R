library(tidyverse)
library(janitor)
library(rvest)
library(xml2)

# Define the URL
if (! exists("base_url")) {
    base_url <- "https://stats.sharksice.timetoscore.com/"
}


promote_header <- function(df) { 
    names(df) <- df[1,]
    df[-1, ] 
}

get_ringers <- function(box_score, home = TRUE, player_stats = players) {
    ringers <- numeric()
        stopifnot(exists("player_stats") && nrow(player_stats) > 0)
        division = box_score %>% scorecard_division()
        team = box_score %>% scorecard_teamname(home = home)
        ringers <- all_teams %>% 
            filter(str_detect(Division, division),
                   Team == team,
                   ringer_count > 0,
                   str_length(`#`) > 0) %>% 
            pull(`#`) %>% 
            
    return(ringers)
}

scorecard_goals <- function(box_score, home = TRUE, remove_ringer_goals = FALSE, player_stats = players) {
    ringers <- numeric()
    if (remove_ringer_goals) { 
        ringers <- get_ringers(box_score, home, player_stats)
    }
    scoring_xml <- box_score %>% 
        read_html() %>% 
        html_elements(".d25l table")
    home_table <- ifelse(length(scoring_xml) == 2, 2, 3) #shootout contingency
    list_num = ifelse(home, home_table, 1)
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

#scoresheet division entry is not always consistent with web site (eg. 7B vs 7B West)
scorecard_division <- function(box_score) {
    box_score %>% 
        read_html() %>% 
        html_elements("tr:nth-child(3) td")  %>% 
        .[1] %>% 
        as.character() %>% 
        str_extract('Adult Division \\w+') %>%
        str_replace('Adult Division ', '')
}

game_division_lkp <- function(id) {
    games %>% filter(Game == id) %>% pull(Division) %>% head(1)
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

