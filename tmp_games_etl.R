# 
url <- "https://stats.sharksice.timetoscore.com/oss-scoresheet?game_id=373622&mode=display"


game_page <- read_html(url)
scoring <- game_page %>% html_elements(".d25l table") 

get_scoring_tbl.old <- function(box_score, home = TRUE) {
    list_num = ifelse(home, 2, 1)
    scoring <- box_score[list_num] %>% html_table() %>% .[[1]]
    names(scoring) <- scoring[1, ]
    scoring[-1, ]
}

promote_header <- function(df) { 
    names(df) <- df[1,]
    df[-1, ] 
}

get_scoring_tbl <- function(box_score, home = TRUE) {
    list_num = ifelse(home, 2, 1)
    scoring <- box_score[list_num] %>% html_table() %>% .[[1]] %>% 
        promote_header() %>% 
        clean_names()
}

visitor_scoring <- scoring %>% get_scoring_tbl(home = FALSE)
home_scoring <- scoring %>% get_scoring_tbl(home = TRUE)


box_score <- game_page %>% 
    html_elements("td:nth-child(3) table") %>% 
    html_table()

home_scoring %>% 
    across(.names = {.[, 1]})

#-----------

team_stat_tables <- read_html("https://stats.sharksice.timetoscore.com/display-stats.php?league=1&season=56%22") %>% 
    html_elements("td , td, th:nth-child(2)") %>% 
    html_table()

c1 <- read_html("https://stats.sharksice.timetoscore.com/display-stats.php?league=1&season=56%22") %>% 
    html_elements(".table-striped :nth-child(2)") %>% 
    html_text(trim = TRUE) 
c1.num <- suppressWarnings(c1[!is.na(as.numeric(c1))])


##
iteam = 36
team_name = teams$team[iteam]


url_team <- "https://stats.sharksice.timetoscore.com/display-schedule?team=841&season=56&league=1&stat_class=1"
team_webpage <- read_html(url_team)

team_tables <- team_webpage %>%
    html_nodes("table")
player_stats <- team_tables %>% .[2] %>% html_table(header = TRUE) %>% .[[1]] %>% promote_header()
game_list <-    team_tables %>% .[1] %>% html_table(header = TRUE) %>% .[[1]] %>% promote_header()

get_scoring_html <- function(url) {url %>% read_html() %>% html_elements(".d25l table") %>% list() }
game_urls <- game_list %>%  
    select(Game, Home, Away) %>% 
    mutate(game_url = sprintf("%soss-scoresheet?game_id=%s&mode=display", 
                                 base_url ,str_extract(Game, '\\d+')),
           home = Home == team_name,
           scoring_html = map(game_url, get_scoring_html)
           )


# scoring_html = map(game_urls$game_url[1:2], get_scoring_html)
# ss1 <- get_scoring_html(game_urls$game_url[1])

for(igame in 1:nrow(game_urls)) {
     game_xml <- scoring_html[[igame]][[1]]
     team_scoring <-    game_xml %>% get_scoring_tbl(home =   game_urls$home[igame])
    opponent_scoring <- game_xml %>% get_scoring_tbl(home = ! game_urls$home[igame])
    

}

