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

#need this because roster from scorecard don't always match team roster names
#scorecards have middle initials, while team rosters don't
match_player_name <- function(name, player_stats = all_teams) {
    #name is single character string
    stopifnot(length(name) == 1)
    #global variable default should be avoided for speed
    stopifnot(exists("player_stats") && nrow(player_stats) > 0)
    
    first_last <- str_replace(name, ' .* ', ' ') %>% tolower()
    match <- player_stats %>% 
        count(Name) %>% 
        filter(first_last == tolower(Name)) %>% 
        pull(Name)
    if (length(match) == 0) return (FALSE)
    if (length(match) > 1) warning("Multiple roster name matches for ", name)
    return(match[1])
}

get_ringers <- function(box_score, home = TRUE, player_stats = all_teams) {
    stopifnot(exists("player_stats") && nrow(player_stats) > 0)
    division = box_score %>% scorecard_division()
    team = box_score %>% scorecard_teamname(home = home)
    team_roster <- player_stats %>% 
        filter(str_detect(Division, division),
               Team == team)
    #if division not found search without it (there shouldn't be duplicate names anyway)
    #this can happen for interdivision games
    if (nrow(team_roster) == 0) {
        team_roster <- player_stats %>% filter(Team == team)
        stopifnot(nrow(team_roster) > 0)
    }
    ringers <- team_roster %>% 
        filter(ringer_count > 0,
               str_length(`#`) > 0) %>% 
        pull(Name) 
        
    playing <- scorecard_players(box_score, home = home) %>% 
        filter(P != 'G') %>%  # remove goalie
        mutate(match_name = map_chr(Name, match_player_name, team_roster))
    
    if (any(playing$match_name == FALSE)) {
        unmatched = character()
        for (ix in which(playing$match_name == FALSE)) {
            match_name = match_player_name(playing$Name[ix]) #attempt match to all players
            if (match_name != FALSE) {
                playing[ix, 'match_name'] <- match_name
            } else {
                unmatched <- c(unmatched, playing$Name[ix])
            }
        }
        if ((n_unmatched <- length(unmatched)) > 0) {
        warning("Team:", team, " Division:", division, " ", unmatched, "names on box score roster not found on team roster:")
            message( unmatched %>% paste(collapse = "\n"))
        }
    }
    
    playing %>% 
        filter(match_name %in% ringers) 
}

scorecard_goals <- function(box_score, home = TRUE, remove_ringer_goals = FALSE, player_stats = all_teams) {
    ringers <- numeric()
    if (remove_ringer_goals) { 
        ringers <- get_ringers(box_score, home, player_stats) %>% pull(`#`)
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

# we need this to get jersey numbers
scorecard_players <- function(box_score, home = TRUE) {
    empty_roster <- tibble::tribble(~"#", ~"P", ~"Name")
    hv_key = ifelse(home, 11, 9)
    raw_chk <- box_score %>% 
        read_html() %>% 
        html_elements("td table") 
    if(length(raw_chk) < 11) {
        return(empty_roster)  #no game stats available - game not played?
    }
    raw_chk2 <- raw_chk %>% 
        .[hv_key] %>% 
        html_table() %>% 
        .[[1]]
    if(ncol(raw_chk2) < 6) {
        return(empty_roster)  #no game stats available - home team no show?
    }
    raw <- raw_chk2 %>% 
        .[-1, c(1:6)] %>% 
        promote_header() 
    stopifnot(names(raw)[3] == "Name")
    roster <- bind_rows(
        raw[ ,1:3],
        raw[ ,4:6]
    ) %>% 
        filter(str_length(`#`) > 0) 
}
