library(tidyverse)
library(janitor)
library(rvest)
library(xml2)

#kludge for data errors
fix_known_scorecard_errors <- function(scorecard, game_id) {
    if (game_id == "384955*") scorecard <- str_replace_all(scorecard, "Chiefs", "Chieftains")
    return(scorecard)
}

# Define the URL
if (! exists("base_url")) {
    base_url <- "https://stats.sharksice.timetoscore.com/"
}

promote_header <- function(df) { 
    names(df) <- df[1,] |> as.character()
    df[-1, ] 
}

#get the ringer adjusted info for a game
#TODO - this steps on current GlobalEnv, so should not be in this utils file.

check_valid_game_id <- function(game_id) {
    #web site returns a scoreboard for virtually any game_id,
    # check that the data is within the scope expected
    game_url = sprintf("%soss-scoresheet?game_id=%s&mode=display", 
                       base_url ,str_extract(game_id, '\\d+')) 
    box_score <- game_url %>% read_html() %>% as.character()
    
    # *SOME* game_ids return nothing
    td_check <- box_score %>% read_html() %>% html_elements("td")
    if (length(td_check) == 0) return(FALSE)
    
    meta <- box_score %>% scorecard_meta() 
    if (meta["League"] != "SIAHL@SJ") return(FALSE) #return early so next line doesn't fail
    game_date <- box_score %>% scorecard_game_time()
    return(game_date > ymd('2016-01-01'))
}

mtime <- NA_POSIXct_
game_info <- function(game_id) {
    if (! (exists("scorecards") && exists("all_teams"))) {
        message("loading data from Current season...")
        rdata_file <- "siahl-eda-Current.qmd.RData"
        load(rdata_file, .GlobalEnv)
        
        mtime <<- file.mtime(rdata_file)
    }
    # test with game_info("387361*")
    # 
    # 
    if (is.null(scorecards[[game_id]])) {
        game_url = sprintf("%soss-scoresheet?game_id=%s&mode=display", 
                           base_url ,str_extract(game_id, '\\d+')) 
        scorecard <- game_url %>% read_html() %>% as.character()
        update_time <- now()
    } else {
        scorecard <- scorecards[[game_id]]
        update_time <- ifelse(exists('mtime'), mtime, NA_POSIXct_) %>% as.POSIXct()
    }

    #return
    hg_adj <- scorecard %>% scorecard_goals(home = TRUE, remove_ringer_goals = TRUE) %>% nrow()
    vg_adj <- scorecard %>% scorecard_goals(home = FALSE, remove_ringer_goals = TRUE) %>% nrow()
    h_goals = scorecard %>% scorecard_goals(home = TRUE) %>% nrow()
    v_goals = scorecard %>% scorecard_goals(home = FALSE) %>% nrow()
    
    info <- list(
        game_id = game_id,
        update_time = update_time,
        start_time = scorecard %>% scorecard_game_time(),
        division = paste("SIAHL@SJ Division", scorecard %>% scorecard_division(), collapse = " "),
        h_team = scorecard %>% scorecard_teamname(home = TRUE),
        v_team = scorecard %>% scorecard_teamname(home = FALSE),
        
        h_goals = h_goals,
        v_goals = v_goals,
        hg_adj = hg_adj,
        vg_adj = vg_adj,
        hdiff = h_goals - v_goals,
        hdiff_adj = hg_adj - vg_adj,
        
        h_ringers = scorecard %>% get_ringers(home = TRUE) %>% select(`#`, Name),
        v_ringers = scorecard %>% get_ringers(home = FALSE) %>% select(`#`, Name)
  
    )
    return(info)
}

#need this because roster from scorecard don't always match team roster names
#scorecards have middle initials, while team rosters don't
match_player_name <- function(name, player_stats = all_teams) {
    #name is single character string
    stopifnot(length(name) == 1)
    #global variable default should be avoided for speed
    stopifnot(exists("player_stats") && nrow(player_stats) > 0)
    
    first_last <- str_replace(name, ' .* ', ' ') %>% tolower() %>% str_squish()
    match <- player_stats %>% 
        count(Name) %>% 
        filter(Name == name | first_last == (tolower(Name) %>% str_squish())) %>% 
        pull(Name)
    if (length(match) == 0) return ("<NOT_FOUND>")
    if (length(match) > 1) warning("Multiple roster name matches for ", name)
    return(match[1])
}

unmatched_players <- tibble()
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
    
    if (any(playing$match_name == "<NOT_FOUND>")) {
        unmatched = character()
        for (ix in which(playing$match_name == "<NOT_FOUND>")) {
            match_name = match_player_name(playing$Name[ix]) #attempt match to all players
            if (match_name != "<NOT_FOUND>") {
                playing[ix, 'match_name'] <- match_name
            } else {
                unmatched <- c(unmatched, playing$Name[ix])
            }
        }
        if ((n_unmatched <- length(unmatched)) > 0) {
            # warning("Team:", team, " Division:", division, " ", n_unmatched, 
            #         " names on box score roster not found on team roster:\n")
            # message( unmatched %>% paste(collapse = "\n"))
            # message("game_id: ", game_id)
            unmatched_players <<- bind_rows(
                unmatched_players,
                playing %>% 
                    filter(match_name == "<NOT_FOUND>") %>% 
                    select(-match_name) %>% 
                    mutate(Division = division,
                           Team = team,
                           Game = game_id))
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


scorecard_game_time <- function(box_score) {
    meta <- box_score %>% scorecard_meta()
    paste(meta['Date'], meta['Time'], collapse = "") %>% 
        lubridate::mdy_hm(tz = "America/Los_Angeles")
}

# a more comprehensive extraction... that other scorecard_... extractions can use
scorecard_meta <- function(box_score) {
    meta <- box_score %>% 
        read_html() %>% 
        html_elements("td")  %>% 
        html_text2() %>% 
        .[str_detect(., '^\\w+:')] %>%   # finds 'Key:Value ...'
        .[! str_detect(., '\\t')]       # remove multiple Key:Value
    keys <- meta %>% str_extract('.*?:') %>% str_replace(':', '') 
    vals <- meta %>% str_extract(":.*") %>% str_replace('^:', '')
    names(vals) <- keys
    return(vals)
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
    if(names(raw_chk2)[3] == "Name") {
        return(empty_roster)  #no game stats available - home visitingg no show? eg: game_id: "386666*"
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


get_season_teams <- function(sid, verbose = 1) {
    season_row <- seasons %>% filter(season_id == sid)
    stopifnot(nrow(season_row) == 1)
    season_name <- ifelse(season_row$season_name == 'Current', 
                          season_current, season_row$season_name)
    season_url <- 
        url <- sprintf("%sdisplay-stats.php?league=1&season=%s", base_url, sid)
    if (verbose >= 1) message("getting teams for season ", sid, ":", season_name, "...")
    
    
    # Read the webpage
    webpage <- read_html(url)
   
    # Initialize an empty tibble for storing results
    teams <- tibble()
    
    # Scrape the Adult Division names and team rows
    table_rows <- webpage %>%
        html_elements("tr") 
    
    if (length(table_rows) <= 0) {
        warning("No tables returned for season ", sid, ":", season_name, "\n")
        return(teams)
    }    
    
    
    # Initialize a variable to store the current division
    current_division <- ""
    
    # Loop over each table row
    for (i in 1:length(table_rows)) {
        # Get the row
        row <- table_rows[[i]]
        team_extract <- row %>% html_elements("td a") %>%  html_text(trim = TRUE)
        
        # Check if it's a division row
        if (row %>% html_text() %>% str_detect("Adult Division")) {
            current_division <- row %>% html_text()
            rank <- 1
            # Check if it's a team row
        } else if (length(team_extract) > 0) {
            team_name <- row %>% html_elements("td a") %>%  html_text(trim = TRUE)
            team_stats <- paste0(base_url, row %>% html_elements("td a") %>%  html_attr("href"))
            
            team_data <- tibble(
                team = team_name,
                team_id = team_stats %>%  str_extract('\\?team=\\d+') %>% str_extract('\\d+'),
                team_stats = team_stats,
                division = current_division %>% str_replace('Adult Division ', ''),
                division_rank = rank
            )
            rank = rank + 1
            
            # Append the team data to final_data
            teams <- bind_rows(teams, team_data) 
        }
    }
    teams%>% 
        mutate(season_id = sid,
               season_name = season_name)
}