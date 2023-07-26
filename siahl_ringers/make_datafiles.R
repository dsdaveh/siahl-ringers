## run all chunks in siahl-etl.qmd
## run all chunks in siahl-eda.qmd


## App deployment -- ignore warnings
setwd("~/Documents/GitHub/siahl-ringers-branch")

library(here)
source('siahl-utils.R')

load(here("siahl-eda-Current.qmd.RData"))
saveRDS(all_teams, file = "siahl_ringers/all_teams.RDS" )

affected_game_ids <- games_adj %>% filter(adj_effect) %>% pull(Game)
affected_scorecards <- scorecards
for(gid in names(affected_scorecards)) {
    if (! gid %in% affected_game_ids) affected_scorecards[[gid]] <- NULL
}
saveRDS(affected_scorecards, file = "siahl_ringers/scorecards.RDS" )

affected_games <- games_adj %>% 
    filter(adj_effect) %>% 
    mutate(n_ringer_goals = h_goals + v_goals - hg_adj - hg_adj) %>% 
    arrange(-n_ringer_goals)
write_csv(affected_games, file = "affected_games.csv")

html_text <- readLines(here("app_demo/index_template.html"))
find_line <- which(str_detect(html_text, "AWAY_TEAM vs HOME_TEAM"))
template_line <- html_text[find_line]

example_games <- character()
for (i in 1:7) {
    game <- affected_games[i, ]
    item_html <- template_line %>% 
        str_replace('HOME_TEAM', game$Home) %>% 
        str_replace('AWAY_TEAM', game$Away) %>% 
        str_replace('OUTCOME', wlt_outcome(game$h_goals, game$v_goals)) %>% 
        str_replace('OUT_ADJ', wlt_outcome(game$hg_adj, game$vg_adj)) %>%
        str_replace('NRG', game$n_ringer_goals %>% as.character())
    example_games <- c(example_games, item_html)
}

new_text <- c(
    html_text[1:(find_line - 1)],
    example_games,
    html_text[(find_line + 1):length(html_text)]
)
writeLines(new_text, con = file("app_demo/index.html"))
