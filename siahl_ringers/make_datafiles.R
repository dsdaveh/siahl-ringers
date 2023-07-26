## run all chunks in siahl-etl.qmd
## run all chunks in siahl-eda.qmd

library(here)
setwd("~/Documents/GitHub/siahl-ringers-branch")
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
