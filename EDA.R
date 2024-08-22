library(tidyverse)
library(nflfastR)
source("functions.R")

pbp = load_pbp(2023)

play_types = unique(pbp$play_type)

passes = pbp %>%
  filter(play_type == "pass")

passes_aggregate = passes %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize(count = n(), yga = mean(yards_gained)) %>%
  filter(count >= 50) %>%
  arrange(desc(yga))


## Stability hunting
pbp = load_pbp(2016:2023)

passes = pbp %>%
  filter(play_type == "pass")

ag_pass_ts = passes %>%
  group_by(season,
           passer_player_id,
           passer_player_name) %>%
  summarize(n = n(), yga = mean(yards_gained)) %>%
  filter(n > 100) %>%
  arrange(desc(yga))

ag_pass_ts$next_season = ag_pass_ts$season + 1
ag_pass_ts_copy = ag_pass_ts %>%
  rename(next_season = season, yga_next_season = yga)
joined = ag_pass_ts %>%
  full_join(ag_pass_ts_copy,
            by = c("passer_player_name",
                   "next_season",
                   "passer_player_id")) %>%
  arrange(passer_player_id, passer_player_name, season)

