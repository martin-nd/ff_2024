library(tidyverse)
library(nflfastR)
pbp = load_pbp(2016:2023)

# 1 point for every 25 passing yards / 1 point for every 10 receiving yards
freq_throwers = pbp %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize(n = n()) %>%
  filter(n >= 50, !is.na(passer_player_id)) %>%
  .$passer_player_id

pr_stats = pbp %>%
  filter(play_type == "pass", passer_player_id %in% freq_throwers,
         (passing_yards == receiving_yards) | (is.na(passing_yards) & is.na(receiving_yards))) %>%
  select(season, passer_player_id, passer_player_name, receiver_player_id, receiver_player_name,
         passing_yards, play_type, receiving_yards, sack, complete_pass)

passer_stats = pr_stats %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize()

