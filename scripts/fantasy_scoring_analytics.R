library(tidyverse)
library(nflfastR)
pbp = load_pbp(2016:2023)
roster = read_csv("Data/current_roster.csv")[, 2:5]

# 1 point for every 25 passing yards / 1 point for every 10 receiving yards
freq_throwers = pbp %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize(n = n()) %>%
  filter(n >= 50, !is.na(passer_player_id)) %>%
  .$passer_player_id

freq_receivers = pbp %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarize(n = n()) %>%
  filter(n >= 50, !is.na(receiver_player_id)) %>%
  .$receiver_player_id

pr_stats = pbp %>%
  filter(play_type == "pass", passer_player_id %in% freq_throwers,
         (passing_yards == receiving_yards) | (is.na(passing_yards) & is.na(receiving_yards))) %>%
  select(season, passer_player_id, passer_player_name, receiver_player_id, receiver_player_name,
         passing_yards, play_type, receiving_yards, sack, complete_pass)

qb_stats = pr_stats %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize(mpy = mean(passing_yards, na.rm = T),
            inv_sack_rate = 1 - mean(sack),
            completion_rate = mean(complete_pass)) %>%
  mutate(eyp = mpy * inv_sack_rate * completion_rate) %>%
  arrange(-eyp) %>%
  rename(player_id = passer_player_id) %>%
  left_join(roster, by = "player_id") %>%
  filter(position == "QB")

write_csv(qb_stats, "Data/qb_passing_stats.csv")

receiver_stats = pbp %>%
  filter(!is.na(receiver_player_id), receiver_player_id %in% freq_receivers) %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarize(mry = mean(receiving_yards, na.rm = T), n = n()) %>%
  rename(player_id = receiver_player_id) %>%
  left_join(roster, by = "player_id") %>%
  filter(position != "QB", !is.na(position), !is.na(mry)) %>%
  arrange(-mry) %>%
  filter(n >= 50)

