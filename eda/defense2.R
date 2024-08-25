library(tidyverse)
library(caret)
library(nflfastR)
library(plotly)
pbp = load_pbp(2016:2023)


yg_plays = pbp %>%
  filter(!is.na(yards_gained), !is.na(defteam))

oneway.test(yards_gained ~ defteam, yg_plays, var.equal = F)

yards_ts = yg_plays %>%
  group_by(defteam, season) %>%
  summarize(av = mean(yards_gained), sd = sd(yards_gained)) %>%
  arrange(season, av)

yards_ts$rank = rep(1:32, 8)

yards_ts %>%
  pivot_wider(id_cols = defteam, names_from = season, values_from = rank) %>%
  write_csv("Data/defense_rank_pivot.csv")

defense_rank_pivot = read_csv("Data/defense_rank_pivot.csv")
ggplot(yards_ts, aes(x = season, y = defteam, fill = rank)) +
  geom_tile()
ggsave("visualizations/rank_heatmap.png")

yards_ts %>%
  group_by(defteam) %>%
  summarize(av_def_rank = mean(rank)) %>%
  arrange(av_def_rank)

ggplot(yards_ts, aes(x = season, y = av, color = defteam)) +
  geom_point() +
  geom_line()

plot_ly(yards_ts, type = "scatter",
        x = ~season, y = ~av, color = ~defteam) %>%
  add_trace(mode = "lines")

