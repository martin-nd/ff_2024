library(tidyverse)

analyze_yoy = function(data, stat, player_id_col, player_name_col) {
  agg = data %>%
    group_by(season, !!sym(player_id_col), !!sym(player_name_col)) %>%
    summarize(stat_mean = mean(!!sym(stat)), n = n()) %>%
    filter(n >= quantile(n)[3])

  agg2 = agg %>%
    rename(next_season = season, ns_stat_mean = stat_mean)

  agg = agg %>%
    mutate(next_season = season + 1)

  joined = agg %>%
    full_join(agg2, by = c("next_season", player_id_col, player_name_col)) %>%
    arrange(!!sym(player_name_col), !!sym(player_id_col), season) %>%
    filter(!is.na(season),
           !is.na(next_season),
           !is.na(stat_mean),
           !is.na(ns_stat_mean))

  mod = lm(ns_stat_mean ~ stat_mean, joined)
  sum = summary(mod)

  return(list(yoy_table = joined,
              model = mod,
              summary = sum))
}