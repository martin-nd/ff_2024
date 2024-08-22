library(tidyverse)
library(nflfastR)

pbp = load_pbp(2023)

passes = pbp %>%
  filter(play_type == "pass")

adot = passes %>%
  group_by(passer_id, passer) %>%
  summarize(count = n(),
            adot = mean(air_yards, na.rm = T)) %>%
  filter(count >= 100 & !is.na(passer)) %>%
  arrange(desc(adot))

ggplot(pbp, aes(x = passing_yards)) +
  geom_histogram(binwidth = 1)

