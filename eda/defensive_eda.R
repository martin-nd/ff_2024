library(tidyverse)
library(caret)
library(nflfastR)
pbp = load_pbp(2016:2023)
to_select = c(
  "defteam",
  "posteam",
  "season",
  "down",
  "ydstogo",
  "yardline_100",
  "game_seconds_remaining",
  "half_seconds_remaining",
  "quarter_seconds_remaining",
  "play_type",
  "yards_gained",
  "epa",
  "success",
  "sack",
  "interception",
  "fumble_forced",
  "fumble_lost",
  "first_down_rush",
  "first_down_pass",
  "touchdown"
)
def_data1 = pbp %>%
    select(all_of(to_select))

touchdowns_py = def_data1 %>%
        filter(!is.na(touchdown)) %>%
        group_by(season, defteam) %>%
        summarize(touchdowns = sum(touchdown))

numeric = def_data1 %>%
      select_if(is.numeric) %>%
      .[complete.cases(.), ]

corrplot::corrplot(cor(numeric))


ml_vars = c(
  "down",
  "ydstogo",
  "yardline_100",
  "game_seconds_remaining",
  "half_seconds_remaining",
  "quarter_seconds_remaining",
  "play_type",
  "yards_gained",
  "defteam",
  "touchdown"
)

mod_data = def_data1 %>%
        select(all_of(ml_vars)) %>%
        .[complete.cases(.), ]

touchdowns = mod_data$touchdown
downs = mod_data$down
mod_data = mod_data %>%
        select(-c(touchdown, down))

numeric_mod_data = mod_data %>%
        select_if(is.numeric) %>%
        scale() %>%
        data.frame()

mod_data = mod_data %>%
        select_if(is.character) %>%
        cbind(numeric_mod_data) %>%
        mutate(touchdown = as.factor(touchdowns),
               down = downs)

mod_data_2 = mod_data %>%
        select(-c(play_type, defteam))

mod_data_3 = mod_data %>%
  select(-defteam)

mod_data_3 = model.matrix(touchdown ~ ., mod_data_3) %>%
  data.frame() %>%
  mutate(touchdown = as.factor(touchdowns))


processed_mod_data = model.matrix(touchdown ~ ., mod_data) %>%
        data.frame() %>%
        mutate(touchdown = as.factor(touchdowns))

use_data = processed_mod_data

inTrain = createDataPartition(use_data$touchdown, p = 0.8, list = F)

trainset = use_data[inTrain, ]
testset = use_data[-inTrain, ]

trCtrl = trainControl(method = "repeatedcv",
                      number = 5, repeats = 5)

begin = Sys.time()
model = train(touchdown ~ .,
              trainset, method = "glmnet",
              family = "binomial",
              trControl = trCtrl,
              tuneLength = 5)

end = Sys.time()

preds = predict(model, testset)

table(preds, testset$touchdown)