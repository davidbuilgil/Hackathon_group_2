setwd("D:/Fellowship Manchester/Hackathon/Data")

library(dplyr)

crime <- read.csv("D:/Fellowship Manchester/Hackathon/Data/us_crime_2018.csv")

table(crime$offense_group)

crime <- crime %>%
  filter(city_name == "Los Angeles")

#crime <- crime %>%
#  filter(offense_group == "arson" | offense_group == "assault offenses" |
#         offense_group == "destruction/damage/vandalism of property (except arson)" |
#         offense_group == "disorderly conduct" | offense_group == "driving under the influence" |
#         offense_group == "drug/narcotic offenses" | offense_group == "larceny/theft offenses" |
#         offense_group == "motor vehicle theft" | offense_group == "trespass of real property" |
#         offense_group == "family offenses, nonviolent")

crime <- crime %>%
  mutate(date = substr(date_single, 0, 10),
         date = lubridate::ymd(date))

games <- read.csv("D:/Fellowship Manchester/Hackathon/Data/retrosheet_gl_2018.csv")

games <- games %>%
  filter(city == "Los Angeles")

games <- games %>%
  filter(name == "Dodger Stadium") %>%
  mutate(date = lubridate::ymd(date))

results <- read.csv("D:/Fellowship Manchester/Hackathon/Data/Dodgers_games.csv")

results <- results %>%
  rename(Date = 2) %>%
  mutate(date = lubridate::mdy(`Date`))

results <- results %>%
  filter(stringr::str_detect(Opponent, "vs "))

days <- crime %>%
  group_by(date) %>%
  summarise(crimes     = n(),
            arson      = length(uid[offense_group == "arson"]),
            assault    = length(uid[offense_group == "assault offenses"]),
            damage     = length(uid[offense_group == "destruction/damage/vandalism of property (except arson)"]),
            disorder   = length(uid[offense_group == "disorderly conduct"]),
            dui        = length(uid[offense_group == "driving under the influence"]),
            drug       = length(uid[offense_group == "drug/narcotic offenses"]),
            larceny    = length(uid[offense_group == "larceny/theft offenses"]),
            vehi_theft = length(uid[offense_group == "motor vehicle theft"]),
            vehi_theft = length(uid[offense_group == "trespass of real property"]),
            family     = length(uid[offense_group == "family offenses, nonviolent"]))

days <- days %>%
  left_join(games, by = "date") %>%
  left_join(results, by = "date")

write.csv(days, "days.csv")  

