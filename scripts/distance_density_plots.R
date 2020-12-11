
# Loading libraries: ------------------------------------------------------

library(tidyverse)
library(leaflet)
library(sf)
library(patchwork)

# Reading in data: --------------------------------------------------------

dodger_stadium <- st_sfc(st_point(c(-118.24, 34.07455)),
                         crs = 4326)

crime <- vroom::vroom(here::here("data", "us_crime_2018.csv"))

days <- read_csv(here::here("data", "days.csv"))

# Filtering crime: --------------------------------------------------------

crime <- crime %>%
  rename(date = date_single) %>%
  filter(city_name == "Los Angeles",
         lubridate::year(date) == 2018) %>% 
  mutate(date = lubridate::as_date(date))

# Playing with dates: -----------------------------------------------------

days <- days %>% 
  mutate(Decision = if_else(is.na(Decision), "No game", Decision),
         Decision = fct_relevel(Decision, "No game", "L", "W"),
         date = lubridate::as_date(date))

days_no_game <- days %>% 
  filter(Decision == "No game")

days_game <- days %>% 
  filter(Decision == "L" | Decision == "W" )

# Density plots: ----------------------------------------------------------

#All days:

la_crime_dist <- crime %>%
  mutate(crime_coord = st_as_sf(crime, coords = c("longitude","latitude"), crs = 4326),
         stadium_dist = st_distance(crime_coord, dodger_stadium))%>%
  filter(as.numeric(stadium_dist) < 500000,
         lubridate::year(date) == 2018)

la_crime_dist %>%
  select(stadium_dist) %>%
  ggplot(aes (x = (as.numeric(stadium_dist))/1000)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  labs(x = "Distance from Dodgers Stadium (in km)", y = "Density")


# Splitting crime data: ---------------------------------------------------

crime_match <- right_join(crime,
                          days_game,
                          by = "date")


crime_no_match <- right_join(crime,
                        days_no_game,
                        by = "date") %>%
  drop_na(longitude, latitude)


# Plots for game vs no game: ----------------------------------------------

#Game:

crime_match_dist <- crime_match %>%
  mutate(crime_coord = st_as_sf(crime_match, coords = c("longitude","latitude"), crs = 4326),
         stadium_dist = st_distance(crime_coord, dodger_stadium))%>%
  filter(as.numeric(stadium_dist) < 500000,
         lubridate::year(date) == 2018)

p1 <- crime_match_dist %>%
  select(stadium_dist) %>%
  ggplot(aes (x = (as.numeric(stadium_dist))/1000)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  labs(x = "Distance from Dodgers Stadium (in km)", y = "Density")

#No game:

crime_no_match_dist <- crime_no_match %>%
  mutate(crime_coord = st_as_sf(crime_no_match, coords = c("longitude","latitude"), crs = 4326),
         stadium_dist = st_distance(crime_coord, dodger_stadium))%>%
  filter(as.numeric(stadium_dist) < 500000,
         lubridate::year(date) == 2018)

p2 <- crime_no_match_dist %>%
  select(stadium_dist) %>%
  ggplot(aes (x = (as.numeric(stadium_dist))/1000)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  labs(x = "Distance from Dodgers Stadium (in km)", y = "Density")


#Both:

crime_match_dist <- crime_match_dist %>%
  mutate(`Match:` = "Yes")

crime_no_match_dist <- crime_no_match_dist %>%
  mutate(`Match:` = "No")


crime_binded <- crime_no_match_dist %>%
  bind_rows(crime_match_dist)

crime_binded %>%
  ggplot(aes(fill = `Match:`, color = `Match:`)) +
  geom_density(aes(x =(as.numeric(stadium_dist))/1000), alpha = 0.5)+
  theme_minimal() +
  labs(title = "Looking at the spatial distribution of crimes",
       subtitle = "Whether or not the Dodgers are playing doesn't seem to have a clear impact...",
       x = "Distance from Dodgers Stadium (in km):", 
       y = "Density (crimes):")




