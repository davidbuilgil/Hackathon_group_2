# load packages

library(sf)
library(tidyverse)
library(here)
library(vroom)
library(broom)

set.seed(nchar("take me to the ballgame") ^ 4)


# data input and wrangle ----------------------------------------------------------



dodger_stadium <- st_sfc(st_point(c(-118.24, 34.07455)),
                         crs = 4326)

days <- read_csv(here::here("data", "days.csv"))

crime <- vroom::vroom(here::here("data", "us_crime_2018.csv"))

crime <- crime %>%
  filter(city_name == "Los Angeles")


pnts <- 
crime %>% 
  select(long = longitude,
         lat = latitude)



pnts_sf <- st_as_sf(pnts, crs = 4326, coords = c("long", "lat"))
  
distances <- st_distance(pnts_sf, dodger_stadium)

crime <- bind_cols(crime, tibble(dist = distances))

crime <- crime %>%
  mutate(date = substr(date_single, 0, 10),
         date = lubridate::ymd(date))


buffer_km <- 5

crime_buffer <- 
crime %>%
  mutate(dist = as.numeric(dist)) %>% 
  mutate(within_5k_buffer = if_else(dist < buffer_km * 1000, 1, 0)) %>% 
  group_by(date, within_5k_buffer) %>%
  summarise(crimes     = n(),
            arson      = length(uid[offense_group == "arson"]),
            assault    = length(uid[offense_group == "assault offenses"]),
            damage     = length(uid[offense_group == "destruction/damage/vandalism of property (except arson)"]),
            disorder   = length(uid[offense_group == "disorderly conduct"]),
            dui        = length(uid[offense_group == "driving under the influence"]),
            drug       = length(uid[offense_group == "drug/narcotic offenses"]),
            larceny    = length(uid[offense_group == "larceny/theft offenses"]),
            vehi_theft = length(uid[offense_group == "motor vehicle theft"]),
            trespass = length(uid[offense_group == "trespass of real property"]),
            family     = length(uid[offense_group == "family offenses, nonviolent"])
            ) %>%
  ungroup()

days_no_crime <- 
days %>% 
  select(-crimes:-family)

crime_buffer <- 
left_join(
  crime_buffer,
  days_no_crime
)

crime_buffer <- 
  crime_buffer %>% 
  mutate(Decision = if_else(is.na(Decision), "No game", Decision),
         Decision = fct_relevel(Decision, "No game", "L", "W"))


# LA is 1,302 km2 (according to google)

la_size <- 1302 - buffer_km
  

crime_buffer <- 
crime_buffer %>% 
  mutate(
    area = if_else(within_5k_buffer == 1, buffer_km, la_size),
    doy = lubridate::yday(date),
    dow = factor(lubridate::wday(date))
  )


# modelling ---------------------------------------------------------------

# including the offset means we're modelling crime counts per km2

mod1 <- 
mgcv::gam(
  crimes ~ 1 + Decision * within_5k_buffer + s(doy, bs="cc") + dow,
  offset = log(area),
  data = crime_buffer,
  family = "quasipoisson"
)

summary(mod1)

# data to predict

new_df <- 
  tibble(
  Decision = c("L", "W", "No game"),
  within_5k_buffer = list(c(1, 0)),
  doy = 175,
  dow = 1,
  area = 5
)

new_df <- unnest(new_df)

preds <- 
broom::augment(
  mod1,
  newdata = new_df
)

# get overdispersion for quasipoisson

res <- summary(mod1)
disp <- as.data.frame(res$dispersion)[[1]]

rqpois <- function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu/(theta-1))
}


# simulate some expected values incorporating esstimation uncertainty
# (I think this should probably be mvnorm from the model's vcov, but this
# will do for now)

preds_long <- 
preds %>% 
  mutate(est = map2(.fitted, .se.fit, ~ rnorm(500, .x, .y))) %>% 
  unnest()

# simulate data from these predictions

preds_long %>% 
  mutate(draws = map(est, ~ rqpois(500, exp(.x), disp))) %>% 
  unnest(draws) %>% 
  #filter(within_5k_buffer == 1) %>% 
  ggplot(aes(x = draws, y = Decision, fill = Decision)) +
  ggridges::geom_density_ridges(alpha = 0.3,
                                stat = "binline") +
  facet_wrap(~ within_5k_buffer, scales = "free_x")



  

