# load packages

library(sf)
library(tidyverse)
library(here)
library(vroom)
library(broom)
library(forcats)
library(binom)

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
                                stat = "binline",
                                binwidth = 1) +
  facet_wrap(~ within_5k_buffer, scales = "free_x") +
  theme_minimal()


# looking at attendance on crime (for days when there are games on --------


games_only <- 
  crime_buffer %>% 
  filter(Decision != "No game")

games_only <- 
  games_only %>% 
  mutate(attendance_k = attendance / 1000,
         attendance_k_s = as.vector(scale(attendance_k, scale = FALSE)))

game_mod <- 
  mgcv::gam(
    crimes ~ 1 + Decision * within_5k_buffer + s(doy) 
    + dow + attendance_k_s + attendance_k_s : within_5k_buffer,
    offset = log(area),
    data = games_only,
    family = "quasipoisson"
  )

summary(game_mod)

coef(game_mod) %>% 
  as.data.frame() %>% 
  exp()

# extra 1k people associated with reduction of 0.7%... but consistent
# within buffer or not?

new_df_g <- 
  tibble(
    Decision = "W",
    within_5k_buffer = c(1, 0),
    doy = 175,
    attendance_k_s = list(c(-5, 0, 5)),
    dow = 1,
    area = 5
  )

new_df_g <- unnest(new_df_g)


preds_g <- 
  broom::augment(
    game_mod,
    newdata = new_df_g
  )

res_g <- summary(game_mod)
disp_g <- as.data.frame(res_g$dispersion)[[1]]


# simulate some expected values incorporating esstimation uncertainty
# (I think this should probably be mvnorm from the model's vcov, but this
# will do for now)

preds_long_g <- 
  preds_g %>% 
  mutate(est = map2(.fitted, .se.fit, ~ rnorm(500, .x, .y))) %>% 
  unnest()

# simulate data from these predictions

draws_df <- 
  preds_long_g %>% 
  mutate(draws = map(est, ~ rqpois(500, exp(.x), disp_g))) %>% 
  unnest(draws)

draws_df %>% 
  #filter(within_5k_buffer == 1) %>% 
  ggplot(aes(x = draws, y = factor(attendance_k_s), fill = Decision)) +
  ggridges::geom_density_ridges(alpha = 0.3,
                                stat = "binline",
                                binwidth = 1) +
  facet_wrap(~ within_5k_buffer, scales = "free_x") +
  labs(
    y = "Attendance (relative to average attendance)",
    x = "Simulated crime counts (per square km)",
    caption = "1 = within 5km of stadium, 0 = elsewhere in LA",
    title = "Every extra 1k in attendance is associated with ~0.7% less crime",
    subtitle = "But that's basically too small a difference to see!"
  ) +
  theme_minimal()


# crime types near and far from stadium -----------------------------------


prop_cis <- 
crime_buffer %>% 
  select(within_5k_buffer:family) %>% 
  select(-dui, -drug) %>% 
  group_by(within_5k_buffer) %>% 
  summarise_at(vars(crimes:family), ~ sum(.x)) %>% 
  mutate(crimes = rowSums(select(., arson:family))) %>% 
  gather(crime, n , arson:family) %>% 
  mutate(prop = map2(n, crimes, binom::binom.wilson)) %>% 
  unnest()

prop_cis %>% 
  ggplot(aes(x = mean, y = fct_reorder(crime, mean), colour = factor(within_5k_buffer))) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper),height = 0.23) +
  labs(x = "Proportion of all crimes",
       y = "Crime type",
       colour = "Within\n5k of\nstadium",
       title = "Close to Dodger stadium assaults are relatively more common",
       subtitle = "And vehicle thefts less common") +
  theme_minimal()
