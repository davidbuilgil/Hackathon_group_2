
# load packages -----------------------------------------------------------



library(here)
library(readr)
library(dplyr)
library(forcats)
library(tidyr)
library(purrr)
library(ggplot2)
library(stringr)


# import data -------------------------------------------------------------


days <- read_csv(here::here("data", "days.csv"))

days %>% 
  count(Decision)


# data wrangling ----------------------------------------------------------



days <- 
days %>% 
  mutate(Decision = if_else(is.na(Decision), "No game", Decision),
         Decision = fct_relevel(Decision, "No game", "L", "W"))


days %>% 
  count(attendance)

# dui and drug are all 0 so lets remove them

days <- 
days %>% 
  select(-drug, -dui)

# convert to long dataframe and nest by crime type

days_nest <- 
days %>% 
  select(crimes:vehi_theft, Decision, attendance) %>% 
  pivot_longer(crimes:vehi_theft) %>% 
  rename(crime_type = name,
         count = value) %>% 
  mutate(attendance = if_else(is.na(attendance), 0, attendance)) %>% 
  group_by(crime_type) %>% 
  nest()


# model fit function

nest_glm <- function(df){
  
glm(
  count ~ Decision, 
  data = df,
  family = "quasipoisson"
)

}


# fit model for each crime type -------------------------------------------


days_nest <- 
days_nest %>% 
  mutate(mod = map(data, nest_glm),
         params = map(mod, broom::tidy)) %>% 
  ungroup()

plot_dat <- 
days_nest %>% 
  select(-data, -mod) %>% 
  unnest(crime_type, params) %>% 
  mutate(conf_low = estimate - 1.96 * std.error,
         conf_high = estimate + 1.96 * std.error) %>% 
  mutate_at(vars(contains("conf_"), estimate), ~ exp(.x)) %>% 
  filter(!str_detect(term, "Intercept"))

crime_type_plot <- 
plot_dat %>% 
  ggplot(aes(x = estimate, y = term, colour = crime_type)) +
  geom_vline(xintercept = 1) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                    position = position_dodge(width = 0.6)) +
  labs(
    y = "Game result (win or lose)",
    caption = "Comparing crime counts on game days vs no game days by game result"
  )


# make the figures directory first if one doesn't exist
# dir.create(here::here("figures"))


ggsave(
  here::here("figures", "win_loss_crime_type.png"),
  crime_type_plot,
  width = 6,
  height = 4,
  type = "cairo-png"
)

