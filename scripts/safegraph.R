library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


safe_df <- read_csv("data/neighborhood_patterns_june_2020.gz")

cali_df <- safe_df %>%
  filter(region == "CA")

write_csv(x = cali_df, path = "data/cali_nhood_safegraph.csv")





