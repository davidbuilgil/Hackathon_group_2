library(crimedata)

crimes_df <- get_crime_data(
  years = 2018, 
  type = "core"
) 

crimes_df <- crimes_df %>% 
  select(-date_start, -date_end)

write_csv(x = crimes_df, path = "data/us_crime_2018.csv")
