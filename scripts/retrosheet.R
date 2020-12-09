library(dplyr)
library(readr)
library(janitor)

# Major League Baseball team data obtained from https://www.retrosheet.org/gamelogs/index.html.
gl_2018_df <- read_csv("data/GL2018.txt", col_names = F)

# Details on variables obtained from https://www.retrosheet.org/gamelogs/glfields.txt.
gl_2018_df <- gl_2018_df %>% 
  rename(date = X1,             # yyyymmdd
         away_team = X4,        # away team
         home_team = X7,        # home team
         day_night = X13,       # day or night game
         completed = X14,       # details if game completed at later date
         pitch     = X17,       # pitch ID  
         attendance = X18,      # attendance
         game_time = X19) %>%   # game time in minutes
  select(date, away_team, home_team, day_night, completed, pitch, attendance, game_time) 

# Check missings.
lapply(gl_2018_df, function(x) sum(is.na(x)))

# Only issue is with 'complete' with 2 missings, so let's drop them
gl_2018_df <- gl_2018_df %>% 
  filter(is.na(completed))

# Retrieve pitch ID names from retrosheet website. This is also saved locally as parkcode.txt.
pitch_df <- read_csv("https://www.retrosheet.org/parkcode.txt")

# Select cols needed.
pitch_sub_df <- pitch_df %>% 
  select(PARKID, NAME, CITY, STATE) %>% 
  rename(pitch = PARKID)

# Left join with game-level data so we know the stadium names and city.
gl_2018_pitch_df <- left_join(gl_2018_df, pitch_sub_df)

# arrange
gl_2018_pitch_df <- gl_2018_pitch_df %>%
  clean_names() %>% 
  arrange(name)

# save
write_csv(x = gl_2018_pitch_df, path = "data/retrosheet_gl_2018.csv")

