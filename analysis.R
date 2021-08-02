library(tidyverse)

# First let's look at blowouts
blowouts <- read_csv("data/point_diff_blowouts.csv")

# let's remove 1982 since there was a strike and then add in the season length
# for each year as well as the number of teams in the league so we can calculate
# the total number of games played in each season
blowouts_add_games <- blowouts %>%
  filter(Year != 1982) %>% # strike season
  group_by(Year) %>%
  summarize(blowouts = n()) %>%
  ungroup() %>%
  mutate(games_played_per_tm = case_when(Year <= 1977 ~ 14,
                                  Year >= 1978 ~ 16), # NFL went to 16 games
         teams_lg = case_when(Year <= 1975 ~ 26, 
                              Year >= 1976 & Year <= 1994 ~ 28, # Hawks and TB expansions
                              Year >= 1995 & Year <= 1998 ~ 30, # Panthers & Jags
                              Year >= 1999 & Year <= 2001 ~ 31, # Return of the Browns
                              Year >= 2002 & Year <= 2020 ~ 32), # realignment, Texans begin run of infamy 
         lg_games = games_played_per_tm * teams_lg,
         blowout_pct = round(blowouts / lg_games, 3) * 100) %>%
  select(Year, blowout_pct)

blowouts_add_games %>% 
  ggplot(aes(x = Year, y = blowout_pct)) +
  geom_vline(xintercept = 1994, color = "grey", linetype = "dashed") +
  geom_line() +
  scale_y_continuous(limits = c(0, 12)) +
  theme_minimal() +
  labs(x = "Season", y = "Blowouts as % of games played",
       title = "Blowouts in the NFL as a percentage of games played, 1970-2020*", caption = "Source: ProFootballReference",
       subtitle = "Blowouts defined as point differentials of 24 or more.*1982 strike year removed. 2010 was uncapped.")

# Let's look at three point games
close_games <- read_csv("data/point_diff_3point.csv")

# let's remove 1982 since there was a strike and then add in the season length
# for each year as well as the number of teams in the league so we can calculate
# the total number of games played in each season

close_games_add_games <- close_games %>%
  filter(Year != 1982) %>% # strike season
  group_by(Year) %>%
  summarize(squeakers = n()) %>% # number of blowouts
  ungroup() %>%
  mutate(games_played_per_tm = case_when(Year <= 1977 ~ 14,
                                         Year >= 1978 ~ 16), # NFL went to 16 games
         teams_lg = case_when(Year <= 1975 ~ 26,
                              Year >= 1976 & Year <= 1994 ~ 28, # Hawks and TB expansions
                              Year >= 1995 & Year <= 1998 ~ 30, # Panthers & Jags
                              Year >= 1999 & Year <= 2001 ~ 31, # Return of the Browns
                              Year >= 2002 & Year <= 2020 ~ 32), # realignment, Texans begin run of infamy 
         lg_games = games_played_per_tm * teams_lg,
         squeaker_pct = round(squeakers / lg_games, 3) * 100) %>%
  select(Year, squeaker_pct)

close_games_add_games %>% 
  ggplot(aes(x = Year, y = squeaker_pct)) +
  geom_vline(xintercept = 1994, color = "grey", linetype = "dashed") +
  geom_line() +
  scale_y_continuous(limits = c(0, 12)) +
  theme_minimal() +
  labs(x = "Season", y = "Squeakers as % of games played",
       title = "Squeakers in the NFL as a percentage of games played, 1970-2020*", caption = "Source: ProFootballReference",
       subtitle = "Squeakers defined as point differentials of 3.*1982 strike year removed. 2010 was uncapped.")
