# load the libraries
library(tidyverse)
library(ggrepel)
library(ggimage)
library(gt)
library(ggpubr)
library(nflfastR)
nflreadr::.clear_cache()
# load the qb epa data from the past 5 years
qbdata_21 <- load_pbp(2021) %>%
  group_by(passer_player_name) %>%
  filter(!is.na(epa)) %>%
  summarize(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
qbdata_20 <- load_pbp(2020) %>%
  group_by(passer_player_name) %>%
  filter(!is.na(epa)) %>%
  summarize(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
qbdata_19 <- load_pbp(2019) %>%
  group_by(passer_player_name) %>%
  filter(!is.na(epa)) %>%
  summarize(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
qbdata_18 <- load_pbp(2018) %>%
  group_by(passer_player_name) %>%
  filter(!is.na(epa)) %>%
  summarize(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
qbdata_17 <- load_pbp(2017) %>%
  group_by(passer_player_name) %>%
  filter(!is.na(epa)) %>%
  summarize(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
# now that the data is loaded we can move on to the standings data
# we can use the r function readxl to take the excel data and put it into R
library(readxl)
standings_21 <- read_excel("2021 NFL Standings.xlsx")
standings_20 <- read_excel("2020 NFL Standings.xlsx")
standings_19 <- read_excel("2019 NFL Standings.xlsx")
standings_18 <- read_excel("2018 NFL Standings.xlsx")
standings_17 <- read_excel("2017 NFL Standings.xlsx")
# here i join the data by passer_player_name and the column from my excel sheet 'Leading Passer'
# this eliminates twams to just one quarterback that is representative of their performance
overall_21 <- qbdata_21 %>%
  inner_join(standings_21, by = c(passer_player_name = 'Leading Passer'))
overall_20 <- qbdata_20 %>%
  inner_join(standings_20, by = c(passer_player_name = 'Leading Passer'))
overall_19 <- qbdata_19 %>%
  inner_join(standings_19, by = c(passer_player_name = 'Leading Passer'))
overall_18 <- qbdata_18 %>%
  inner_join(standings_18, by = c(passer_player_name = 'Leading Passer'))
overall_17 <- qbdata_17 %>%
  inner_join(standings_17, by = c(passer_player_name = 'Leading Passer'))
# after joining you can familiarize yourself with the data
overall_21 %>%
  view()
overall_20 %>%
  view()
overall_19 %>%
  view()
overall_18 %>%
  view()
overall_17 %>%
  view()
# using 'print' you can also familiarize yourself with data types, such as 'dbl' for win percentage and avg_epa
print(overall_21)
print(overall_20)
print(overall_19)
print(overall_18)
print(overall_17)
# now that we have the overall dataset that i want we can plot the data
# we can use the 'join' function to join the data with team_colors_logos to plot points to easier relate to teams
# we need to create new data sets so we're going to have another set of data to include the team data and numerical data
total_21 <- overall_21 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
total_20 <- overall_20 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
total_19 <- overall_19 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
total_18 <- overall_18 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
total_17 <- overall_17 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
# now that we've joined the dataset with teams_colors_logos we can plot the data!
total_21 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2021 QB EPA",
       y = "2021 Team Win Percentage",
       title = "2021 QB and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
total_20 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2020 QB EPA", 
       y = "2020 Team Win Percentage",
       title = "2020 QB and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
total_19 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2019 QB EPA", 
       y = "2019 Team Win Percentage",
       title = "2019 QB and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
total_18 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2018 QB EPA", 
       y = "2018 Team Win Percentage",
       title = "2018 QB and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
total_17 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2017 QB EPA", 
       y = "2017 Team Win Percentage",
       title = "2017 QB and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# now we can also look to calculate the correlation coefficients
cor(total_21$avg_epa, total_21$`Win Percentage`, use = "complete.obs")
cor(total_20$avg_epa, total_20$`Win Percentage`, use = "complete.obs")
cor(total_19$avg_epa, total_19$`Win Percentage`, use = "complete.obs")
cor(total_18$avg_epa, total_18$`Win Percentage`, use = "complete.obs")
cor(total_17$avg_epa, total_17$`Win Percentage`, use = "complete.obs")
# now i want to look for the data for only Matthew Stafford
matt_stafford17 <- overall_17 %>%
  filter(passer_player_name == "M.Stafford") %>%
  inner_join(standings_17, by = c(passer_player_name = "Leading Passer"))
matt_stafford18 <- overall_18 %>%
  filter(passer_player_name == "M.Stafford") %>%
  inner_join(standings_18, by = c(passer_player_name = "Leading Passer"))
matt_stafford19 <- overall_19 %>%
  filter(passer_player_name == "M.Stafford") %>%
  inner_join(standings_19, by = c(passer_player_name = "Leading Passer"))
matt_stafford20 <- overall_20 %>%
  filter(passer_player_name == "M.Stafford") %>%
  inner_join(standings_20, by = c(passer_player_name = "Leading Passer"))
matt_stafford21 <- overall_21 %>%
  filter(passer_player_name == "M.Stafford") %>%
  inner_join(standings_21, by = c(passer_player_name = "Leading Passer"))
# I'm going to create a dataset to plot the data for matthew stafford
epa <- c(matt_stafford17$avg_epa, matt_stafford18$avg_epa, matt_stafford19$avg_epa, matt_stafford20$avg_epa, matt_stafford21$avg_epa) 
team_abbr <- c(matt_stafford17$team_abbr, matt_stafford18$team_abbr, matt_stafford19$team_abbr, matt_stafford20$team_abbr, matt_stafford21$team_abbr)                               
win_pct <- c(matt_stafford17$`Win Percentage.x`, matt_stafford18$`Win Percentage.x`, matt_stafford19$`Win Percentage.x`, matt_stafford20$`Win Percentage.x`, matt_stafford21$`Win Percentage.x`)
matt_stafford <- data_frame(epa, team_abbr, win_pct)
# plot the stafford data
matt_stafford <- matt_stafford %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
matt_stafford %>%
  ggplot(aes(x=epa, y=win_pct)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "Matthew Stafford EPA 2017-2021",
       y = "Matthew Stafford Team Win Percentage 2017-2021",
       title = "Matthew Stafford"
       ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
cor(matt_stafford$epa, matt_stafford$win_pct, use = "complete.obs")
# Jeez, for Stafford it looks like there's no relation that his performance leads to success
# this can also possibly apply to other quarterbacks like Jared Goff, Deshaun Watson and Kirk Cousins.
# maybe there are other lurking variables that can be indicators of team success so I wanted to look at defensive_epa by team
# It's possible that defensive_epa is more correlated with higher win percentages than qb_epa
defense_21 <- nflfastR::load_pbp(2021, qs = TRUE) %>%
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
defense_20 <- nflfastR::load_pbp(2020, qs = TRUE) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
defense_19 <- nflfastR::load_pbp(2019, qs = TRUE) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
defense_18 <- nflfastR::load_pbp(2018, qs = TRUE) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
defense_17 <- nflfastR::load_pbp(2017, qs = TRUE) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
# join the defense_epa data and win percentage
DS_21 <- defense_21 %>% inner_join(standings_21, by = c(defteam = "Team Abbreviation"))
DS_20 <- defense_20 %>% inner_join(standings_20, by = c(defteam = "Team Abbreviation"))
DS_19 <- defense_19 %>% inner_join(standings_19, by = c(defteam = "Team Abbreviation"))
DS_18 <- defense_18 %>% inner_join(standings_18, by = c(defteam = "Team Abbreviation"))
DS_17 <- defense_17 %>% inner_join(standings_17, by = c(defteam = "Team Abbreviation"))
# now that we have our new dataframes, we can plot the data and find correlation coefficients. 
# PLOT
DS_21 <- DS_21 %>% left_join(teams_colors_logos, by = c(defteam = "team_abbr"))
DS_20 <- DS_20 %>% left_join(teams_colors_logos, by = c(defteam = "team_abbr"))
DS_19 <- DS_19 %>% left_join(teams_colors_logos, by = c(defteam = "team_abbr"))
DS_18 <- DS_18 %>% left_join(teams_colors_logos, by = c(defteam = "team_abbr"))
DS_17 <- DS_17 %>% left_join(teams_colors_logos, by = c(defteam = "team_abbr"))
DS_21 %>%
  ggplot(aes(x=def_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2021 Defensive EPA", 
       y = "2021 Team Win Percentage",
       title = "2021 Defense and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
DS_20 %>%
  ggplot(aes(x=def_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2020 Defensive EPA", 
       y = "2020 Team Win Percentage",
       title = "2020 Defense and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
DS_19 %>%
  ggplot(aes(x=def_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2019 Defensive EPA", 
       y = "2019 Team Win Percentage",
       title = "2019 Defense and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
DS_18 %>%
  ggplot(aes(x=def_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2018 Defensive EPA", 
       y = "2018 Team Win Percentage",
       title = "2018 Defense and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
DS_17 %>%
  ggplot(aes(x=def_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs(x = "2017 Defensive EPA", 
       y = "2017 Team Win Percentage",
       title = "2017 Defense and its Relation to Win Percentage") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# now that we've plotted the data, we can look for the correlation coefficients and the average correlation coefficient over the past 5 years
-cor(DS_21$def_epa, DS_21$`Win Percentage`, use = "complete.obs")
-cor(DS_20$def_epa, DS_20$`Win Percentage`, use = "complete.obs")
-cor(DS_19$def_epa, DS_19$`Win Percentage`, use = "complete.obs")
-cor(DS_18$def_epa, DS_18$`Win Percentage`, use = "complete.obs")
-cor(DS_17$def_epa, DS_17$`Win Percentage`, use = "complete.obs")
((-cor(DS_21$def_epa, DS_21$`Win Percentage`, use = "complete.obs")) + 
  (-cor(DS_20$def_epa, DS_20$`Win Percentage`, use = "complete.obs")) + 
    (-cor(DS_19$def_epa, DS_19$`Win Percentage`, use = "complete.obs")) +
                (-cor(DS_18$def_epa, DS_18$`Win Percentage`, use = "complete.obs")) + 
                (-cor(DS_17$def_epa, DS_17$`Win Percentage`, use = "complete.obs")))/5
(cor(total_21$avg_epa, total_21$`Win Percentage`, use = "complete.obs") +
cor(total_20$avg_epa, total_20$`Win Percentage`, use = "complete.obs") +
cor(total_19$avg_epa, total_19$`Win Percentage`, use = "complete.obs") +
cor(total_18$avg_epa, total_18$`Win Percentage`, use = "complete.obs") +
cor(total_17$avg_epa, total_17$`Win Percentage`, use = "complete.obs"))/5
# it looks like qb_epa is more correlated with win percentage than defensive_epa is 
# however, both qb_epa and defensive_epa are somewhat correlated with a higher win percentage in the NFL.