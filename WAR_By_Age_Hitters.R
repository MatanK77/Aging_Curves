# Necessary Packages

library(tidyverse)
library(ggpubr)

# Import Data (can be found under dataset.csv)

dataset 

# Summary Data

war_age_summary <- dataset %>%
  filter(Season >= 2005) %>%
  mutate(
    Age_Group = case_when(
      Age < 25 ~ "24-",
      between(Age, 25, 29) ~ "25 to 29",
      between(Age, 30, 34) ~ "30 to 34",
      Age >= 35 ~ "35+"
    ), .after = Age
  ) %>%
  mutate(
    Season_Group = case_when(
      between(Season, 2005, 2009) ~ "2005-2009",
      between(Season, 2010, 2014) ~ "2010-2014",
      between(Season, 2015, 2019) ~ "2015-2019",
      Season >= 2020 ~ "2020-2023"
    ), .after = Season
  ) %>%
  group_by(Season_Group, Age_Group) %>%
  summarize(
    n_players = n(),
    players_500pa = sum(PA >= 500),
    players_2war = sum(WAR >= 2),
    total_war = round(sum(WAR), digits = 0),
    avg_war = round(mean(WAR), digits = 1),
    total_war_per_pa = round(sum(WAR) / sum(PA), digits = 4),
    `avg_wrc+` = round(weighted.mean(`wRC+`, w = PA), digits = 1),
    total_offense_per_600pa = round((sum(Off) / sum(PA)) * 600, digits = 1),
    total_defense_per_600pa = round((sum(Def) / sum(PA)) * 600, digits = 1),
  ) %>%
  ungroup() %>%
  group_by(Season_Group) %>%
  mutate(
    league_war = sum(total_war),
    league_war_percent = round((total_war / league_war) * 100, digits = 1),
    .after = total_war
  ) %>%
  ungroup()

# Plots

p1 <- ggplot(war_age_summary, aes(x = Age_Group, y = players_500pa)) +
  geom_col() +
  facet_wrap(~Season_Group) +
  labs(title = "Players with 500+ PA Seasons by Age Group 2005-2023", y = "Players With 500+ PA Seasons", x = "Age Group",
       caption = "Plot by Matan K/@mk237700. Data from Fangraphs")

p2 <- ggplot(war_age_summary, aes(x = Age_Group, y = total_war_per_pa * 500)) +
  geom_col() +
  facet_wrap(~Season_Group) +
  labs(title = "WAR Per 500 Plate Appearances by Age Group 2005-2023", y = "WAR Per 500 PA", x = "Age Group",
       caption = "Plot by Matan K/@mk237700. Data from Fangraphs")

ggarrange(p1, p2)

ggplot(war_age_summary, aes(x = Age_Group, y = players_2war)) +
  geom_col() +
  facet_wrap(~Season_Group) +
  labs(title = "Players with 2+ WAR Seasons by Age Group 2005-2023", y = "Players With 2+ WAR Seasons", x = "Age Group",
       caption = "Plot by Matan K/@mk237700. Data from Fangraphs")


ggplot(war_age_summary, aes(x = Age_Group, y = league_war_percent)) +
  geom_col() +
  facet_wrap(~Season_Group) +
  labs(title = "% of Total WAR by Age Group 2005-2023", y = "% of Total WAR", x = "Age Group",
       caption = "Plot by Matan K/@mk237700. Data from Fangraphs")

