# Necessary Packages

library(mgcv)
library(tidyverse)
library(ggpubr)

# Filter dataset (dataset.csv)

fg_pos_player_age_curve <- dataset %>%
  filter(PA >= 100 & between(Age, 20, 40)) %>%
  mutate(PlayerId = as.factor(PlayerId),
         def_600 = (Def / PA) * 600) %>%
  rename(wRC_plus = `wRC+`, k_rate = `K%`, bb_rate = `BB%`, contact_rate = `Contact%`)

# Add columns for averages

fg_pos_player_career_avgs <- fg_pos_player_age_curve %>%
  group_by(PlayerId) %>%
  summarize(
    career_wRC_plus = weighted.mean(wRC_plus, PA),
    career_ISO = weighted.mean(ISO, PA),
    career_BABIP = weighted.mean(BABIP, PA), 
    career_krate = weighted.mean(k_rate, PA),
    career_bbrate = weighted.mean(bb_rate, PA),
    career_contactrate = weighted.mean(contact_rate, PA, na.rm = TRUE),
    career_fld = weighted.mean(Fld, PA),
    career_bsr = weighted.mean(BsR, PA),
    career_pos_adj = weighted.mean(Pos, PA),
    career_def_600 = weighted.mean(def_600, PA)
  ) %>%
  ungroup()


fg_pos_player_age_curve <- left_join(fg_pos_player_age_curve, fg_pos_player_career_avgs, by = "PlayerId")


# Add columns for seasonal averages

fg_pos_player_season_avgs <- fg_pos_player_age_curve %>%
  group_by(Season) %>%
  summarize(
    season_ISO = weighted.mean(ISO, PA),
    season_BABIP = weighted.mean(BABIP, PA), 
    season_krate = weighted.mean(k_rate, PA),
    season_bbrate = weighted.mean(bb_rate, PA),
    season_contactrate = weighted.mean(contact_rate, PA, na.rm = TRUE)
  ) %>%
  ungroup()


fg_pos_player_age_curve <- left_join(fg_pos_player_age_curve, fg_pos_player_season_avgs, by = "Season")

# Adding columns for player stats relative to lg avg

fg_pos_player_age_curve <- fg_pos_player_age_curve %>%
  mutate(
    iso_vs_avg = ISO - season_ISO,
    babip_vs_avg = BABIP - season_BABIP,
    krate_vs_avg = k_rate - season_krate,
    bbrate_vs_avg = bb_rate - season_bbrate,
    contact_vs_avg = contact_rate - season_contactrate
  )


# Split dataset into 2005-2014 and 2015-2023

fg_pos_player_age_curve_2015 <- fg_pos_player_age_curve %>%
  filter(Season >= 2015)

fg_pos_player_age_curve_2014 <- fg_pos_player_age_curve %>%
  filter(between(Season, 2005, 2014))

### Aging curves

# Demonstration of how regular curve is too flat:

age_curve_demo <- gam(wRC_plus ~ s(Age), data = fg_pos_player_age_curve_2015, weights = PA)

summary(age_curve_demo)

plot(age_curve_demo, xlab = "Age", ylab = "Impact on wRC+", main = "Aging Curve (Just Age) for wRC+ Since 2015",
     xlim = c(20,40), ylim = c(-30, 10))

# 2005-2014 version demo

age_curve_demo2 <- gam(wRC_plus ~ s(Age), data = fg_pos_player_age_curve_2014, weights = PA)

summary(age_curve_demo2)

plot(age_curve_demo2, xlab = "Age", ylab = "Impact on wRC+", main = "Aging Curve (Just Age) for wRC+ Through 2014",
     xlim = c(20,40), ylim = c(-30, 10))

## wRC+ curve (with career mean)


age_curve_model_2015_wRC_plus <- gam(wRC_plus ~ s(Age) + career_wRC_plus, data = fg_pos_player_age_curve_2015, weights = PA)

summary(age_curve_model_2015_wRC_plus)

plot(age_curve_model_2015_wRC_plus, xlab = "Age", ylab = "Impact on wRC+", main = "Aging Curve for wRC+ Since 2015",
     xlim = c(20,40), ylim = c(-30, 10))

 # Creating age range to visualize curve

  age_range_2015 <- data.frame(Age = seq(from = 20, to = 40))

 # Adding average career wRC+ to isolate age impact in predictions
  
  age_range_2015$career_wRC_plus <- mean(fg_pos_player_age_curve_2015$career_wRC_plus)
  
 # wRC+ predictions based on age
  
  age_range_2015$predwRC_plus <- predict(age_curve_model_2015_wRC_plus, newdata = age_range_2015, type = "terms")[, "s(Age)"]
  

  
## Factor Smoothing wRC+ curve (test; runs slowly can skip)

age_curve_model_2015_wRC_plus_RE <- gam(wRC_plus ~ s(Age) + s(PlayerId, bs="fs"), data=fg_pos_player_age_curve_2015, method="REML", weights=PA)

summary(age_curve_model_2015_wRC_plus_RE)

plot(age_curve_model_2015_wRC_plus_RE, xlab = "Age", ylab = "Impact on wRC+", main = "Aging Curve for wRC+ Since 2015 Factor Smoothing",
     xlim = c(20,40), ylim = c(-30, 10))

# Adding a random PlayerId column (is irrelevant for predictions)

age_range_2015$PlayerId <- 20003

# wRC_plus predictions based on age alone

age_range_2015$predwRC_plus_RE <- predict(age_curve_model_2015_wRC_plus_RE, newdata = age_range_2015, type = "terms")[, "s(Age)"]


## Plotting both curves next to each other

# Transform data to a long format 

long_age_range_2015 <- age_range_2015 %>%
  gather(key = "PredictionType", value = "wRC_plus", predwRC_plus, predwRC_plus_RE)

# Plotting

ggplot(long_age_range_2015, aes(x = Age, y = wRC_plus, color = PredictionType)) +
  geom_point() +
  scale_color_manual(values = c("predwRC_plus" = "Blue", "predwRC_plus_RE" = "Red"),
                     name = "Prediction Type",
                     labels = c("predwRC_plus" = "wRC+ Aging Curve Career Mean", "predwRC_plus_RE" = "wRC+ Aging Curve Factor Smoothing")) +
  xlim(c(20, 40)) +
  ylim(c(-30, 10)) +
  labs(x = "Age", y = "wRC+")

## The 2 curves are pretty similar. In general, using career mean is preferable in terms of time to run and
# prior test on efficacy by Jonathan Judge.


### 2005-2014 aging curve

## wRC+ curve (career mean)

age_curve_model_2014_wRC_plus <- gam(wRC_plus ~ s(Age) + career_wRC_plus, data = fg_pos_player_age_curve_2014, weights = PA)

summary(age_curve_model_2014_wRC_plus)

plot(age_curve_model_2014_wRC_plus, xlab = "Age", ylab = "Impact on wRC+", main = "Aging Curve for wRC+ Through 2014",
     xlim = c(20,40), ylim = c(-30, 10))

# Creating age range to visualize curve

age_range_2014 <- data.frame(Age = seq(from = 20, to = 40))

# Adding average career wRC_plus to isolate age impact in predictions

age_range_2014$career_wRC_plus <- mean(fg_pos_player_age_curve_2014$career_wRC_plus)

# wRC_plus predictions based on age

age_range_2014$predwRC_plus_2014 <- predict(age_curve_model_2014_wRC_plus, newdata = age_range_2014, type = "terms")[, "s(Age)"]

## Join curves from both time periods

age_range_2014 <- age_range_2014 %>%
  rename(career_wRC_plus_2014 = career_wRC_plus)

age_range_both <- left_join(age_range_2015, age_range_2014, by = "Age") 

## Plot both career mean curves 

# Transform data to a long format 

long_age_range_both <- age_range_both %>%
  gather(key = "PredictionType", value = "wRC_plus", predwRC_plus, predwRC_plus_2014)

# Plotting

ggplot(long_age_range_both, aes(x = Age, y = wRC_plus, color = PredictionType)) +
  geom_point() +
  scale_color_manual(values = c("predwRC_plus" = "Blue", "predwRC_plus_2014" = "Red"),
                     name = "Aging Curve",
                     labels = c("predwRC_plus" = "wRC+ 2015-2023", "predwRC_plus_2014" = "wRC+ 2005-2014")) +
  xlim(c(20, 40)) +
  ylim(c(-30, 10)) +
  labs(x = "Age", y = "wRC+", title = "Comparison of wRC+ Aging Curves", caption = "Plot by Matan K/@mk237700. Data from Fangraphs")
