library(SportsAnalytics270)
library(dplyr)
library(magrittr)
library(ggplot2)

data("nba2009_2018")
nba2009_2018 %>%
  filter(SEASON_ID == "2018") %>%
  group_by(home.TEAM_ABBREVIATION) %>%
  summarise(p_value = shapiro.test(home.PTS)$p.value) %>%
  arrange(p_value)

home <- nba2009_2018 %>%
  filter(SEASON_ID == "2018") %>%
  group_by(home.TEAM_ABBREVIATION) %>%
  summarise(mean = mean(home.PTS), 
            var = var(home.PTS)) %>%
  arrange(mean,var)


away <- nba2009_2018 %>%
  filter(SEASON_ID == "2018") %>%
  group_by(away.TEAM_ABBREVIATION) %>%
  summarise(mean = mean(away.PTS), 
            var = var(away.PTS)) %>%
  arrange(mean,var)

was <- home %>% 
  filter(home.TEAM_ABBREVIATION == "WAS")

dal <- away %>%
  filter(away.TEAM_ABBREVIATION == "DAL")


mean <- (was %>%
  select(mean) - 
  dal %>%
  select(mean))$mean

sd <- sqrt(dal$var + was$var)

#P(X <= 0)
pnorm(0,mean,sd, lower.tail = T)
#P(X > 0)
pnorm(0,mean,sd, lower.tail = F)

home_away <- list(home, away)
probs <- function(data, home, away, spread=0){
  home_df <- data[[1]]
  away_df <- data[[2]]
  home_team <- (home_df %>% 
                  filter(home.TEAM_ABBREVIATION == home))
  away_team <- (away_df %>%
                  filter(away.TEAM_ABBREVIATION == home))
  mean_home <- home_team$mean
  mean_away <- away_team$mean
  mean_diff <- mean_home - mean_away
  var_home <- home_team$var
  var_away <- away_team$var
  
  sd_diff <- sqrt(var_home + var_away)
  
  df <- data.frame(pnorm(spread, mean_diff, sd_diff, lower.tail = F),
                   pnorm(spread, mean_diff, sd_diff))
  colnames(df) <- c(home,away)
  return(df)
}

probs(home_away, home="WAS", away = "DAL")
