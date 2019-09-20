library(SportsAnalytics270)
library(dplyr)
library(ggplot2)
library(magrittr)
library(VGAM)
data("f_data_sm")
head(f_data_sm)

f_data_sm %>%
  filter(SEASON == '2019') %>%
  group_by(COUNTRY) %>%
  summarise(average = mean(FTTG, na.rm = T)) %>%
  arrange(desc(average))

seria_a <- f_data_sm %>%
  filter(SEASON == '2019', COUNTRY == 'Italy') %>%
  group_by(FTHG) %>%
  summarise(count = n()) %>%
  as.data.frame()

rel_freq <- seria_a$count / sum(seria_a$count) 

f_data_sm %>%
  filter(COUNTRY == "Italy", SEASON == "2019") %>%
  summarise(mean(FTHG))

probs <- dpois(c(0:6), lambda = 1.484211)
probs

chisq.test(rel_freq, p = probs, rescale.p = T)


home_means <- f_data_sm %>% 
  filter(COUNTRY == "Italy", SEASON == "2019") %>%
  group_by(HOMETEAM) %>%
  summarise(average = mean(FTHG)) %>%
  arrange(desc(average))


away_means <- f_data_sm %>% 
  filter(COUNTRY == "Italy", SEASON == "2019") %>%
  group_by(AWAYTEAM) %>%
  summarise(average = mean(FTAG)) %>%
  arrange(desc(average))

lazio <- home_means$average[home_means$HOMETEAM == "Lazio"]
fiorentina <- home_means$average[away_means$AWAYTEAM == "Fiorentina"]

options(scipen=999)
goal_probs_lazio <- dpois(c(0:9), lambda = lazio)
goal_probs_fiorentina<- dpois(c(0:9), lambda = fiorentina)

a <- goal_probs_lazio %*% t(goal_probs_fiorentina)
print(round(a, digits = 4))

draw <- sum(diag(a))
lazio_win <- sum(a[lower.tri(a, diag = F)])
fio_win <- sum(a[upper.tri(a, diag = F)])


home_goals <- f_data_sm %>%
  filter(COUNTRY == "England", SEASON == "2014") %>%
  summarise(mean(FTHG))

away_goals <- f_data_sm %>%
  filter(COUNTRY == "England", SEASON == "2014") %>%
  summarise(mean(FTAG))


premier <- f_data_sm %>%
  filter(SEASON == '2014', COUNTRY == 'England') %>%
  group_by(HOMETEAM) %>%
  summarise(average = mean(FTHG)) %>%
  as.data.frame()

premier_away <- f_data_sm %>%
  filter(SEASON == '2014', COUNTRY == 'England') %>%
  group_by(AWAYTEAM) %>%
  summarise(average = mean(FTAG)) %>%
  as.data.frame()

prem_rel_freq <- premier$count / sum(premier$count)
prem_away_freq <- premier_away$count / sum(premier_away$count)

prem_mean <- premier %>% 
  summarise(mean(FTHG))
prem_away_mean <- premier_away %>% 
  summarise(mean(FTAG))

prem_mean
prem_away_mean

liverpool <- premier$average[premier$HOMETEAM == "Liverpool"]
chelsea <- premier_away$average[premier_away$AWAYTEAM == "Chelsea"]

options(scipen=999)
goal_probs_liv <- dpois(c(0:9), lambda = liverpool)
goal_probs_chel<- dpois(c(0:9), lambda = chelsea)



a <- goal_probs_liv %*% t(goal_probs_chel)
print(round(a, digits = 4))
k <- rskellam(10000, mu1 = 1.894737, mu2 = 1.157895)

ggplot() + geom_bar(aes(x = k))

sum(k>0)/10000
sum(k==0)/10000