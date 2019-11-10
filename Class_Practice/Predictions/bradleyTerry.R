library(SportsAnalytics270)
library(magrittr)
library(dplyr)
library(BradleyTerry2)
library(ggplot2)
data("nfl")
head(nfl)

nfl2 <- nfl %>%
  select(team_home, team_away, result) %>%
  filter(result != "T") %>%
  mutate(ht_w = ifelse(result == "H",1,0),
         at_w = ifelse(result == "A",1,0))
head(nfl2)

nfl3 <- nfl2 %>%
  mutate(team_home = as.factor(team_home),
         team_away = as.factor(team_away)) %>%
  group_by(team_home, team_away) %>%
  summarise(ht=sum(ht_w), at=sum(at_w))
head(nfl3, n = 5)

model <- BTm(cbind(ht, at), 
             team_home, 
             team_away, 
             data = nfl3, 
             id = "team_")
coef <- model$coefficients
sort(coef, decreasing = T)

BTabilities(model)

abilities <- as.data.frame(BTabilities(model))
abilities$team <- rownames(abilities)
abilities <- abilities[order(abilities$ability, decreasing = T),]
abilities


abilities %>% 
  ggplot(aes(x = reorder(team,ability), y = ability)) + 
  geom_bar(stat = "identity", fill = 1:32) + coord_flip() + 
  labs(x = "Teams", y = "Ability to Win", title = "Ability to win for NFL teams")


nfl4 <- nfl2 %>% 
  mutate(team_home = relevel(team_home, ref = "Cleveland Browns"),
         team_away = relevel(team_away, ref = "Cleveland Browns")) %>%
  group_by(team_home, team_away) %>%
  summarise(ht=sum(ht_w), at=sum(at_w))

abilities_brown <- abilities
abilities_brown$ability <- abilities_brown$ability - min(abilities_brown$ability)
head(abilities_brown)


broncos <- data.frame(team_home = rep("Denver Broncos", 4),
                      team_away = c("Cleveland Browns", "Tennessee Titans", 
                                      "Chicago Bears", "Detroit Lions"))

level_away <-levels(nfl3$team_away)
level_home <-levels(nfl3$team_home)

broncos$team_home <- factor(broncos$team_home,
                          levels(nfl3$team_away))
broncos$team_away <- factor(broncos$team_away,
                            levels(nfl3$team_away))

str(broncos)
broncos

broncos_prob <- predict(model, newdata = broncos, level = 2, type = "response")
broncos_prob

broncos_df <- data.frame(broncos, ht_w=broncos_prob,
                         at_w=1-broncos_prob)



broncos_ab <- abilities[rownames(abilities) == "Denver Broncos",]$ability
browns_ab <- abilities[rownames(abilities) == "Cleveland Browns",]$ability

broncos_ab
browns_ab

p_bronc_browns <- exp(broncos_ab)/(exp(broncos_ab) + exp(browns_ab))
p_bronc_browns

nfl3$team_home <- data.frame(team = nfl3$team_home, at.home = 1)
nfl3$team_away <- data.frame(team = nfl3$team_away, at.home = 0)

levels(nfl3$team_away) <- level_away
levels(nfl3$team_home) <- level_home

str(nfl3)
head(nfl3$team_away, n=5)
str(nfl3)

model2 <- BTm(cbind(ht, at), team_home, team_away, formula = ~team+at.home, data = nfl3, id = "team")
model2$coefficients

db <- coefficients(model2)['teamDenver Broncos']
cb <- coefficients(model2)['teamCleveland Browns']
h <- coefficients(model2)['at.home']

w_h <- exp(db + h) / (exp(db + h) + exp(cb))
od_h <- w_h / (1-w_h)
od_h

w_n <- exp(db) / (exp(db) + exp(cb))
w_n

od_n <- w_n / (1 - w_n)
od_n

od_h / od_n



broncos <- data.frame(team_home=rep("Denver Broncos", 4),
                      team_away=c("Cleveland Browns", "Tennessee Titans",
                                  "Chicago Bears", "Detroit Lions"))
broncos$team_home <- data.frame(team=factor(broncos$team_home, 
                                             level_home), at.home = 1)

broncos$team_away <- data.frame(team= factor(broncos$team_away, 
                                             level_away), at.home = 0)

broncos

br2 <- predict(model2, newdata=broncos, level = 2, type = "response")
br2
