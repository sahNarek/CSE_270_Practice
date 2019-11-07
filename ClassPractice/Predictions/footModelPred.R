library(dplyr)
library(SportsAnalytics270)
library(VGAM)
library(ggplot2)
data("f_data_sm")
df <- f_data_sm %>% mutate(FTAG = ifelse(FTAG >= 4, "4+", FTAG),
                           FTHG = ifelse(FTHG >= 4, "4+", FTHG))

table(Away = df$FTAG, Home = df$FTHG)

df %>% filter(SEASON == 2019) %>% group_by(COUNTRY) %>%
  summarise(pvalue = chisq.test(table(FTAG,FTHG), simulate.p.value = T)$p.value)

portugal <- f_data_sm %>%
  filter(SEASON == 2019, COUNTRY == "Portugal") %>%
  select(HOMETEAM, AWAYTEAM, FTHG, FTAG)

port1 <- data.frame(portugal[,c("HOMETEAM","AWAYTEAM", "FTHG")], Home = 1)
port2 <- data.frame(portugal[,c("AWAYTEAM","HOMETEAM", "FTAG")], Home = 0)

colnames(port1) <- c("Team", "Opponent", "Goal", "Home")
colnames(port2) <- colnames(port1)

portugal2 <- rbind(port1, port2)

model <- glm(Goal~Team+Opponent+Home, data = portugal2, family = poisson(link = log))
coefficients(model)

options(scipen = 1, digits = 4)
predict(model, data.frame(Home = 1, Team = "Benfica", Opponent = "Porto"), type = "response")
predict(model, data.frame(Home = 0, Team = "Porto", Opponent = "Benfica"), type = "response")


options(scipen = 1, digits = 4)
intercept <- coefficients(model)["(Intercept)"]
benf_home <- coefficients(model)["TeamBenfica"]
porto_away <- coefficients(model)["OpponentPorto"]
home <- coefficients(model)["Home"]

b_h <- exp(intercept + benf_home + porto_away + home)
b_a <- exp(intercept + benf_home + porto_away)
b_home_exp <- b_h/b_a
b_h
b_a

porto_home <- coefficients(model)["TeamPorto"]
benf_away <- coefficients(model)["OpponentBenfica"]
p_h <- exp(intercept + porto_home + benf_away + home)
p_a <- exp(intercept + porto_home + benf_away)
p_home_exp <- p_h / p_a
p_h
p_a
predict(model, data.frame(Home = 1, Team = "Benfica", Opponent = "Porto"), type = "response")
predict(model, data.frame(Home = 0, Team = "Porto", Opponent = "Benfica"), type = "response")

set.seed(1)

k <- rskellam(10000,mu1 = 1.522, mu2 = 1.342)

ggplot() + geom_bar(aes(x=k))

sum(dskellam(c(-100:-1), 1.522, 1.342))
sum(dskellam(0, 1.522, 1.34))
sum(dskellam(c(1:100), 1.522, 1.342))
