library(SportsAnalytics270)
library(ggplot2)
library(magrittr)
data("mlb_standings")

mlb_standings$Wpct <- mlb_standings$W / (mlb_standings$W + mlb_standings$L)
mlb_standings$RD <- mlb_standings$R - mlb_standings$RA

mlb_standings %>%
  ggplot(aes(x = RD, y = Wpct)) + geom_point() +
    geom_smooth(method = "lm", se = F) +
    labs(x = "Run differential", y = "Winning percentage")

options(scipen = 999)
model <- lm(Wpct ~ RD, data = mlb_standings)
coefficients(model)
summary(model)

mlb_standings$PW_percentage <- mlb_standings$R^1.9 / (mlb_standings$R^1.9 + mlb_standings$RA^1.9)
errors <- mlb_standings$Wpct - mlb_standings$PW_percentage
sqrt(mean(errors^2))
sqrt(mean(model$residuals^2))

mlb_standings$PW <- mlb_standings$PW_percentage * (mlb_standings$W + mlb_standings$L)
mlb_standings$PW <- round(mlb_standings$PW,0)

mlb_standings %>%
  filter(Season == 2018) %>%
  select(Team, W, PW) %>%
  head()
