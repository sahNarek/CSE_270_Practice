library(SportsAnalytics270)
library(dplyr)
library(ggplot2)
data("seriea_st")
data("nfl_st")

seriea_s <- seriea_st %>%
  group_by(Season) %>%
  summarise(SD = sd(Pt))


ggplot(seriea_s, aes(x = reorder(factor(Season),SD))) +
  geom_bar(aes(y = SD),stat = "identity") + 
  ggtitle("SD in Seria A") + 
  coord_flip(ylim = c(0,20))

nfl_s <- nfl_st %>%
  group_by(Season) %>%
  summarise(SD = sd(Wpct))

ggplot(nfl_s, aes(x = reorder(factor(Season),SD))) +
  geom_bar(aes(y = SD),
           stat = "identity") + 
  ggtitle("SD in NFL") + 
  coord_flip(ylim = c(0,0.21))

id_s <- 0.5/sqrt(16)
sd_wpct <- sd(nfl_st$Wpct)

sd_wpct / id_s

nfl_st %>%
  group_by(Season) %>%
  summarise(NS = sd(Wpct) / id_s) %>%
  ggplot(aes(x = Season, y = NS)) + 
  geom_bar(stat = "identity")

