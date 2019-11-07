load("nba12.rda")
load("nba12p.rda")
load("nba5.rda")
library(ggplot2)
library(dplyr)

oks_g <- grepl("OKC", nba12$GameID, fixed = T)
okc <- nba12[oks_g == T,]

head(okc[,19:23], n = 5)

okc$line_h <-
  paste(okc$HomePlayer1Name, okc$HomePlayer2Name, 
        okc$HomePlayer3Name, okc$HomePlayer4Name , 
        okc$HomePlayer5Name, sep = ',')
okc$line_a <-
  paste(okc$AwayPlayer1Name, okc$AwayPlayer2Name, 
        okc$AwayPlayer3Name, okc$AwayPlayer4Name , 
        okc$AwayPlayer5Name, sep = ',')

okc$Home_PM <- okc$PointsScoredHome - okc$PointsScoredAway
okc$Away_PM <- -okc$Home_PM

okc$D_H <- grepl("Durant", okc$line_h, fixed = T)
okc$D_A <- grepl("Durant", okc$line_a, fixed = T)

sum(okc$Home_PM[okc$D_H == T]) + sum(okc$Away_PM[okc$D_A == T])

durant_pm <- okc %>%
  mutate(PM = ifelse(D_H == T,Home_PM,
                     ifelse(D_A==T,Away_PM,0)))%>%
  group_by(GameID) %>%
  summarise(PM = sum(PM))

head(durant_pm, n = 5)

ggplot(durant_pm, aes(x = GameID, y = PM)) + 
  geom_bar(stat = "identity") + 
  ggtitle("P/M for Kevin Durant") 

okc <- okc %>%
  mutate(OKC_PM = ifelse(AwayT == "OKC", Away_PM,
                         ifelse(HomeT == "OKC", Home_PM, 0)),
         D_PM = ifelse(D_H == T | D_A == T, T, F))

durrant_pm <- okc%>%
  group_by(GameID, D_PM)%>%
  summarise(PM = sum(OKC_PM))

head(durant_pm)

durrant_pm <- okc %>%
  group_by(GameID, D_PM)%>%
  summarise(PM = sum(OKC_PM)) %>%
  group_by(GameID) %>%
  mutate(NET = PM - lag(PM, 1)) %>%
  filter(NET != "NA") %>%
  select(GameID, NET) 

head(durant_pm)


