library(SportsAnalytics270)
library(dplyr)
library(ggplot2)
data("seriea_st")
data("nfl_st")
data("f_data_sm")


final_tables <- function(data = f_data_sm,country){
  result <- c()
  for(season in unique(data$SEASON)){
    season_table <- final_table(data, country, season)
    league <- data[data$SEASON == season & 
                     data$COUNTRY == country,]$LEAGUE[1]
    season_table$SEASON = season
    season_table$COUNTRY = country
    season_table$LEAGUE = league
    result <- rbind(result, season_table)
  }
  cols <- colnames(result)
  len <- length(cols)
  result <- result[c("SEASON","COUNTRY","LEAGUE",cols[1 : (len-3)])]
  return(as.data.frame(result))
}

bundes <- final_tables(country = "Germany")

england_st <- final_tables(country = "England")

top_5_bund <- bundes %>%
  group_by(SEASON) %>%
  filter(POSITION <= 5) %>%
  summarize(TopP = sum(POINTS))

all_bund <- bundes %>%
  group_by(SEASON) %>%
  summarize(P = sum(POINTS))

C5_bund <- data.frame(Season = top_5_bund$SEASON, C5 = top_5_bund $TopP / all_bund$P)

top_6_eng <- england_st %>%
  group_by(SEASON) %>%
  filter(POSITION <= 6) %>%
  summarize(TopP = sum(POINTS))

all_eng <- england_st %>%
  group_by(SEASON) %>%
  summarize(P = sum(POINTS))

C5_eng <- data.frame(Season = all_eng$SEASON, C5 = top_6_eng$TopP / all_eng$P)

C5_eng %>%
  ggplot(aes(factor(Season), C5)) +
  geom_bar(stat = "identity") + 
  coord_flip()

C5_eng %>%
  ggplot(aes(factor(Season), 1 - C5)) +
  geom_bar(stat = "identity") + 
  coord_flip()


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

top_5 <- seriea_st %>%
  group_by(Season) %>%
  filter(Rank < 6) %>%
  summarize(TopP = sum(Pt))

all <- seriea_st %>%
  group_by(Season) %>%
  summarize(Pt = sum(Pt))

C5 <- data.frame(Season = top_5$Season, C5 = top_5$TopP / all$Pt)

C5 %>%
  ggplot(aes(Season, C5)) +
  geom_bar(stat = "identity")

C5 %>%
  ggplot(aes(Season, 1 - C5)) +
  geom_bar(stat = "identity")

mean(C5$C5)
mean(C5_eng$C5)

champ_c <- seriea_st %>%
  filter(Rank == 1) %>%
  group_by(Team) %>%
  summarize(champ = n()) %>%
  arrange(desc(champ))

champ_e <- england_st %>%
  filter(POSITION == 1) %>%
  group_by(TEAM) %>%
  summarize(champ = n()) %>%
  arrange(desc(champ))

100 * (sum(champ_e[1:4,2]) / sum(champ_e$champ))


HHI <- seriea_st %>%
  group_by(Season) %>%
  mutate(Perc = Pt / sum(Pt)) %>%
  summarise(HHI = sum(Perc ^ 2))

HHI_eng <- england_st %>%
  group_by(SEASON) %>%
  mutate(Perc = POINTS / sum(POINTS)) %>%
  summarise(HHI = sum(Perc ^ 2))


HHI_eng %>%
  ggplot(aes(SEASON, HHI)) +
  geom_bar(stat = "identity")
