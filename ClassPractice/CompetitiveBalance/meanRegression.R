library(SportsAnalytics270)
library(dplyr)
data("nba_misc")
data("nba_east")
data("nba_west")
data("nfl_st")

nba_misc$WR <- nba_misc$W/82
var(nba_misc$WR)

nba_st <- rbind(nba_east, nba_west)

nba_st$PW <- nba_st$PS ^ 14.105 / 
  (nba_st$PS ^ 14.105 + nba_st$PA ^ 14.105)
nba_st$G <- nba_st$W + nba_st$L
nba_st$PW_G <- round(nba_st$PW * nba_st$G)

nba_st$DF <- nba_st$PW_G - nba_st$W

nba_st %>% filter(Team == "Sacramento Kings") %>%
  ggplot(aes(x = Season, y = DF)) +
  geom_point()+
  geom_line()+
  ggtitle("Wins difference for SK")

unlucky <- nba_st %>% 
  filter(DF == max(DF))

lucky <- nba_st %>%
  filter(DF == min(DF))


means <- nba_st %>%
  group_by(Team) %>%
  summarise(mean = mean(DF)) %>%
  arrange(desc(mean))


unlucky_teams <- nba_st %>%
  filter(DF > 0)

lucky_teams <- nba_st %>%
  filter(DF < 0)

deserved_teams <- nba_st %>%
  filter(DF == 0)

ggplot(means, aes(x = mean)) +
  geom_histogram()




