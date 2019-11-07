library(SportsAnalytics270)
library(ggplot2)
library(magrittr)
library(dplyr)

data("nba_east")
data("nba_west")

nba_st <- rbind(nba_east, nba_west)
nba_st$PD <- nba_st$PS - nba_st$PA

nba_st %>% 
  ggplot(aes(x = PD, y = Pct)) + geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  labs(x = "Points differential", y = "Winning percentage")

options(scipen = 999)
model <- lm(Pct~PD, data = nba_st)
summary(model)

nba_st$Ratio <- nba_st$PS / nba_st$PA
model2 <- lm(log(W/L) ~ 0 + log(Ratio), 
             data = nba_st)
summary(model2)


nba_st$PW <- nba_st$PS^14.093 / (nba_st$PS^14.093 
                                 +nba_st$PA^14.093)
nba_st$G <- nba_st$W + nba_st$L
nba_st$PW_G <- round(nba_st$PW * nba_st$G)
nba_st %>% filter(Season==2019) %>%
  ggplot(aes(x=PW_G, y=W)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_text(aes(label = Team))

