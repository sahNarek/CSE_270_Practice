library(SportsAnalytics270)
library(dplyr)
library(odds.converter)
library(measures)
library(ggplot2)

data("f_data_sm")

f_2009 <- f_data_sm %>%
  filter(SEASON > 2008,
         COUNTRY %in% c("Italy","Germany","Spain",
                        "England","Netherlands","Portugal"),
         !is.na(H) ) 
head(f_2009)

x <- odds.fv(c(1.4,3.8,8), input = "dec", output = "prob")

f_2009[,c("H","D","A")] <- odds.fv(f_2009[,c("H","D","A")],
                                   input = "dec", output = "prob")

brier_score <- f_2009 %>%
  group_by(COUNTRY) %>%
  do(SCORE = multiclass.Brier(.[,c("H","D","A")], .$FTR))
brier_score <- as.data.frame(brier_score)
brier_score$SCORE <- unlist(brier_score$SCORE)

ggplot(brier_score, aes(x = COUNTRY, y = SCORE)) +
  geom_bar(stat = "identity") + 
  coord_cartesian(ylim = c(0.5, 0.6)) + 
  ggtitle("Match uncertainity in each Country")

brier_score_s <- f_2009 %>%
  group_by(COUNTRY,SEASON) %>%
  do(SCORE = multiclass.Brier(.[,c("H","D","A")], .$FTR))
brier_score_s <- as.data.frame(brier_score_s)
brier_score_s$SCORE <- unlist(brier_score_s$SCORE)

ggplot(brier_score, aes(x=COUNTRY, y = SCORE)) +
  geom_bar(stat = "identity") + 
  coord_cartesian(ylim = c(0.5, 0.6)) + 
  ggtitle("Match Uncertainity by country")


brier_score_s %>%
  filter(COUNTRY == "Spain") %>%
  ggplot(aes(x = SEASON, y = SCORE)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = 2009:2019)

brier_score_s %>%
  ggplot(aes(x = SEASON, y = SCORE)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  facet_grid(~COUNTRY) + 
  scale_x_continuous(breaks = 2009:2019) + 
  coord_flip()
