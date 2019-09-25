library(PlayerRatings)
library(elo)
library(EloRating)
library(measures)
library(SportsAnalytics270)
library(magrittr)
library(dplyr)
data("epl_00_18")
epl_00_18

elos <- elo.run(score(HG, AG) ~ Home + Away, 
                data = epl_00_18, k = 20)
elos_df <- as.data.frame(elos)

elo_final <- final.elos(elos)
sort(elo_final, decreasing = T)

epl2 <- elo.run(score(HG, AG) ~ adjust(Home,200) + Away, 
                data = epl_00_18, 
                k = 20)
sort(final.elos(epl2), decreasing = T)

difference <- abs(epl_00_18$HG - epl_00_18$AG)

epl_00_18$G <- ifelse(difference<=1,1,
                      ifelse(difference==2,1.5,
                      (difference+11)/8))

epl3 <- elo.run(score(HG, AG) ~ adjust(Home, 200) + 
                  Away + 
                  k(20*epl_00_18$G), 
                data = epl_00_18)
sort(final.elos(epl3), decreasing = T)


epl2 <- elo.run(score(HG, AG) ~ adjust(Home, 50) + Away, data = epl_00_18, k = 20)
epl2 <- as.data.frame(epl2)

epl2 <- data.frame(epl2, epl_00_18[,c("Round","Season")])
head(epl2)

elo.prob(elo.A = 1500 + 50, elo.B = 1500)

epl2 %>%
  group_by(Season) %>%
  summarise(Brier = Brier(p.A, wins.A, positive = 1)) %>%
  arrange(desc(Brier))


epl2$Predicted <- ifelse(epl2$p.A >= 0.55, 1,
                         ifelse(epl2$p.A <= 0.45,0,0.5))

confusion <- table(epl2$wins.A, epl2$Predicted)
confusion

sum(diag(confusion))/nrow(epl2)

table(epl2$wins.A)/nrow(epl2)



