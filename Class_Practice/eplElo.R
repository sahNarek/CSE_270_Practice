library("PlayerRatings")
library("elo")
library("EloRating")
library(SportsAnalytics270)
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

epl3 <- elo.run(score(HG, AG) ~ adjust(Home, 200) + Away + k(20*epl_00_18$G), data = epl_00_18)
sort(final.elos(epl3), decreasing = T)
