library(SportsAnalytics270)
library(dplyr)
data("seriea_st")

seriea_s <- seriea_st %>%
  group_by(Season) %>%
  summarise(SD = sd(Pt))


ggplot(seriea_s, aes(x = reorder(factor(Season),SD))) +
  geom_bar(aes(y = SD),
           stat = "identity") + 
  ggtitle("SD in Seria A") + 
  coord_cartesian(ylim = c(10,22)) +
  coord_flip()
