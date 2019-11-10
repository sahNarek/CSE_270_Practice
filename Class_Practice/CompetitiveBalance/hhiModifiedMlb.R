library(SportsAnalytics270)
library(dplyr)
data("mlb_standings")
data("mlb_salary")

mlb_HHI <- mlb_standings %>%
  group_by(Season) %>%
  summarise(HHI = sum(W^2) * (4/(30^2*162^2)))

mlb_HHI %>%
  ggplot(aes(Season, HHI)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 1/30, col = "red", size = 1.5) +
  coord_cartesian(ylim = c(0.03, 0.035))
str(mlb_salary)

mlb_s <- mlb_salary %>%
  group_by(Season) %>%
  summarise(Tot.salary = sum(Current),
            SD = sd(Current))

cor(mlb_s$SD, mlb_HHI$HHI)

mlb_HHI_s <- mlb_salary %>%
  group_by(Season) %>%
  mutate(Perc = Current / sum(Current)) %>%
  summarise(HHI_S = sum(Perc^2))

cor(mlb_HHI_s$HHI_S, mlb_HHI$HHI)
