library(SportsAnalytics270)
library(dplyr)
library(zoo)
library(reshape)
library(reshape2)
data(f_data_sm)

england <- f_data_sm %>%
  filter(COUNTRY == "England") %>%
  arrange(DATE) %>%
  select("SEASON","DATE","HOMETEAM",  "AWAYTEAM","FTHG", "FTAG")%>%
  mutate(ID = 1:n())


england_long <- reshape2::melt(england, 
                               id.vars = c("ID","HOMETEAM",  "AWAYTEAM",  "SEASON", "DATE"), 
                               measure.vars = c("FTHG", "FTAG"), variable.name="H_A")

england_long <- england_long[order(england_long$ID),]
rownames(england_long) <- NULL



england_long$TEAM1 <- ifelse(england_long$H_A =='FTAG', 
                             england_long$AWAYTEAM, england_long$HOMETEAM)

england_long$TEAM2 <- ifelse(england_long$H_A =='FTAG', 
                             england_long$HOMETEAM, england_long$AWAYTEAM)

england_long <- england_long[,c("ID","SEASON","DATE", "TEAM1","TEAM2", "H_A","value")]


library(dplyr)
library(zoo)
england_long1 <- england_long %>%
  group_by(SEASON, TEAM1) %>%
  mutate(CUM_SC = dplyr::lag(rollapply(value,6,sum, fill=NA, align='right'),1)) %>%
  ungroup() %>%
  group_by(SEASON, TEAM2) %>%
  mutate(CUM_C = dplyr::lag(rollapply(value,6,sum, fill=NA, align='right'),1)) %>%
  filter(CUM_SC != "NA")
england_long1 <- as.data.frame(england_long1)



england_wide <- reshape(england_long1, direction="wide",
                        timevar = "H_A", idvar = "ID", 
                        v.names=c("value", "CUM_SC", "CUM_C"))


england_wide$POWERHOME <- england_wide$CUM_SC.FTHG-england_wide$CUM_C.FTAG
england_wide$POWERAWAY <- england_wide$CUM_SC.FTAG-england_wide$CUM_C.FTHG
england_wide$POWERDIFF <- england_wide$POWERHOME-england_wide$POWERAWAY

england_wide$RESULT <- ifelse(england_wide$value.FTHG>england_wide$value.FTAG,"HW",
                              ifelse(england_wide$value.FTHG<england_wide$value.FTAG, "AW", "D"))

england_wide <- england_wide[,c("SEASON","DATE","TEAM1","TEAM2","value.FTHG",
                                "value.FTAG","POWERHOME", "POWERAWAY","POWERDIFF", "RESULT")]


england_wide <- england_wide[! is.na(england_wide$POWERDIFF),]
head(england_wide)


england_wide$RESULT <- ifelse(england_wide$value.FTHG>england_wide$value.FTAG,"HW",
                              ifelse(england_wide$value.FTHG<england_wide$value.FTAG, "AW", "D"))

england_wide <- england_wide[,c("SEASON","DATE","TEAM1","TEAM2","value.FTHG",
                                "value.FTAG","POWERHOME", "POWERAWAY","POWERDIFF", "RESULT")]


england_wide <- england_wide[! is.na(england_wide$POWERDIFF),]
head(england_wide)

freqs <- dcast(england_wide, POWERDIFF~RESULT)
head(freqs)
freqs <- data.frame(freqs, freqs[,2:4]/rowSums(freqs[,2:4]))
head(freqs)

freqs[freqs$POWERDIFF == 0,]

ggplot(freqs, aes(x=POWERDIFF, y=HW)) + geom_point() + 
  ggtitle("Home wins by Power Difference")

ggplot(freqs, aes(x=POWERDIFF, y = HW.1)) + geom_point() + 
  geom_smooth(method = "lm", se = F)

modelH <- lm(HW.1 ~ POWERDIFF, data = freqs)  
summary(modelH)

modelA <- lm(AW.1 ~ POWERDIFF, data = freqs)
summary(modelA)

modelD <- lm(D.1 ~ POWERDIFF, data = freqs)
summary(modelD)

modelD <- lm(D.1 ~ poly(POWERDIFF, 2), data = freqs)
summary(modelD)
