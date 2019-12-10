library(SportsAnalytics270)
library(dplyr)
library(zoo)
library(reshape)
library(reshape2)
library(ggplot2)
data(f_data_sm)

pick_country <- function(source = f_data_sm, country) {
  country <- source %>%
    filter(COUNTRY == country) %>%
    arrange(DATE) %>%
    select("SEASON","DATE","HOMETEAM",  "AWAYTEAM","FTHG", "FTAG")%>%
    mutate(ID = 1:n())
  
  country_long <- reshape2::melt(country, 
                                 id.vars = c("ID","HOMETEAM",  "AWAYTEAM",  "SEASON", "DATE"), 
                                 measure.vars = c("FTHG", "FTAG"), variable.name="H_A")
  country_long <- country_long %>%
    arrange(ID)
  return(list(wide = country,
              long = country_long))
}

prepare_long <- function(data){
  modified <- data %>%
    mutate(TEAM1 = ifelse(H_A == "FTHG", HOMETEAM, AWAYTEAM),
           TEAM2 = ifelse(TEAM1 == HOMETEAM, AWAYTEAM, HOMETEAM)) %>%
    select(c("ID","SEASON","DATE", "TEAM1","TEAM2", "H_A","value"))
  return(modified)
}

# n = number of games, before the match
goals_scored_conceded <- function(data, n) {
  data_long <- england_long %>%
    group_by(SEASON, TEAM1) %>%
    mutate(CUM_SC = lag(rollapply(value,n,sum, fill=NA, align='right'),1)) %>%
    ungroup() %>%
    group_by(SEASON, TEAM2) %>%
    mutate(CUM_C = lag(rollapply(value,n,sum, fill=NA, align='right'),1)) %>%
    filter(CUM_SC != "NA")
  data_long <- as.data.frame(data_long)
  
  data_wide <- reshape(data_long, direction="wide",
                          timevar = "H_A", idvar = "ID", 
                          v.names=c("value", "CUM_SC", "CUM_C"))
  return(list(long = data_long,
              wide = data_wide))
}

get_power_diff <- function(data){
  data$POWERHOME <- data$CUM_SC.FTHG-data$CUM_C.FTAG
  data$POWERAWAY <- data$CUM_SC.FTAG-data$CUM_C.FTHG
  data$POWERDIFF <- data$POWERHOME-data$POWERAWAY
  result <- data %>%
    mutate(RESULT = ifelse(value.FTHG > value.FTAG, "HW", 
                           ifelse(value.FTHG < value.FTAG, "AW", "D" ) )) %>%
    select(c("SEASON","DATE","TEAM1","TEAM2","value.FTHG",
             "value.FTAG","POWERHOME", "POWERAWAY","POWERDIFF", "RESULT"))
  result <- result[!is.na(result$POWERDIFF),]
  return(result)
}

build_models <- function(freqs) {
  modelH <- lm(HW.1 ~ POWERDIFF, data = freqs)  
  modelA <- lm(AW.1 ~ POWERDIFF, data = freqs)
  modelD <- lm(D.1 ~ POWERDIFF, data = freqs)
  modelD_2 <- lm(D.1 ~ poly(POWERDIFF, 2), data = freqs)
  
  return(list(H = modelH,
              A = modelA,
              D = modelD,
              D_2 = modelD_2))
}


england <- pick_country(country = "England")$wide
england_long <- pick_country(country = "England")$long
england_long <- prepare_long(data = england_long)
england_long_1 <- goals_scored_conceded(data = england_long_2, n = 6)$long
england_wide <- goals_scored_conceded(data = england_long, n = 6)$wide
england_wide <- get_power_diff(data = england_wide)

freqs <- dcast(england_wide, POWERDIFF~RESULT)
head(freqs)
freqs <- data.frame(freqs, freqs[,2:4]/rowSums(freqs[,2:4]))
head(freqs)
freqs[freqs$POWERDIFF == 0,]

ggplot(freqs, aes(x=POWERDIFF, y=HW)) + geom_point() + 
  ggtitle("Home wins by Power Difference")

ggplot(freqs, aes(x=POWERDIFF, y = HW.1)) + geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  labs(x = "Power Difference", y = "Home Win %", title = "POWERDIFF vs HW%")

modelH <- build_models(freqs = freqs)$H
summary(modelH)
