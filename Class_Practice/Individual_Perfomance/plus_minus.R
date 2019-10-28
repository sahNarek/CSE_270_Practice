load("nba5.rda")
load("nba12.rda")
load("nba12p.rda")
library(dplyr)
library(ggplot2)

nba5 <- data.frame(PM = nba12$PointsScoredHome - nba12$PointsScoredAway, nba5)
model <- lm(PM ~ ., data = nba5)
coeff <- sort(coefficients(model), decreasing = T)
coeff[1:10]

name1 <- names(coeff)
name1 <- gsub("X", "", name1)
name1 <- as.data.frame(name1)

nba12p$PlayerID <- as.character(nba12p$PlayerID)
name2 <- left_join(name1, nba12p[,1:2], by = c("name1" = "PlayerID"))

names(coef) <- name2$PlayerTrueName
coeff[1:10]

ggplot(data = nba12p, aes(SimpleMin)) + 
  geom_histogram() + 
  ggtitle("Histogram for Played Minutes")

more250 <- nba12p$PlayerID[nba12p$SimpleMin > 250]
length(more250)

colnames(nba5) <- gsub("X","",colnames(nba5)) 
nba6 <- nba5[,colnames(nba5) %in% more250]
nba6 <- data.frame(PM = nba5$PM, nba6)

model2 <- lm(PM ~ .,data = nba6)
coeff2 <- sort(coefficients(model2), decreasing = T)
coeff2[1:10]

name3 <- names(coeff2)
name3 <- gsub("X", "", name3)
name3 <- as.data.frame(name3)
name4 <- left_join(name3, nba12p[,1:2], by = c("name3" = "PlayerID"))

names(coeff2) <- name4$PlayerTrueName
coeff2[1:10]

