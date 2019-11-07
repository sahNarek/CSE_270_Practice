load("nba5.rda")
load("nba12.rda")
load("nba12p.rda")
library(dplyr)
library(ggplot2)

nba5 <- data.frame(PM = nba12$PointsScoredHome - nba12$PointsScoredAway, nba5)
nba12p$PlayerID <- as.character(nba12p$PlayerID)

player_names <- function(games_data = nba5, players_data = nba12p){
  model <- lm(PM ~ ., data = games_data)
  coeff <- sort(coefficients(model), decreasing = T)
  names <- names(coeff)
  names <- gsub("X","",names)
  names <- as.data.frame(names)
  real_names <- left_join(names, players_data[,1:2], by = c("names" = "PlayerID"))
  names(coeff) <- real_names$PlayerTrueName
  return(list(
    model = model,
    coeff = coeff
  ))
}

coeff <- player_names()$coeff
coeff[1:10]

ggplot(data = nba12p, aes(SimpleMin)) +
  geom_histogram() +
  ggtitle("Histogram for played minutes")

more250 <- nba12p$PlayerID[nba12p$SimpleMin > 250]
length(more250)

colnames(nba5) <- gsub("X","",colnames(nba5))
nba6 <- nba5[,colnames(nba5) %in% more250]
nba6 <- data.frame(PM = nba5$PM, nba6)

coeff2 <- player_names(games_data = nba6)$coeff
coeff2[1:10]

nba12$PM_Home <- nba12$PointsScoredHome / nba12$PossessionsHome
nba12$PM_Away <- nba12$PointsScoredAway / nba12$PossessionsAway
nba12$Margin <- nba12$PM_Home - nba12$PM_Away

nba6$PM <- nba12$Margin

nba7 <- nba6[is.finite(nba6$PM),]

coeff3 <- player_names(games_data = nba7)$coeff
coeff3[1:10]

coeff3 <- coeff3 - mean(coeff3)

ggplot() + aes(coeff3) + geom_histogram() +
  ggtitle("Hist for scaled coeff")

colnames(nba7)

model <- lm(PM ~., data = nba7, weights = Secs)
