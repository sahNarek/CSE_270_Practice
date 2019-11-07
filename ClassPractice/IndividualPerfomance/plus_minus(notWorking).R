load("nba12.rda")
load("nba12p.rda")
load("nba5.rda")
library(ggplot2)
library(dplyr)
library(VIM)


nba3 <-nba12[, c("HomePlayer1ID","HomePlayer2ID",
                 "HomePlayer3ID","HomePlayer4ID","HomePlayer5ID")]
nba4 <-nba12[, c("AwayPlayer1ID","AwayPlayer2ID",
                 "AwayPlayer3ID","AwayPlayer4ID","AwayPlayer5ID")]




# Make ID as character
nba3 <- as.data.frame(sapply(nba3, as.character))
nba4 <- as.data.frame(sapply(nba4, as.character))
#create dummy variables
nba3 <- as.data.frame(model.matrix(~. - 1, nba3))
nba4 <- as.data.frame(model.matrix(~. - 1, nba4))
# for away team
nba4[nba4==1]<- -1
# change colnames to be more readable
colnames(nba3) <- gsub("HomePlayer[123456789]ID","", colnames(nba3))
colnames(nba4) <- gsub("AwayPlayer[123456789]ID","", colnames(nba4))
# nba3 remove double entries
nba3 <- t(nba3) # transpose the data.frame
nba3 <- by(nba3,INDICES=row.names(nba3),FUN=colSums) # collapse the rows with the same name
nba3 <- as.data.frame(do.call(cbind,aggr))
nba4 <- t(nba4) # transpose the data.frame
nba4 <- by(nba4,INDICES=row.names(nba4),FUN=colSums) # collapse the rows with the same name
nba4 <- as.data.frame(do.call(cbind,nba4))
#combine home and away players and double columns
nba5<-cbind(nba3,nba4)
nba5 <- t(nba5) # transpose the data.frame
nba5 <- by(nba5,INDICES=row.names(nba5),FUN=colSums) # collapse the rows with the same name
nba5 <- as.data.frame(do.call(cbind,nba5))
