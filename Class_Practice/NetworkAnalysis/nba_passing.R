library(SportsAnalytics270)
library(igraph)
library(network)
library(intergraph)

load("passing.rda")
load("gsw.rda")

x <- gsw$PLAYER_ID

passing <- passing[passing$PASS_TYPE == "made",]
passing <- passing[passing$PASS_TEAMMATE_PLAYER_ID %in% x,]

i_pass <- graph_from_edgelist(
  as.matrix(passing[,c("PLAYER_NAME","PASS_TEAMMATE_PLAYER_NAME")], 
            directed = T))

plot(i_pass)

passing <- passing[passing$PLAYER_NAME != passing$PASS_TEAMMATE_PLAYER_NAME,]

i_pass <-  graph_from_edgelist(
  as.matrix(passing[,c("PLAYER_NAME","PASS_TEAMMATE_PLAYER_NAME")], 
            directed = T))

plot(i_pass)

n_pass <- intergraph::asNetwork(i_pass)
plot(n_pass, displaylabels = T)

n_pass %v% "vertex.names"

p <- n_pass %v% "vertex.names"
p

gsw <- gsw[order(match(gsw$PLAYER, p)),]
gsw$PLAYER

network::set.vertex.attribute(n_pass, "position", gsw$POSITION)
n_pass %v% "position"

ngames <- unique(passing[,c("PLAYER_NAME", "G")])
ngames <- ngames[order(match(ngames$PLAYER_NAME,p)),]
ngames

network::set.vertex.attribute(n_pass, "ngames", ngames$G)
n_pass %v% "ngames"
