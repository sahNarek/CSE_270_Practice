library(SportsAnalytics270)
library(igraph)
library(network)
library(intergraph)
library(ggplot2)
library(circlize)

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

network::set.edge.attribute(n_pass, "passes", passing$PASS)
n_pass %e% "passes"

ggplot(passing, aes(x = FGM)) + geom_histogram() +
  labs(x = "Field Goals", title = "FG")

ggplot(passing, aes(x = FG_PCT)) + geom_histogram() +
  labs(x = "Field Goal Percentage", title = "FGP")

network::set.edge.attribute(n_pass, "FGP", passing$FG_PCT)
n_pass %e% "FGP"

ggplot(passing, aes(x = PASS)) + geom_histogram() + 
  labs(x = "Passes", title = "Distribution of passes")

n_pass1 <- get.inducedSubgraph(n_pass,
                               eid = which(n_pass %e% "passes" > 30))
plot(n_pass, displaylabels = T, mode = "circle")

n_pass_mat <- as.matrix(n_pass, matrix.type = "adjacency", 
                        attrname = "passes")

chordDiagram(n_pass_mat)

x <- n_pass1 %v% "ngames"
z <- 2*(x - min(x)) / (max(x) - min(x))

plot(n_pass1, displaylabels = T, mode = "circle",
     vertex.cex = z, vertex.col = "position",
     label = paste(n_pass1 %v% "vertex.names", n_pass1 %v% "position", sep = "-"))

lineup <- c("Kevin Durant", "Zaza Pachulia",
            "Draymond Green", "Stephen Curry",
            "Klay Thompson")
n_pass2 <- get.inducedSubgraph(n_pass1, 
                               v = which(n_pass1 %v% "vertex.names" %in% lineup))

plot(n_pass2, displaylabels = T, mode = "circle",
     vertex.cex = z, vertex.col = "position", edge.curve = 0.025, usecurve = T,
     label = paste(n_pass2 %v% "vertex.names", n_pass2 %v% "position", sep = "-"))


coords <- plot(n_pass2, displaylabels = T, mode = "circle",
               vertex.cex = z, vertex.col = "position", edge.curve = 0.025, usecurve = T,
               label = paste(n_pass2 %v% "vertex.names", n_pass2 %v% "position", sep = "-"))
coords

coords[1,] <- c(-2, -3)
coords[2,] <- c(-2, -4.2)
coords[3,] <- c(-3.5, -4.5)
coords[4,] <- c(-3.6, -3)
coords[5,] <- c(-2.7, -3.2)

plot(n_pass2, displaylabels = T, mode = "circle",
     vertex.cex = z, vertex.col = "position", edge.curve = 0.025, usecurve = T,
     label = paste(n_pass2 %v% "vertex.names", n_pass2 %v% "position", sep = "-"),
     coord = coords)

x <- n_pass2 %e% "passes"
z <- 10*(x - min(x)) / (max(x) - min(x))

plot(n_pass2, displaylabels=T, coord = coords, 
     usecurve = T, edge.curve = 0.015,
     edge.lwd = z)

plot(n_pass2, displaylabels=T, coord = coords, 
     usecurve = T, edge.curve = 0.015,
     edge.lwd = z, edge.label = n_pass2 %e% "FGP")

i_pass2 <- intergraph::asIgraph(n_pass2)
plot(i_pass2, vertex.label = V(i_pass2)$vertex.names, 
     layout = coords)

x <- E(i_pass2)$passes
z <- 10*(x - min(x)) / (max(x) - min(x))

plot(i_pass2, vertex.label = V(i_pass2)$vertex.names, 
     layout = coords, edge.width = z)

