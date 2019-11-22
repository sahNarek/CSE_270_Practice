library(SportsAnalytics270)
library(igraph)
library(network)
library(intergraph)
library(circlize)

data("wc_14_final")

wc_14_final$germany_passing

i_ger <- igraph::graph.adjacency(as.matrix(wc_14_final$germany_passing),
                                 mode = "directed", weighted = T, diag = F)
net_ger <- intergraph::asNetwork(i_ger)

network::set.vertex.attribute(net_ger, "player.name", 
                              wc_14_final$germany_team$Player_name)
get.vertex.attribute(net_ger, "player.name")

network::set.vertex.attribute(net_ger, "player.position",
                              as.character(wc_14_final$germany_team$Position))
get.vertex.attribute(net_ger, "player.position")

network::set.vertex.attribute(net_ger, "passes.completed",
                              wc_14_final$germany_team$Pass_completed)

wc_14_final$germany_team$Completion <- wc_14_final$germany_team$Pass_completed / 
  wc_14_final$germany_team$Pass_attempted

network::set.vertex.attribute(net_ger, "completion",
                              wc_14_final$germany_team$Completion)


net_st <- get.inducedSubgraph(net_ger, v = 1:11)
names <- get.vertex.attribute(net_st, "player.name")

coords <- plot(net_st, suppress.axes = F)

ger_team <- wc_14_final$germany_team

ger_team[8,]

coords[1,] <- c(-6, 1.5)

coords[2,] <- c(-4, 3)
coords[8,] <- c(-4, 0)

coords[10,] <- c(-5, 2)
coords[3,] <- c(-5, 1)

coords[4,] <- c(-3, 2)
coords[11,] <- c(-3, 1)


coords[5,] <- c(-2, 3)
coords[7,] <- c(-2, 0)

coords[9,] <- c(-2, 1.5)
coords[6,] <- c(-1, 1.5)

plot(net_st, coord = coords, suppress.axes = F, displaylabels = T,
     label = net_st %v% "player.name")

plot(net_st, coord = coords, edge.lwd ="weight",
     vertex.col = "player.position")

plot(net_st, coord = coords, vertex.col = "player.position",
     vertex.cex = 2*(net_st %v% "completion"),
     label = net_st %v% "player.name")

st_mat <- as.matrix(net_st, matrix.type = "adjacency", attrname = "weight")
colnames(st_mat) <- net_st %v% "player.name"
rownames(st_mat) <- colnames(st_mat)

chordDiagram(st_mat, directional = T)
