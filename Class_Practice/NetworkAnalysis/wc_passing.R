library(SportsAnalytics270)
library(igraph)
library(network)
library(intergraph)
library(circlize)

data("wc_14_final")

wc_14_final$argentina_passing

i_arg <- igraph::graph.adjacency(as.matrix(wc_14_final$argentina_passing),
                                 mode = "directed", weighted = T, diag = F)
net_arg <- intergraph::asNetwork(i_arg)

network::set.vertex.attribute(net_arg, "player.name", 
                              wc_14_final$argentina_team$Player_name)
get.vertex.attribute(net_arg, "player.name")

network::set.vertex.attribute(net_arg, "player.position",
                              as.character(wc_14_final$argentina_team$Position))
get.vertex.attribute(net_arg, "player.position")

network::set.vertex.attribute(net_arg, "passes.completed",
                              wc_14_final$argentina_team$Pass_completed)

wc_14_final$argentina_team$Completion <- wc_14_final$argentina_team$Pass_completed / 
  wc_14_final$argentina_team$Pass_attempted

network::set.vertex.attribute(net_arg, "completion",
                              wc_14_final$argentina_team$Completion)


net_st <- get.inducedSubgraph(net_arg, v = 1:11)
names <- get.vertex.attribute(net_st, "player.name")

coords <- plot(net_st, suppress.axes = F)

arg_team <- wc_14_final$argentina_team

coords[1,] <- c(-6, 1.5)
coords[2,] <- c(-5, 2)
coords[9,] <- c(-5, 1)
coords[10,] <- c(-4, 3)
coords[3,] <- c(-4, 0)
coords[4,] <- c(-3, 2)
coords[8,] <- c(-3, 1)
coords[11,] <- c(-2, 3)
coords[5,] <- c(-2, 0)
coords[7,] <- c(-1.2, 1)
coords[6,] <- c(-1, 2)
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
