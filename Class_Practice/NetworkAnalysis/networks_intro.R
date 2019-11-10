library(network)
library(sna)
library(igraph)
library(intergraph)

mat <- rbind(c(0,1,1,0,0),c(0,0,1,1,0),c(0,1,0,0,0),
             c(0,0,0,0,0),c(0,0,1,0,0))
rownames(mat) <- c("A","B","C","D","E")
colnames(mat) <- c("A","B","C","D","E")
mat
net <- network(mat, matrix.type = "adjacency")
summary(net)
plot(net, main="Network example")
plot(net, main="Network example", label = network.vertex.names(net))
plot(net, main="Network example", label = network.vertex.names(net), 
     vertex.cex=2.5, label.cex = 1.5, edge.lwd = 10)

mat1 <- cbind(FROM = c("A","A","B","B","C","E"),
              TO = c("B","C","C","D","B","C"))

mat1

net1 <- network(mat1, matrix.type = "edgelist")

plot(net1, main="Network example 2",
     label = network.vertex.names(net1), 
     vertex.cex = 2.5, label.cex = 1.5, edge.lwd = 10)

net_ed <- as.matrix(net, matrix.type = "edgelist")

net3 <- igraph::graph_from_edgelist(net_ed)
plot(net3)
net4 <- igraph::graph_from_adjacency_matrix(mat)
plot(net4)


sna::gplot(net1, displaylabels = T)


net_i <- intergraph::asIgraph(net1)
class(net_i)
#vice_versa
net_n <- intergraph::asNetwork(net_i)
class(net_n)


network::set.vertex.attribute(net1, "gender", c("F","F","M","F","M"))
summary(net1)

network::get.vertex.attribute(net1, "gender")
net1 %v% "gender"


plot(net1, label = network.vertex.names(net1), vertex.cex = 2.5, label.cex = 1.5,
     main = "Sample graph with attr",
     vertex.col = "gender")

network::set.edge.attribute(net1, "edge.size", c(3,0,5,2,6))
# Check if added
network::get.edge.attribute(net1,"edge.size")
net1 %e% "edge.size"

network::list.edge.attributes(net1)
network::list.vertex.attributes(net1)

plot(net1, label = network.vertex.names(net1), vertex.cex = 2.5, label.cex = 1.5,
     main = "Sample graph with vet and edge attr",
     vertex.col = "gender", edge.label = "edge.size")

set.seed(1)
plot(net1, label = network.vertex.names(net1))

my_coord <- plot(net1, label = network.vertex.names(net1))
plot(net1, label = network.vertex.names(net1), coord = my_coord)

plot(net1, label = network.vertex.names(net1), 
     mode = "circle", main = "Circle Layout")

plot(net1, label = network.vertex.names(net1), 
     mode = "fruchtermanreingold", main = "Fruch Layout")

plot(net1, label = network.vertex.names(net1), 
     mode = "kamadakawai", main = "Kamada Layout")

net5 <- intergraph::asIgraph(net1)
coords <- layout_in_circle(net5)
coords

plot(net5, layout = coords, main = "circle layout")

plot(net5, layout = layout_with_fr(net5), main = "FR layout")

plot(net5, layout = layout_on_grid(net5), main = "Grid layout")

plot(net5, layout = layout_nicely(net5), main = "Nice layout")

set.seed(1)
coord1 <- plot(net1, displaylabels = T, mode = "circle",
               main = "circle layout", suppress.axes = F)

coord1

coord1[3,] <- 0
plot(net1, displaylabels = T, mode = "circle", 
     main = "Circle Layout", coord = coord1)

net1 %v% "gender"

net_f <- get.inducedSubgraph(net1, v = which(net1 %v% "gender" == "F"))
net_m <- get.inducedSubgraph(net1, v = which(net1 %v% "gender" == "M"))
plot(net_f, label = network.vertex.names(net_f))
plot(net_m, label = network.vertex.names(net_m))


net_e <- get.inducedSubgraph(net1, eid = which(net1 %e% "edge.size" >= 3 ))
plot(net_e, label = network.vertex.names(net_e))


class(net4)

net4 <- set_vertex_attr(net4, "gender", value = c("F", "F", "M", "F", "M"))
V(net4)$gender[1] <- "M"
V(net4)$name

net4 <- set_edge_attr(net4, "edge.val", value = c(5,0,3,1,0,3))
E(net4)$edge.val
