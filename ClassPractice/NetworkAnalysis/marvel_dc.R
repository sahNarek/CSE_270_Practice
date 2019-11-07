library(igraph)
library(network)
library(circlize)

load("marvel_roles.rda")
marvel <- read.csv("marvel.csv")
movies <- read.csv("marvel_movies.csv", stringsAsFactors = F)

head(marvel)

m_igraph <- igraph::graph_from_edgelist(as.matrix(marvel[,1:2]), directed = F)
m_igraph <- set_edge_attr(m_igraph, "characters", value = marvel[,3])

class(m_igraph)

plot(m_igraph, layout = layout_in_circle(m_igraph))

m_net <- intergraph::asNetwork(m_igraph)
plot(m_net)

plot(m_net, mode = "circle", displaylabels = T, label.pos = 6)

m_net %v% "vertex.names"

identical(movies$Movie.title, m_net %v% "vertex.names")


network::set.vertex.attribute(m_net, attrname = "BoxOffice", movies$World_gross)
network::get.vertex.attribute(m_net, "BoxOffice")

m_igraph <- set_vertex_attr(m_igraph, name = "BoxOffice", value = movies$World_gross)
V(m_igraph)$BoxOffice

get.vertex.attribute(m_net, "BoxOffice")

plot(m_net, mode = "circle", displaylabels = T,
     label.pos = 6, edge.label = "characters")

plot(m_net, mode = "circle", displaylabels = T,
     label.pos = 6, edge.lwd = "characters", label.cex = 1.1)

plot(m_net, mode = "circle", displaylabels = T,
     label.pos = 6, edge.lwd = "characters", edge.col = "characters")

plot(m_net, mode = "circle", displaylabels = T,
     label.pos = 6, edge.lwd = "characters",
     vertex.cex = m_net %v% "BoxOffice")

x <- m_net %v% "BoxOffice"
z <- (x - mean(x))/sd(x)

plot(m_net, mode = "circle", displaylabels = T,
     label.pos = 6, edge.lwd = "characters",
     vertex.cex = z)

as_m <- as.matrix(m_net, matrix.type = "adjacency", attrname = "characters")
as_m[upper.tri(as_m)] <- 0


as_m[as_m == 1] <- 0
chordDiagram(as_m)


m_net1 <- get.inducedSubgraph(m_net,
                              eid = which(m_net %e% "characters" >= 2 ))

plot(m_net1, mode = "circle", displaylabels = T,
     label.pos = 6, edge.lwd = "characters")

m_net2 <- get.inducedSubgraph(m_net,
                             v = which(m_net %v% "BoxOffice" >= 600))

plot(m_net2, mode = "circle", displaylabels = T,
     label.pos = 6, edge.lwd = "characters")

list.edge.attributes(marvel_roles)

plot(marvel_roles, main = "Marvel characters network")

marvel_roles1 <- get.inducedSubgraph(marvel_roles, 
                                     eid = which(marvel_roles %e% "weight" > 1))

plot(marvel_roles1, main = "Marvel characters network")

plot(marvel_roles1, main = "Marvel characters network", edge.col = "weight")
legend("topleft", fill = unique(marvel_roles1 %e% "weight"),
       legend = unique(marvel_roles1 %e% "weight"))

marvel_roles2 <- get.inducedSubgraph(marvel_roles, 
                                     eid = which(marvel_roles %e% "weight" == 3))

plot(marvel_roles2, mode = "circle", displaylabels = T,
     main = "Marvel characters network")
