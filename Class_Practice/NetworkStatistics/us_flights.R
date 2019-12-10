load("usa.rda")
load("usa_i.rda")

library(UserNetR)
library(network)
library(sna)
library(igraph)
library(intergraph)
library(ggplot2)


network::list.vertex.attributes(usa)
network::list.edge.attributes(usa)

plot(usa)

get_diameter(usa_i)
edge_density(usa_i)

edge_density(usa_i)

mean_distance(usa_i, directed = T)


list.vertex.attributes(usa_i)
table(V(usa_i)$type)

type_num <- as.numeric(as.factor(V(usa_i)$type))
assortativity(usa_i, type_num)
