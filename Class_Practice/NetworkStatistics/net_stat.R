library(UserNetR)
library(network)
library(sna)
library(igraph)
library(intergraph)
library(ggplot2)

data("Moreno")

set.seed(1)
plot(Moreno)

network.size(Moreno)

33*(33-1)/2

# 46 = number of edges
2*46 / (33*(32))

sna::gden(Moreno)

lgc <- sna::component.largest(Moreno, result = "graph")
gd <- sna::geodist(lgc)
max(gd$gdist)


mat <- rbind(c(0,1,1,0,0),c(0,0,1,1,0),c(0,1,0,0,0),
             c(0,0,0,0,0),c(0,0,1,0,0))
rownames(mat) <- c("A","B","C","D","E")
colnames(mat) <- c("A","B","C","D","E")

net <- network(mat, matrix.type = "adjacency")
net_i <- graph_from_adjacency_matrix(mat)

d <- shortest.paths(net_i, v = V(net_i), to = V(net_i))
d

sum(d[upper.tri(d)])

16 * 2 / (5 * 4)

Moreno_i <- asIgraph(Moreno)

mean_distance(Moreno_i, directed = F)

gorder(Moreno_i)

edge_density(Moreno_i)

i_random <- erdos.renyi.game(n = gorder(Moreno_i),
                             p.or.m = edge_density(Moreno_i),
                             type = "gnp")

plot(i_random)


i_list <- list()

for(i in 1:1000){
  i_list[[i]] <- erdos.renyi.game(n = gorder(Moreno_i),
                                  p.or.m = edge_density(Moreno_i),
                                  type = "gnp")
}

avg_pl <- unlist(lapply(i_list, mean_distance, directed = FALSE ))

mean(avg_pl)

ggplot() + geom_histogram(aes(x = avg_pl)) + 
  geom_vline(xintercept = mean_distance(Moreno_i, directed = F),
             size = 1.2, color = "red")

mean(avg_pl < mean_distance(Moreno_i, directed = F))

