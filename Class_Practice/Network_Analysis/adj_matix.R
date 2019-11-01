library(network)
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
