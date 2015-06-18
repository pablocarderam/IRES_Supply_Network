#Directed Graph in R
setwd("~/Desktop/fall_2014_work/Spring_2014/DirectedGraphs")
library("igraph")

#Example of a ring graph
g <- graph.ring(10)
plot(g)
get.shortest.paths(g, 1)
shortest.paths(g)

#Assign weights to edges
el <- matrix(nc=3, byrow=TRUE, c(1,2,1, 1,3,5, 2,3,1, 3,4,1))
g2 <- add.edges(graph.empty(4), t(el[,1:2]), weight=el[,3])
plot(g2)
shortest.paths(g2, mode="out")

g3 <- matrix(c(0,1,5,0,
               0,0,1,0,
               0,0,0,1,
               0,0,0,0),ncol=4, byrow=TRUE)
net=graph.adjacency(g3,mode="directed",weighted=TRUE,diag=FALSE)
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold,
            edge.color="black",edge.width=E(net)$weight)



## Weighted shortest paths
el <- matrix(ncol=3, byrow=TRUE,
             c(1,2,0, 1,3,2, 1,4,1, 2,3,0, 2,5,5, 2,6,2, 3,2,1, 3,4,1,
               3,7,1, 4,3,0, 4,7,2, 5,6,2, 5,8,8, 6,3,2, 6,7,1, 6,9,1,
               6,10,3, 8,6,1, 8,9,1, 9,10,4) )
g2 <- add.edges(graph.empty(10), t(el[,1:2]), weight=el[,3])
plot(g2)
shortest.paths(g2, mode="out")

##
g <- graph.ring(10,directed=TRUE)               
plot(g)
ShortPth <- get.shortest.paths(g, 8, 2)    # List of path 8->2
ShortPth
E(g)$color <- "SkyBlue2"
E(g)$width <- 1
E(g, path=ShortPth[[1]])$color <- "red"
plot(g)

