#Creating the shortest paths
library(igraph)
set.seed(2002)

num.nodes <- 10
#Generates a full graph with random weights, lat, and lon coodinates
#Should be used to test algorithms
RandomFullGraph <- function(num.nodes=4, weights=TRUE){
  if(weights){my.weights <- runif(length(combn(num.nodes,2)[1,]))}
  else{my.weights <- rep(0.5,length(combn(num.nodes,2)[1,])) }

el <- matrix(c(combn(num.nodes,2)[1,],combn(num.nodes,2)[2,],my.weights),ncol=3,byrow=FALSE)
g=graph.edgelist(el[,1:2], directed=FALSE)
E(g)$weight=as.numeric(el[,3])
V(g)$lat = runif(num.nodes)
V(g)$lon = runif(num.nodes)
return(g)
}
mytest <- RandomFullGraph(10, weights=FALSE)
plot(mytest,edge.width=E(mytest)$weight*6)

#IDEAS!!!!!!!!!!!!!!!!!!
#Time-to-Death could be the persons patiences for driving
#Creating a random hospital and judging if it will increase coverage of the area
plot(g)
mstg <- minimum.spanning.tree(g)
plot(mstg)
V(g)$name <- as.character(1:100)
## Some steiner nodes:
steiner.points <- sample(1:100, 5)

## Complete distance graph G'
Gi <- graph.full(5)
plot(Gi)
V(Gi)$name <- steiner.points

## Find a minimum spanning tree T' in G'
mst <- minimum.spanning.tree(Gi)
##  For each edge in mst, replace with shortest path:
edge_list <- get.edgelist(mst)

Gs <- mst
for (n in 1:nrow(edge_list)) {
  i <- edge_list[n,2]
  j <- edge_list[n,1]
  ##  If the edge of T' mst is shared by Gi, then remove the edge from T'
  ##    and replace with the shortest path between the nodes of g: 
  if (length(E(Gi)[which(V(mst)$name==i) %--% which(V(mst)$name==j)]) == 1) {
    ##  If edge is present then remove existing edge from the 
    ##    minimum spanning tree:
    Gs <- Gs - E(Gs)[which(V(mst)$name==i) %--% which(V(mst)$name==j)]
    
    ##  Next extract the sub-graph from g corresponding to the 
    ##    shortest path and union it with the mst graph:
    g_sub <- induced.subgraph(g, (get.shortest.paths(g, from=V(g)[i], to=V(g)[j])$vpath[[1]]))
    Gs <- graph.union(Gs, g_sub, byname=T)
  }
}

