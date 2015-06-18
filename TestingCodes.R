#Run This First
library(igraph)
set.seed(2002)

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
  V(g)$avg_inc = rpois(10, lambda=rpois(10, lambda = 20))
  return(g)
}

#Example
mytest <- RandomFullGraph(10, weights=T)
V(mytest)
plot(mytest,edge.width=E(mytest)$weight*6)

#Procedure 2
for(i in order(V(mytest)$avg_inc)){
  #Find Absorbing Node
  min.dis <- min(E(mytest)$weight[get.edgelist(mytest)[,1] == i | get.edgelist(mytest)[,2] == i])
  my.index <- which(E(mytest)$weight == min.dis)
  if(get.edgelist(mytest)[my.index,1] == i){V(mytest)$ab_node[i] <- get.edgelist(mytest)[my.index,2]}
  else{V(mytest)$ab_node[i] <- get.edgelist(mytest)[my.index,1]}
  
#Checks if two nodes have the same area code
  if (length(intersect(as.numeric(V(mytest)[V(mytest)$row[i] == V(mytest)$row]),
                       as.numeric(V(mytest)[V(mytest)$col[i] == V(mytest)$col]))) > 1
#Checks if distance to next hospital is shorter than time to death
      && min.dis < time.to.death
#Check if absorbing node will be over capacity
      && V(mytest)$avg_inc[V(mytest)$ab_node[i]] + V(mytest)$avg_inc[i] < 
         V(mytest)$max_cap[V(mytest)$ab_node[i]])
#Add incidence rate to absorbing node and close current node
      {V(mytest)$avg_inc[V(mytest)$ab_node[i]] <- V(mytest)$avg_inc[V(mytest)$ab_node[i]] + V(mytest)$avg_inc[i];
       mytest <- delete.vertices(mytest, v=i); print(paste("Deleted node", i))}
  else{print(paste("Kept node", i))}
}

#Procedure 3
Total_antivenom <- 2000
Amt_per_treat <- 10
V(mytest)$frac_inc <- V(mytest)$avg_inc/sum(V(mytest)$avg_inc)
V(mytest)$treat_ava <- V(mytest)$frac_inc*Total_antivenom/Amt_per_treat
V(mytest)$deaths <- V(mytest)$avg_inc- V(mytest)$treat_ava
