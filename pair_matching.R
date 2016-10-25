#install.packages("rjson")
#install.packages("geosphere")
#install.packages("igraph")
#install.packages("lpSolve")
#install.packages("lpSolveAPI")

library(rjson)
library(geosphere)
library(igraph)
library(lpSolve)
library(lpSolveAPI)

n_matched <- function(file,d){

locations <- fromJSON(file)
locations <- lapply(locations, function(x) {
  unlist(x)
})
locations <- as.data.frame(locations)
locations <- data.frame(locations[,2],locations[,1])

pairwise_dist <- apply(locations, 1, FUN=function(X) distHaversine(X, locations))

potential_matches <- ifelse(pairwise_dist <= d & pairwise_dist != 0,1,0)

g <- graph.adjacency(potential_matches, mode="undirected")

#plot(g)

lprec <- make.lp(0, length(E(g)))
set.type(lprec,seq(1,length(E(g)),1), type ="binary")
set.objfn(lprec, rep(-1,length(E(g))))

a = data.frame(get.edgelist(g)[,2],get.edgelist(g)[,1])
names(a)=c("vertex1","vertex2")
b = get.edgelist(g)
b = unname(data.frame(b))
names(b)=names(a)
edge_list = rbind(a,b)
constraint <- function(g,v){
  return (ifelse((get.edgelist(g)[,1] == v & get.edgelist(g)[,2] %in% neighbors(g,v,mode="total")) | (get.edgelist(g)[,2] == v & get.edgelist(g)[,1] %in% neighbors(g,v,mode="total")),1,0))
}
for (v in unique(edge_list[,1])){
  add.constraint(lprec, constraint(g,v), "<=", 1)
}
#set.bounds(lprec, lower = rep(0,length(E(g))))
#set.bounds(lprec, upper = rep(1,length(E(g))))

solve(lprec)
obj = - get.objective(lprec)
matched_pairs = get.edgelist(g)[get.variables(lprec)==1,]
matched_users = unique(c(matched_pairs[,1],matched_pairs[,2]))
return (length(matched_users))
}

file = '~/Desktop/locations.json'
n_matched(file,4000)
