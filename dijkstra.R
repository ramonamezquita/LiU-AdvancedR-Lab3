dijkstra<- function(graph, init_node){
  nodes<- unique(c(graph$v1, graph$v2))
  distances<-rep(Inf, length(nodes))
  distances[which(nodes == init_node)]<-0
  visited<-rep(FALSE,length(nodes))
  
  while(TRUE){
    current_node<-which.min(distances[!visited])
    if (is.infinite(distances[current_node])) break
    visited[current_node]<-TRUE
    
    neighbors<- graph[graph$v1 == nodes[current_node], ]
    for (i in 1:nrow(neighbors)){
      neighbors_index<-which(nodes== neighbors$v2[i])
      if(!visited[neighbors_index]){
        new_distance<- distances[current_node] +neighbors$w[i]
        if (new_distance < distances[neighbors_index]){
          distances[neighbors_index]<- new_distance
        }
      }
    }
  }
  return(distances)
}

