#' Finds greatest common divisor using euclidean algorithm.
#' 
#' @param a An integer.
#' @param b An integer.
#' @return An integer.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean <- function(a, b){
  stopifnot(is.numeric(a), is.numeric(b))
  
  if(a > b){
    r1 <- a
    r2 <- b
  } else{
    r1 <- b
    r2 <- a
  }
  
  while(r2 != 0){
    ri <- r1 %% r2
    r1 <- r2
    r2 <- ri
  }
  
  return(r1)
}


#' Runs Dijkstra algorithm
#'
#' @param graph A dataframe.
#' @param init_node A numeric scalar that exists in the graph.
#' @return A vector
#' @examples
#' wiki_graph <- data.frame(
#' v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), 
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' )
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
dijkstra <- function(graph, init_node){
  nodes <- unique(c(graph$v1, graph$v2))
  distances <- rep(Inf, length(nodes))
  distances[which(nodes == init_node)] <- 0
  visited <- rep(FALSE,length(nodes))
  
  while(TRUE){
    current_node <- which.min(distances[!visited])
    if (is.infinite(distances[current_node])) break
    visited[current_node] <- TRUE
    
    neighbors <- graph[graph$v1 == nodes[current_node], ]
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