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

  return(abs(r1))
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
dijkstra<- function(graph, init_node){


  # Check for correct input.
  mandatory_cols <- c("v1", "v2", "w")
  stopifnot(is.numeric(init_node), init_node %in% graph$v1)
  stopifnot(is.data.frame(graph))
  stopifnot(all(mandatory_cols %in% colnames(graph)))


  nodes<- unique(c(graph$v1, graph$v2))
  distances<-rep(Inf, length(nodes))
  distances[which(nodes == init_node)]<-0

  for(i in graph$v2[graph$v1 == init_node]){
    posiciones<- which(graph$v1==init_node & graph$v2==i)
    distances[which(nodes==i)]<- graph$w[posiciones]
  }

  visited<-rep(FALSE,length(nodes))
  visited[which ( nodes == init_node)]<-TRUE

  while(any(visited== FALSE)){
    j<-which.min(distances[visited==FALSE])
    current_node<-nodes[which(visited==FALSE)[j]]
    visited[which(nodes== current_node)]<-TRUE
    neighbors<-graph$v2[graph$v1 == current_node]
    for (x in neighbors){
      posicionesNEW<- which(graph$v1==current_node & graph$v2==x)
      weight<- graph$w[posicionesNEW]
      new_distance<- distances[which(nodes==current_node)]+weight
      if(new_distance<distances[which(nodes==x)]){
        distances[which(nodes==x)]<-new_distance
      }
    }
  }
  return(distances)
}
