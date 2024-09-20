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
    ri <- r2 %% r1
    r1 <- r2
    r2 <- ri
  }
  
  return(r1)
}