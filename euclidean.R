# LAB 3 732A94
# HELENA LLORENS LLUÍS

name <- "Helena Llorens Lluís"
liuid <- "hllor282"


####################### R CODE ##############################

########## ex 1.1.1. ################# 

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


euclidean(123612, 13892347912)



########## ex 1.1.2. ################# 
