

# plot.tersA is for the transect approach the total area is 10km2
plot.tersA <- function(n,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(10*n,-500,1500),runif(n,-500,4500))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) 
}

# plot.tersB is for the area_search approach the total area is 6km2
plot.tersB <- function(n,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(6*n,-500,1500),runif(n,-500,2500))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) 
}
plot.tersB(6,20)
plot.tersA(6,20)
