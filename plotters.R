# The shape of the area for which we need to run simulations for 
# is dependent on whether a 'transect' or 'area_search' is used

# points that are not min d distance from another will be removed. it will try 3000 times to plot the required 
# number of territories after which it will give up and return an error. 
#the way to resolve the two different size areas
#is to have simA() and simB() simulation functions, and the survey design
#radiobutton selects which one is called...
#so all the code has to be written out twice, esentially
#gives the ggplot output produced below, and also a GLM

# this is the basic version of the function which plots a distribution of n territories
# a min distance d apart from each other on a grid x0:x1:y0:y1

plot.ters <- function(n,x0,x1,y0,y1,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(n,x0,x1),runif(n,y0,y1))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) 
}

# plot.tersT is used for the transect survey design
plot.tersT <- function(n,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(10*n,-500,1500),runif(n,-500,4500))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) 
}

# plot.tersA is for the area_search survey design
plot.tersA <- function(n,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(6*n,-500,1500),runif(n,-500,2500))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) 
}

# test code, generate 
plot.tersT(6,20)
plot.tersA(6,20)
