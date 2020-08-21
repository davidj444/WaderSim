library(rsconnect)
library(shiny)
library(data.table)
library(magrittr)
library(gifski)
library(mapview)
library(magick)
library(transformr)
library(sf)
library(scales)
library(ggplot2)
library(gganimate)
library(manipulate)
library(dplyr)
library(plyr)
install.packages("dtplyr")
install.packages("tidyfast")
install.packages("microbenchmark")
library(tidyfast)
library(dtplyr)
library(tidyr)
library(tidyverse)
library(raster)
library(Orcs)
library(lubridate)
library(purrr)
library(rbenchmark)
library(microbenchmark)

##these are the bits that need to be inputs on Rshiny
ter.dens<-90
sep.dist<-5
inc.surv<-0.98
inc.period<-28
fledge.period<-35
relay.prob<-0.5
chick.surv<-0.97
lay.date<-100
period<-40
survival<-0.9
date.a<-88
date.b<-150
size<-2000
det.centre<-0.9
det.second<-0.8
det.third<-0.7
inc.offset<-0.6

#code to create the transect detection areas
c1 <- cbind(c((size/2)-275, (size/2)-225, (size/2)-225, (size/2)-275), c(0, 0, size, size))
c1 <- coords2Polygons(c1, ID ="A")
c2 <- cbind(c((size/2)+225, (size/2)+275, (size/2)+275, (size/2)+225), c(0, 0, size, size))
c2 <- coords2Polygons(c2, ID = "A")
centre<-bind(c1, c2)
#create 25-100m bands either side of two 5km transects
s1 <- cbind(c((size/2)-350,(size/2)-275,(size/2)-275,(size/2)-350), c(0, 0, size, size))
s1a <- coords2Polygons(s1, ID = "B")
s2 <- cbind(c((size/2)-225, (size/2)-150, (size/2)-150, (size/2)-225), c(0, 0, size, size))
s2a <- coords2Polygons(s2, ID = "B")
s3 <- cbind(c((size/2)+150, (size/2)+225, (size/2)+225,(size/2)+150), c(0, 0, size, size))
s3a <- coords2Polygons(s3, ID = "B")
s4 <- cbind(c((size/2)+275, (size/2)+350, (size/2)+350, (size/2)+275), c(0, 0, size, size))
s4a <- coords2Polygons(s4, ID = "B")
#combine to one shapefile
merge1<-bind(s1a, s2a)
merge2<-bind(s3a, s4a)
sec<-bind(merge1, merge2)
####create 100-250m bands either side of two 5km transects
th1 <- cbind(c((size/2)-500,(size/2)-350,(size/2)-350,(size/2)-500), c(0, 0, size, size))
th1 <- coords2Polygons(th1, ID = "C")
th2 <- cbind(c((size/2)-150, size/2, size/2,(size/2)-150), c(0, 0, size, size))
th2 <- coords2Polygons(th2, ID = "C")
th3 <- cbind(c((size/2)+150, size/2, size/2, (size/2)+150), c(0, 0, size, size))
th3 <- coords2Polygons(th3, ID = "C")
th4 <- cbind(c((size/2)+350, (size/2)+500, (size/2)+500, (size/2)+350), c(0, 0, size, size))
th4 <- coords2Polygons(th4, ID = "C")
#combine to one shapefile
merge3<-bind(th1, th2)
merge4<-bind(th3, th4)
third<-bind(merge3, merge4)
second<-st_as_sf(sec)
centre<-st_as_sf(centre)
third<-st_as_sf(third)


#Need to think about how chick survival actually works - 
#it should really be a decreasing exponential binomial distribution back calculated from 
#a probability of fledging - this is easier to understand

#this is the code that gives a probability of survival through the incubation / 
#fledging period & a date of failure if failure
is_zero <- function(x) x == 0
survival<-function(period,survival){
rbinom(round(rnorm(1,period,3)),1,survival) %>% detect_index(is_zero)} 

plot.ters <- function(n,x0,x1,y0,y1,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(n,x0,x1),runif(n,y0,y1))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) #removes points too close together 
}

betw<-Vectorize(between)

#this is the good code
xx<-function(){
terx<-st_as_sf(data.frame(plot.ters(ter.dens,0,2000,0,2000,sep.dist)), coords = c("X1", "X2"))
terx$disp<-round(rnorm(n=nrow(terx), lay.date, sd = 3))-21
terx$inc.start<-terx$disp+21
terx$surv<-replicate(n=nrow(terx),survival(40,inc.surv))
terx$c<-terx$inc.start+round(rnorm(n=nrow(terx), inc.period, sd = 1))
terx$inc.success<-if_else(terx$surv==0,TRUE,
                            if_else((terx$surv+terx$inc.start)>terx$c,TRUE,FALSE))
terx$inc.end<-if_else(terx$inc.success==TRUE,terx$c,terx$surv+terx$inc.start)
terx$relay<-if_else(terx$inc.success==FALSE & rbinom(n=nrow(terx),1,relay.prob)>0.5,TRUE,FALSE)

#relay attempt starts 7 days after failure
terx$relay.start<-if_else(terx$relay==TRUE,terx$inc.end+7,0)
terx$relay.surv<-replicate(n=nrow(terx),survival(40,inc.surv))
terx$c2<-if_else(terx$relay==TRUE,terx$relay.start+round(rnorm(n=nrow(terx), inc.period, sd = 1)),0)
terx$relay.success<-if_else(terx$relay==FALSE,NA,
                              if_else(terx$relay.surv==0 & terx$relay==TRUE,TRUE,
                              if_else((terx$relay.surv+terx$relay.start)>terx$c2 &terx$relay==TRUE,TRUE,FALSE)))
terx$relay.end<-if_else(terx$relay.start==0,0,
    if_else(terx$relay.success==TRUE,terx$c2,terx$relay.surv+terx$relay.start))
  
terx$chick.start<-if_else(terx$inc.success==TRUE,terx$inc.end,
                           if_else(terx$relay.success==TRUE,terx$relay.end,0))
terx$chick.surv<-replicate(n=nrow(terx),survival(40,chick.surv))
terx$c3<-if_else(terx$inc.success==TRUE|terx$relay.success==TRUE,terx$chick.start+round(rnorm(n=nrow(terx), fledge.period, sd = 1)),0)

terx$chick.success<-if_else(terx$chick.surv==0 & terx$chick.start>0,1,
                                if_else(terx$chick.start+terx$chick.surv>terx$c3 & terx$c3 > 0,1,0))
terx$chick.end<-if_else(terx$chick.success==1,terx$c3,
                        if_else(terx$chick.success==0,terx$chick.start+terx$chick.surv,0))
terx[is.na(terx)] <- 0
terx
}


#this is seperated into one function so you can run lots of simulations of territories, and then
#change the dates without having to do all the simulations of territories over again


####we assume that visit.a needs to include display and incubation, and visit.b needs to include incubation and chicks

#count detections at date.a of pairs displaying & incubating
testfunc<-function(){
xx()
  
date.a<-round(runif(1,date.a.start,date.a.end))
  
detect$det.a<-dt_case_when(betw(date.a,tt$disp,tt$inc.start)==TRUE & 
                            as.numeric(st_intersects(tt, centre))==1 & rbinom(n=nrow(tt),1,det.centre)==1  |
                            
                            betw(date.a,tt$disp,tt$inc.start)==TRUE &
                            as.numeric(st_intersects(tt, second))==1 & rbinom(n=nrow(tt),1,det.second)==1 |
                            
                            betw(date.a,tt$disp,tt$inc.start)==TRUE &
                            as.numeric(st_intersects(tt, third))== 1 & rbinom(n=nrow(tt),1,det.third)==1 |
                            
                            betw(date.b,tt$inc.start,tt$inc.end)==TRUE & 
                            as.numeric(st_intersects(tt, centre))==1 & rbinom(n=nrow(tt),1,det.centre*inc.offset)==1  |
                            
                            betw(date.b,tt$inc.start,tt$inc.end)==TRUE &
                            as.numeric(st_intersects(tt, second))==1 &  rbinom(n=nrow(tt),1,det.second*inc.offset)==1 |
                             
                            betw(date.b,tt$inc.start,tt$inc.end)==TRUE &  
                            as.numeric(st_intersects(tt, third))== 1 & rbinom(n=nrow(tt),1,det.third*inc.offset)==1 ~ 1,  
                          TRUE ~ 0)
 
#count detections at date.b of pairs incubationg & with chicks
date.b<-round(runif(1,date.b.start,date.b.end)) 
  
detect$det.b<-dt_case_when(betw(date.b,tt$chick.start,tt$chick.end)==TRUE & 
                           as.numeric(st_intersects(tt, centre))==1 & rbinom(n=nrow(tt),1,det.centre)==1  |
                         
                           betw(date.b,tt$chick.start,tt$chick.end)==TRUE &
                           as.numeric(st_intersects(tt, second))==1 & rbinom(n=nrow(tt),1,det.second)==1 |
  
                           betw(date.b,tt$chick.start,tt$chick.end)==TRUE &
                           as.numeric(st_intersects(tt, third))== 1 & rbinom(n=nrow(tt),1,det.third)==1 |
                          
                           betw(date.b,tt$inc.start,tt$inc.end)==TRUE & 
                           as.numeric(st_intersects(tt, centre))==1 & rbinom(n=nrow(tt),1,det.centre*inc.offset)==1  |
                           
                           betw(date.b,tt$inc.start,tt$inc.end)==TRUE &
                           as.numeric(st_intersects(tt, second))==1 &  rbinom(n=nrow(tt),1,det.second*inc.offset)==1 |
                           
                           betw(date.b,tt$inc.start,tt$inc.end)==TRUE &  
                           as.numeric(st_intersects(tt, third))== 1 & rbinom(n=nrow(tt),1,det.third*inc.offset)==1 ~ 1,  
                         TRUE ~ 0)

act.success<-sum(terx$chick.success)
ttx[[1]]<-sum(tt$xx.det.a)
ttx[[2]]<-sum(tt$xx.det.b)
ttx[[3]]<-sum(tt$chick.success)
xx<-sum(terx$chick.success)
xx
sum(xx)
typeof(tt$chick.success)
ttx[[3]]<-sum(tt$chick.success)
tt$chick.success

productivity<-sum(tt$xx.det.b)/sum(tt$xx.det.a)
productivity
}
prods<-replicate(30,testfunc())
prods

test1<-function(){
terx$inc.success<-mutate(terx,if_else(terx$surv==0,'TRUE',
                          if_else((terx$surv+terx$inc.start)>terx$c,'TRUE','FALSE')))
}
terx<-as_tibble(terx)
test1<-function(){
  terx$inc.success<-if_else(terx$surv==0,'TRUE',
                            if_else((terx$surv+terx$inc.start)>terx$c,'TRUE','FALSE'))
}

test2<-function(){
terx$inc.sucess<-mutate(terx,dt_case_when(terx$surv==0 ~ 'TRUE',
                               (terx$surv+terx$inc.start)>terx$c ~ 'TRUE',
                               ))
}
survival<-function(period,survival){
  rbinom(round(rnorm(1,period,3)),1,survival) %>% detect_index(is_zero)}
microbenchmark(survival(40,.98),test2())
##### attempt at rubbish below here

  
terx$ii<-min(which(rbinom(round(rnorm(n=nrow(terx), 40, sd = 3)),1,inc.surv)==0))
  terx$inc.period<-round(rnorm(n=nrow(terx), inc.period, sd = 1))
  terx$hatch<-terx$lay+terx$inc.period
  terx$hatch<-terx$lay+terx$inc.period
  terx
  terx$inc.fail[sapply(terx$fail, is.infinite)] <- terx$hatch-terx$lay
  
  terx
  terx$relay<-if_else(terx$fail=="NA",0,
                      if_else(terx$fail>1 & rbinom(n=nrow(terx),1,relay.prob)>0.5,terx$fail+7,0))

surveys<-15
date.a.start<-80
date.a.end<-95
date.b.start<-120
date.b.end<-150

date.a<-round(runif(1,date.a.start,date.a.end))
date.b<-round(runif(1,date.b.start,date.b.end))
date.a
date.b


z <- sample(c(TRUE, FALSE), 1000, rep = TRUE)

sum(z)
