
# these values are specified so that simulations can be run in the console (not in the shiny app)
ter.dens<-40
sep.dist<-5
inc.surv<-0.994
inc.period<-28
fledge.period<-35
relay.prob<-0.5
chick.surv<-0.982
lay.date<-100
period<-40
date.a.start<-80
date.a.end<-96
date.b.start<-140
date.b.end<-155
size<-2000
det.centre<-0.9
det.second<-0.8
det.third<-0.7
inc.offset<-0.6
library(rbenchmark)
sites<-5
sims<-10

# plot.ters function plots a distribution of n territories a min distance d apart from each other on a grid x0:x1:y0:y1
# points that are not min d distance from another will be removed. it will try 3000 times to plot the required 
# number of territories after which it will give up and return an error. 

#the way to resolve the two different size areas
#is to have simA() and simB() simulation functions, and the survey design
#radiobutton selects which one is called...
#so all the code has to be written out twice, esentially
#gives the ggplot output produced below, and also a GLM

#function for choosing survey design plottype
plotType <- function(type) {
  switch(type,
         search = area_search,
         tran = transects)
}


plot.ters <- function(n,x0,x1,y0,y1,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(n,x0,x1),runif(n,y0,y1))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) 
}


# the survival function is used to calculate the date of failure of nest/chicks given a daily survival probability
is_zero <- function(x) x == 0

survival<-function(period,prob.surv){
  rbinom(round(rnorm(1,period,3)),1,prob.surv) %>% detect_index(is_zero)}


# ter.outcomes uses the 'plot.ters' and 'survival' function and the parameters specified by the user  
# to calculate dates for each territory to start displaying, incubating, chicks hatching, fledging,
# nest predation, chick predation, relay dates. These data are then held in the dataframe 'terx'.

ter.outcomes<-function(){
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
                          if_else(terx$chick.success==0 & terx$chick.start>0,terx$chick.start+terx$chick.surv,0))
  terx[is.na(terx)] <- 0
  terx
}




# this vectorises the function between() which is used in the detekt function (below)
betw<-Vectorize(between)

#count detections at date.a of pairs displaying & incubating
detekt<-function(){
  date.b<-round(runif(1,date.b.start,date.b.end))
  date.a<-round(runif(1,date.a.start,date.a.end))
  terx<-ter.outcomes()
  
  detekt.a<-dt_case_when(betw(date.a,terx$disp,terx$inc.start)==TRUE & 
                           as.numeric(st_intersects(terx, centre))==1 & rbinom(n=nrow(terx),1,det.centre)==1  |
                           
                           betw(date.a,terx$disp,terx$inc.start)==TRUE &
                           as.numeric(st_intersects(terx, second))==1 & rbinom(n=nrow(terx),1,det.second)==1 |
                           
                           betw(date.a,terx$disp,terx$inc.start)==TRUE &
                           as.numeric(st_intersects(terx, third))== 1 & rbinom(n=nrow(terx),1,det.third)==1 |
                           
                           betw(date.b,terx$inc.start,terx$inc.end)==TRUE & 
                           as.numeric(st_intersects(terx, centre))==1 & rbinom(n=nrow(terx),1,det.centre*inc.offset)==1  |
                           
                           betw(date.b,terx$inc.start,terx$inc.end)==TRUE &
                           as.numeric(st_intersects(terx, second))==1 &  rbinom(n=nrow(terx),1,det.second*inc.offset)==1 |
                           
                           betw(date.b,terx$inc.start,terx$inc.end)==TRUE &  
                           as.numeric(st_intersects(terx, third))== 1 & rbinom(n=nrow(terx),1,det.third*inc.offset)==1 ~ 1,  
                         TRUE ~ 0)
  
  #it is assumed that at date.b all pairs are either incubating or with chicks
  
  detekt.b<-dt_case_when(betw(date.b,terx$chick.start,terx$chick.end)==TRUE & 
                           as.numeric(st_intersects(terx, centre))==1 & rbinom(n=nrow(terx),1,det.centre)==1  |
                           
                           betw(date.b,terx$chick.start,terx$chick.end)==TRUE &
                           as.numeric(st_intersects(terx, second))==1 & rbinom(n=nrow(terx),1,det.second)==1 |
                           
                           betw(date.b,terx$chick.start,terx$chick.end)==TRUE &
                           as.numeric(st_intersects(terx, third))== 1 & rbinom(n=nrow(terx),1,det.third)==1 |
                           
                           betw(date.b,terx$inc.start,terx$inc.end)==TRUE & 
                           as.numeric(st_intersects(terx, centre))==1 & rbinom(n=nrow(terx),1,det.centre*inc.offset)==1  |
                           
                           betw(date.b,terx$inc.start,terx$inc.end)==TRUE &
                           as.numeric(st_intersects(terx, second))==1 &  rbinom(n=nrow(terx),1,det.second*inc.offset)==1 |
                           
                           betw(date.b,terx$inc.start,terx$inc.end)==TRUE &  
                           as.numeric(st_intersects(terx, third))== 1 & rbinom(n=nrow(terx),1,det.third*inc.offset)==1 ~ 1,  
                         TRUE ~ 0)
  detekt.b[is.na(detekt.b)]<-0
  detekt.a[is.na(detekt.a)]<-0
  sums<-data.frame(sum(detekt.a))
  sums$late.det<-sum(detekt.b)
  sums$suc.ters<-sum(terx$chick.success)
  sums$tot.ters<-(n=nrow(terx))
  names(sums)[1]<-"early.det"
  sums
}

###this takes the detection function detekt() and runs it for however many sites you have surveyed
###it compares est.prod to actual.prod
simulate<-function(){
  res <- ldply(1:sites, function(i) data.table(iteration = i, detekt()))
  actual.prod<-sum(res$suc.ters)/sum(res$tot.ters)
  est.prod<-sum(res$late.det)/sum(res$early.det)
  output<-cbind(actual.prod,est.prod)
  output
}

###this produces the output graph which compares actual.prod to est.prod
res1<-gather(ldply(1:sims, function(i) data.table(iteration = i, simulate())), iteration)
ggplot(res1, aes(value, fill = iteration)) + geom_density(alpha = 0.2) +
  xlim(0,1)
res1

###this function produces sums of the estimated productivity
surv.prod<-function(){
res <- ldply(1:sites, function(i) tibble(iteration = i, detekt()))
r<-data.frame(sum(res$late.det),(sum(res$early.det)))
names(r)[[1]]<-"k"
names(r)[[2]]<-"n"
r
}

###
moddat<-function(){
mod.data<-ldply(1:sims, function(i) tibble(iteration = i, surv.prod()))
mod.data$set<-c("A","B")
mod.data
}

mod.data2 <- moddat() %>% 
  group_by(iteration) %>% 
  nest()

mod.data2

view(mod.data2)

mapmodels<-function(df){
lm(cbind(df$k,df$n-df$k)~factor(df$set),data = df, family="binomial")}

###this needs to be changed to an independent samples t-test
###instead of a lm / glm 
models <- map(mod.data2$data, mapmodels)



