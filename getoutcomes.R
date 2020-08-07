plot.ters <- function(n,x0,x1,y0,y1,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(n,x0,x1),runif(n,y0,y1))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) #removes points too close together 
}

#run weekly data
get.outcomes<-function(ter.dens,sep.dist,inc.surv,relay.prob,chick.surv){
  ter<-st_as_sf(data.frame(plot.ters(ter.dens,0,2000,0,2000,sep.dist)), coords = c("X1", "X2"))
  ter[[2]]<-"d"
  ter[[3]]<-"d"
  ter[[4]]<-if_else(ter[[3]]=="d" & ter[[2]]=="d" & rbinom(n=nrow(ter), size=1, 0.8), 'i','d')
  ter[[5]]<-if_else(ter[[4]]=="d" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i',
                    if_else(ter[[4]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i','f'))
  
  ter[[6]]<-if_else(ter[[5]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i', 
                    if_else(ter[[5]]=="f" & rbinom(n=nrow(ter), size=1, prob=relay.prob)>0.5, 'i', 'f'))
  ter[[7]]<-if_else(ter[[6]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i', 
                    if_else(ter[[5]]!="f" & ter[[6]]=="f" & rbinom(n=nrow(ter), size=1, prob=relay.prob)>0.5, 'i','f'))
  ####now chicks can potentially appear
  ter[[8]]<-if_else(ter[[7]]=="i" & ter[[6]]=="i" & ter[[5]]=="i" & ter[[4]]=="i" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c',
                    if_else(ter[[7]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i', 
                            if_else(ter[[7]]=="f" & rbinom(n=nrow(ter), size=1, prob=relay.prob)>0.5, 'i', 'f')))
  ###no relays allowed after V8, and chicks appear. 
  ###This means that the points need to start moving around
  ter[[9]]<-if_else(ter[[8]]=="f", "a",
                    if_else(ter[[8]]=="i" & ter[[7]]=="i" & ter[[6]]=="i" & ter[[5]]=="i" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c',
                            if_else(ter[[8]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i',
                                    if_else(ter[[8]]=="c" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c','a'))))
  ter[[10]]<-if_else(ter[[9]]=="f", "a",
                     if_else(ter[[9]]=="i" & ter[[8]]=="i" & ter[[7]]=="i" & ter[[6]]=="i" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c',
                             if_else(ter[[9]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i',
                                     if_else(ter[[9]]=="c" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c','a'))))
  ter[[11]]<-if_else(ter[[10]]=="f", "a",
                     if_else(ter[[10]]=="i" & ter[[9]]=="i" & ter[[8]]=="i" & ter[[7]]=="i" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c',
                             if_else(ter[[10]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i',
                                     if_else(ter[[10]]=="c" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c','a'))))
  ter[[12]]<-if_else(ter[[11]]=="f", "a",
                     if_else(ter[[11]]=="i" & ter[[10]]=="i" & ter[[9]]=="i" & ter[[8]]=="i" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c',
                             if_else(ter[[11]]=="i" & rbinom(n=nrow(ter), size=1, prob=inc.surv)>0.5, 'i',
                                     if_else(ter[[11]]=="c" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c','a'))))             
  ter[[13]]<-if_else(ter[[12]]=="a", "a",
                     if_else(ter[[12]]=="i" & ter[[11]]=="i" & ter[[10]]=="i" & ter[[9]]=="i" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c',
                             if_else(ter[[12]]=="c" & ter[[11]]=="c" & ter[[10]]=="c" & ter[[9]]=="c","fl",
                                     if_else(ter[[12]]=="c" & rbinom(n=nrow(ter), size=1, prob=chick.surv)>0.5, 'c','a'))))                                                                          
  
  ter2<-gather(ter, "week", "nest_status", 2:13)
  ter2$week<-as.numeric(if_else(ter2$week=="V2",1,
                                if_else(ter2$week=="V3",2,
                                        if_else(ter2$week=="V4",3,
                                                if_else(ter2$week=="V5",4,
                                                        if_else(ter2$week=="V6",5,
                                                                if_else(ter2$week=="V7",6,
                                                                        if_else(ter2$week=="V8",7,
                                                                                if_else(ter2$week=="V9",8,
                                                                                        if_else(ter2$week=="V10",9,
                                                                                                if_else(ter2$week=="V11",10,
                                                                                                        if_else(ter2$week=="V12",11,
                                                                                                                if_else(ter2$week=="V13",12,
                                                                                                                        13)))))))))))))
  ter2
}
