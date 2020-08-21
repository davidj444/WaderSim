install.packages("profvis")
install.packages("rbenchmark")
library(rbenchmark)
library(profvis)
Xi.surv<-0.985
Xc.surv<-0.985
Xinc.period<-28
Xfledge.period<-35
Xrelay.prob<-0.7
Xlay.date<-100
ter.dens<-20
indTBL <- tibble(id=1:100,
                 type=sample(1:3, size=100, replace=T),
                 age=1)
view(indTBL)
x
benchmark(xtest())
xtest<-function(){
x <- matrix(NaN, nrow=ter.dens*4, ncol=17)
#,1 disp date
x[,1]<-round(rnorm(n=nrow(x), Xlay.date, sd = 3))-21
#,2 inc.start
x[,2]<-x[,1]+21
#,3 (surv) incubation - 0 is success, number is notional predation date
x[,3]<-replicate(n=nrow(x),survival(40,Xi.surv))
#,4 c - provisional hatching date
x[,4]<-x[,2]+round(rnorm(n=nrow(x), Xinc.period, sd = 1))
#,5 - inc.success
x[,5]<-if_else(x[,3]==0 | (x[,3]+x[,2])>x[,4],1,0)
#,6 - inc.end
x[,6]<-if_else(x[,5]==1,x[,4],x[,3]+x[,2])
#,7 - relay 
x[,7]<-if_else(x[,5]==0 & rbinom(n=nrow(x),1,Xrelay.prob)>0.5,1,0)
#,8 - relay.start - time of relay = failure + 7 here
x[,8]<-if_else(x[,7]==1,x[,6]+7,0)
#,9 relay.surv
x[,9]<-replicate(n=nrow(x),survival(40,Xi.surv))
#,10 c2 - provisional relay hatch date
x[,10]<-if_else(x[,7]==1,x[,8]+round(rnorm(n=nrow(x), Xinc.period, sd = 1)),0)
#,11 relay.success
x[,11]<-if_else(x[,7]==0,0,
                if_else(x[,9]==0 & x[,7]==1,1,
                        if_else((x[,9]+x[,8])>x[,10] & x[,7]==1,1,0)))
#,12 relay.end
x[,12]<-if_else(x[,8]==0,0,
                if_else(x[,11]==1,x[,10],x[,9]+x[,8]))
#,13 chick.start
x[,13]<-if_else(x[,5]==1,x[,6],
                if_else(x[,11]==1,x[,12],0))
#,14 chick.surv
x[,14]<-replicate(n=nrow(x),survival(40,Xc.surv))
#,15 c3
x[,15]<-if_else(x[,5]==1|x[,11]==1,x[,13]+round(rnorm(n=nrow(x), Xfledge.period, sd = 1)),0)
#,16 chick.success
x[,16]<-if_else(x[,14]==0 & x[,13] > 0, 1,
                if_else(x[,13] + x[,14] > x[,15] & x[,15] > 0, 1, 0))
#,17 chick end
x[,17]<-if_else(x[,16]==1,x[,15],
                if_else(x[,16]==0 & x[,13]>0,x[,13]+x[,14],0))
y<-plot.ters(ter.dens*4,0,2000,0,2000,sep.dist)

xy<-cbind(x,y)
xy
}


x<-tibble(plot.ters(ter.dens*4,0,2000,0,2000,sep.dist))
x

xy<-function(){
  terx.xy<-st_as_sf(data.frame(plot.ters(ter.dens*4,0,2000,0,2000,sep.dist)), coords = c("X1", "X2"))
  terx<-tibble(id=(1:(n=nrow(terx.xy)))) %>%
  mutate(x,disp=round(rnorm(n=nrow(terx.xy), lay.date, sd = 3))-21) %>% 
  mutate(inc.start=disp+21) %>% 
  mutate(surv=replicate(n=nrow(terx.xy),survival(40,Xi.surv))) %>% 
  mutate(c=inc.start+round(rnorm(n=nrow(terx.xy), Xinc.period, sd = 1))) %>% 
  mutate(inc.success=if_else(surv==0|(surv+inc.start>c),1,0)) %>% 
  mutate(inc.end=if_else(inc.success==1,c,surv+inc.start)) %>%                           
  mutate(relay=if_else(inc.success==0 & rbinom(n=nrow(terx.xy),1,Xrelay.prob)>0.5,1,0)) %>% 
  mutate(relay.start=if_else(relay==1,inc.end+7,0)) %>% 
  mutate(relay.surv=replicate(n=nrow(terx.xy),survival(40,Xi.surv))) %>% 
  mutate(c2=if_else(relay==1,relay.start+round(rnorm(n=nrow(terx.xy), Xinc.period, sd = 1)),0)) %>% 
  mutate(relay.success=if_else(relay==0,0,
                              if_else(relay.surv==0 & relay==1,1,
                                      if_else((relay.surv+relay.start)>c2 & relay==1,1,0)))) %>% 
  mutate(relay.end=if_else(relay.start==0,0,
                          if_else(relay.success==1,c2,relay.surv+relay.start))) %>% 
  mutate(chick.start=if_else(inc.success==1,inc.end,
                            if_else(relay.success==1,relay.end,0))) %>% 
  mutate(chick.surv=replicate(n=nrow(terx.xy),survival(40,Xc.surv))) %>% 
  mutate(c3=if_else(inc.success==1|relay.success==1,chick.start+round(rnorm(n=nrow(terx.xy), Xfledge.period, sd = 1)),0)) %>% 
  mutate(chick.success=if_else(chick.surv==0 & chick.start > 0, 1,
                 if_else(chick.start + chick.surv > c3 & c3 > 0, 1, 0))) %>% 
  mutate(chick.end=if_else(chick.success==1,c3,
                          if_else(chick.success==0,chick.start+chick.surv,0)))
  terx
}
xy()

benchmark(xy())
time2
