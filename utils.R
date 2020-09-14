# the best way to do ter.dens is a) take a decimal value, b) multiply and c) round

# This is a blank version of the code, 
# these values are specified so that simulations can be run in the console (not in the shiny app)
ter.dens<-40
inc.surv<-0.99
sep.dist<-5
inc.period<-28
fledge.period<-35
relay.prob<-0.5
chick.surv<-0.982
lay.date<-100
period<-40
Edate.st<-80
Edate.end<-95
Ldate.st<-145
Ldate.end<-160
size<-2000
det.centre<-0.9
det.second<-0.8
det.third<-0.7
inc.offset<-0.6
sites<-5
sims<-10

# vectorize date search function
betw<-Vectorize(between)

# 
plotType <- function(type) {
  switch(type,
         search = area_search,
         tran = transects)
}



# the survival function is used to calculate the date of failure of nest/chicks given a daily survival probability
is_zero <- function(x) x == 0

survival<-function(period,prob.surv){
  rbinom(round(rnorm(1,period,3)),1,prob.surv) %>% detect_index(is_zero)}


# ter.outcomes uses the 'plot.ters' and 'survival' function and the parameters specified by the user  
# to calculate dates for each territory to start displaying, incubating, chicks hatching, fledging,
# nest predation, chick predation, relay dates. These data are then held in the dataframe 'terx'.

ter.outcomes<-function(ter.dens, sep.dist, disp.period, lay.date, inc.surv, inc.period, relay.prob, chick.surv,
                       fledge.period, det.centre, det.second, det.third, inc.offset, Edate.st, Edate.end, Ldate.st, Ldate.end){
  
  # plot territories and convert to SF
  terx<-st_as_sf(data.frame(plot.ters(ter.dens*10,-500,1500,-500,4500,sep.dist)), coords = c("X1", "X2"))
  
  #start display 21 days before lay.date ****with standard deviation of 3***
  terx$disp<-round(rnorm(n=nrow(terx), lay.date, sd = 3))-disp.period
  
  #incubation starts
  terx$inc.start<-terx$disp+disp.period
  
  #use survival function to predict survival for 40 days - if function returns a '0', the nest would 
  #survive for 40 days - if it returns a value, the nest fails after this many days
  terx$surv<-replicate(n=nrow(terx),survival(40,inc.surv))

  #get date that incubation period would notionally end if successful (inc.period has standard deviation = 1)
  terx$c<-terx$inc.start+round(rnorm(n=nrow(terx), inc.period, sd = 1))
  
  #if survival continues for period > than inc.period, then inc.success is true
  terx$inc.success<-if_else(terx$surv==0,TRUE,
                            if_else((terx$surv+terx$inc.start)>terx$c,TRUE,FALSE))
  
  #date that incubation ends - regardless of whether nest is predated or successful
  terx$inc.end<-if_else(terx$inc.success==TRUE,terx$c,terx$surv+terx$inc.start)
  
  # do failed territories relay? [TRUE or FALSE]
  terx$relay<-if_else(terx$inc.success==FALSE & rbinom(n=nrow(terx),1,relay.prob)>0.5,TRUE,FALSE)
  
  #date of relay attempt [returns zero if no relay]
  terx$relay.start<-if_else(terx$relay==TRUE,terx$inc.end+7,0)
  
  # use survival function same as for terx$surv
  terx$relay.surv<-replicate(n=nrow(terx),survival(40,inc.surv))
  
  #date when relay incubation would end
  terx$c2<-if_else(terx$relay==TRUE,terx$relay.start+round(rnorm(n=nrow(terx), inc.period, sd = 1)),0)
  
  # was relay successful [TRUE or FALSE]
  terx$relay.success<-if_else(terx$relay==FALSE,NA,
                              if_else(terx$relay.surv==0 & terx$relay==TRUE,TRUE,
                                      if_else((terx$relay.surv+terx$relay.start)>terx$c2 &terx$relay==TRUE,TRUE,FALSE)))
  
  # date end of incubation
  terx$relay.end<-if_else(terx$relay.start==0,0,
                          if_else(terx$relay.success==TRUE,terx$c2,terx$relay.surv+terx$relay.start))
  
  # date that chicks hatch, whether from 1st or 2nd attempt
  terx$chick.start<-if_else(terx$inc.success==TRUE,terx$inc.end,
                            if_else(terx$relay.success==TRUE,terx$relay.end,0))
  
  # use survival function same as for terx$surv
  terx$chick.surv<-replicate(n=nrow(terx),survival(40,chick.surv))
  
  #get date of fledging
  terx$c3<-if_else(terx$inc.success==TRUE|terx$relay.success==TRUE,terx$chick.start+round(rnorm(n=nrow(terx), fledge.period, sd = 1)),0)
  
  #do chicks survive to date of fledging
  terx$chick.success<-if_else(terx$chick.surv==0 & terx$chick.start>0,1,
                              if_else(terx$chick.start+terx$chick.surv>terx$c3 & terx$c3 > 0,1,0))
 #get fledging date
   terx$chick.end<-if_else(terx$chick.success==1,terx$c3,
                          if_else(terx$chick.success==0 & terx$chick.start>0,terx$chick.start+terx$chick.surv,0))
  # change NA values to zero

################## the next stage is to see how many territories are detected on survey visits ###################################
  
  # select date for first visit from specified range
date.early<-round(runif(1,Edate.st,Edate.end))
  
  # select date for second visit from specified range
date.late<-round(runif(1,Ldate.st,Ldate.end))
 terx 
  # assume that at first visit pairs are either displaying or incubating - 
terx$detekt.a<-dt_case_when(
                           # count early detections of displaying birds in centre band, applying detection probability 'det.centre'
                           betw(date.early,terx$disp,terx$inc.start)==TRUE & 
                           as.numeric(st_intersects(terx, centre)) > 0 & rbinom(n=nrow(terx),1,det.centre)==1  |
                           
                           # count early detections of displaying birds in second band, applying detection probability 'det.second'
                           betw(date.early,terx$disp,terx$inc.start)==TRUE &
                           as.numeric(st_intersects(terx, second)) > 0 & rbinom(n=nrow(terx),1,det.second)==1 |
                           
                           # count early detections of displaying birds in third band, applying detection probability 'det.third'
                           betw(date.early,terx$disp,terx$inc.start)==TRUE &
                           as.numeric(st_intersects(terx, third)) > 0 & rbinom(n=nrow(terx),1,det.third)==1 |
                           
                           # count early detections of incubating birds in centre band, applying detection probability 'det.centre'
                           betw(date.early,terx$inc.start,terx$inc.end)==TRUE & 
                           as.numeric(st_intersects(terx, centre)) > 0 & rbinom(n=nrow(terx),1,det.centre*inc.offset)==1  |
                           
                           # count early detections of incubating birds in second band, applying detection probability 'det.second'
                           betw(date.early,terx$inc.start,terx$inc.end)==TRUE &
                           as.numeric(st_intersects(terx, second)) > 0 &  rbinom(n=nrow(terx),1,det.second*inc.offset)==1 |
                           
                           # count early detections of incubating birds in third band, applying detection probability 'det.third'  
                           betw(date.early,terx$inc.start,terx$inc.end)==TRUE &  
                           as.numeric(st_intersects(terx, third)) > 0 & rbinom(n=nrow(terx),1,det.third*inc.offset)==1 ~ 1,  
                         TRUE ~ 0)

  #it is assumed that at date.b all pairs are either incubating or with chicks
  # then we count late survey values using same logic
  terx$detekt.b<-dt_case_when(betw(date.late,terx$chick.start,terx$chick.end)==TRUE & 
                           as.numeric(st_intersects(terx, centre)) > 0 & rbinom(n=nrow(terx),1,det.centre)==1  |
                           
                           betw(date.late,terx$chick.start,terx$chick.end)==TRUE &
                           as.numeric(st_intersects(terx, second)) > 0 & rbinom(n=nrow(terx),1,det.second)==1 |
                           
                           betw(date.late,terx$chick.start,terx$chick.end)==TRUE &
                           as.numeric(st_intersects(terx, third)) > 0 & rbinom(n=nrow(terx),1,det.third)==1 |
                           
                           betw(date.late,terx$inc.start,terx$inc.end)==TRUE & 
                           as.numeric(st_intersects(terx, centre)) > 0 & rbinom(n=nrow(terx),1,det.centre*inc.offset)==1  |
                           
                           betw(date.late,terx$inc.start,terx$inc.end)==TRUE &
                           as.numeric(st_intersects(terx, second)) > 0 &  rbinom(n=nrow(terx),1,det.second*inc.offset)==1 |
                           
                           betw(date.late,terx$inc.start,terx$inc.end)==TRUE &  
                           as.numeric(st_intersects(terx, third)) > 0 & rbinom(n=nrow(terx),1,det.third*inc.offset)==1 ~ 1,  
                         TRUE ~ 0)
  # remove NA values
  terx[is.na(terx)] <- 0
  terx
  sums<-data.frame(sum(terx$detekt.a))
  sums$late.det<-sum(terx$detekt.b)
  sums$suc.ters<-sum(terx$chick.success)
  sums$tot.ters<-(n=nrow(terx))
  names(sums)[1]<-"early.det"
  sums
}


# run ter.outcomes function over specified number of sites ('sites') and aggregate data

run.sites<-function(ter.dens,sep.dist,disp.period,lay.date,inc.surv,inc.period,relay.prob,chick.surv,
                    fledge.period,det.centre,det.second,det.third,inc.offset, Edate.st, Edate.end, Ldate.st, Ldate.end, sites){
  
  res <- ldply(1:sites, function(i) data.table(iteration = i, ter.outcomes(ter.dens, sep.dist, disp.period, lay.date, inc.surv, inc.period, relay.prob, chick.surv,
                                                                           fledge.period, det.centre, det.second, det.third, inc.offset, Edate.st, Edate.end, Ldate.st, Ldate.end)))
  
  output<-cbind(sum(res$suc.ters)/sum(res$tot.ters),sum(res$late.det)/sum(res$early.det),sum(res$early.det),sum(res$late.det))
  colnames(output) <- c("actual.prod","est.prod","n","k")
  output
}

###   params in runsims <- c("lay.date","inc.period","fledge.period","inc.surv","relay.prob","chick.surv","relay.prob",
###   "ter.dens","sep.dist","band1","band2","band3","inc.offset","sites","Edates","Ldates")

###this runs the simulation 'n number of times'sims' number of times
run.sims<-function(sims, ter.dens, sep.dist, disp.period, lay.date, inc.surv, inc.period, relay.prob, chick.surv,
                   fledge.period, det.centre, det.second, det.third, inc.offset, Edate.st, Edate.end, Ldate.st, Ldate.end, sites){
  
  ldply(1:sims, function(i) data.table(iteration = i, run.sites(ter.dens, sep.dist, disp.period, lay.date, inc.surv, inc.period, relay.prob, chick.surv,
                                                                fledge.period, det.centre, det.second, det.third, inc.offset, Edate.st, Edate.end,Ldate.st, Ldate.end, sites)))
}

a_data<-(run.sims(5, 5, 30,21, 100, 0.9, 28, 0.5, 0.9, 30, 0.9, 0.8, 0.7, 0.5, 85, 100,150,160, 12))
b_data<-(run.sims(5, 5, 30,21, 100, 0.95, 28, 0.5, 0.95, 30, 0.9, 0.8, 0.7, 0.5,95, 110,145,150, 12))

#this produces a quick frequency plot of actual vs estimated productivity
a_data %>% .[c(1:3)] %>% gather(iteration) %>% 
  ggplot(aes(value, fill = iteration)) + geom_density(alpha = 0.2) +
  xlim(0,1)

# run the fisher test
sig.test <- cbind(a_data %>% .[c(1,4:5)], b_data %>% .[c(4:5)] %>% rename(n2 = n,k2 = k)) %>%   
  group_by(iteration) %>% 
  nest()  %>% 
  mutate(matrix = map(data, ~matrix(unlist(.x), nrow = 2))) %>% 
  mutate(fisher = map(matrix, ~fisher.test(.x))) %>% 
  mutate(stats = map(fisher, ~broom::glance(.x))) %>% 
unnest(stats)

sig.test$fill_dot<-case_when(sig.test$p.value < 0.05 ~ "sig",
                            sig.test$p.value > 0.10 ~ "not.sig",
                            TRUE ~ "marg.sig")

# plot should show the estimates, converted into productivity difference as well

p.plot <-ggplot(sig.test, aes(x=p.value, y=estimate)) + 
  geom_point(aes(fill=fill_dot), size = 1.25) +
  theme_minimal() + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  geom_line((aes(x = 0.05)), color = "red", linetype = "dashed")
p.plot

variables_a<-c(input$a_disp.period,input$a_ter.dens, input$a_sep.dist, yday(input$a_lay.date), input$a_inc.surv, input$a_inc.period, input$a_relay.prob, input$a_chick.surv,
               input$a_fledge.period, input$a_det.centre, input$a_det.second, input$a_det.third, input$a_inc.offset,yday(input$a_Edates[1]),yday(input$a_Edates[2]),yday(input$a_Ldates[1]),yday(input$a_Ldates[2]))

