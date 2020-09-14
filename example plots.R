

example.outcomes<-function(disp.period,ter.dens, sep.dist, lay.date, inc.surv, inc.period, relay.prob, chick.surv,
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
  terx[is.na(terx)] <- 0
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
  
  ######################################################## what are birds doing on early visit################################### 
  terx$action.early<-
    if_else(betw(date.early,terx$disp,terx$inc.start)==TRUE, 'dis',
    if_else(betw(date.early,terx$inc.start,terx$inc.end)==TRUE, 'inc',
    if_else(date.early > terx$inc.end & date.early < terx$relay.start & terx$inc.success == FALSE, 'fail','fail')))
 
 ######################################################## detect displaying birds###################################### 
  terx<-mutate(terx, action.early=replace(action.early, action.early=='dis'&
                                         as.numeric(st_intersects(terx, centre))>0 & rbinom(n=nrow(terx),1,det.centre)==1 |
                                         action.early=='dis'&
                                         as.numeric(st_intersects(terx, second))>0 & rbinom(n=nrow(terx),1,det.second)==1 | 
                                         action.early=='dis'&
                                         as.numeric(st_intersects(terx, third))>0 & rbinom(n=nrow(terx),1,det.third)==1, 'countdis')) 
 terx
  # detect incubating birds
 terx<-  mutate(terx, action.early=replace(action.early, action.early=='inc' &
                               as.numeric(st_intersects(terx, centre))>0 & rbinom(n=nrow(terx),1,det.centre*inc.offset)==1 |  
                               action.early=='inc' &
                               as.numeric(st_intersects(terx, second))>0 & rbinom(n=nrow(terx),1,det.second*inc.offset)==1 |  
                               action.early=='inc' &
                               as.numeric(st_intersects(terx, third))>0 & rbinom(n=nrow(terx),1,det.third*inc.offset)==1, 'countinc')) 
terx
  # what are birds doing on late visit
  
  terx$action.late<-
    if_else(betw(date.late,terx$chick.start,terx$chick.end)==TRUE, 'chick',
            if_else(betw(date.late,terx$inc.start,terx$inc.end)==TRUE, 'inc',
                    if_else(date.late > terx$chick.end & terx$chick.success== 0 | terx$chick.start == 0, 'ab',
                            if_else(date.late > terx$chick.end & terx$chick.success==1,'fl','fail'))))

  # detect some birds on late visit
 terx<- terx %>% mutate(action.late = replace(action.late, action.late=='inc'&
                                          as.numeric(st_intersects(terx, centre))>0 & rbinom(n=nrow(terx),1,det.centre*inc.offset)==1 |  
                                          action.early=='inc' &
                                          as.numeric(st_intersects(terx, second))>0 & rbinom(n=nrow(terx),1,det.second*inc.offset)==1 |  
                                          action.early=='inc' &
                                          as.numeric(st_intersects(terx, third))>0 & rbinom(n=nrow(terx),1,det.third*inc.offset)==1, 'countinc')) %>%
   
   mutate(action.late = replace(action.late, action.late=='chick'&
                                  as.numeric(st_intersects(terx, centre))>0 & rbinom(n=nrow(terx),1,det.centre)==1 |  
                                  action.late=='chick' &
                                  as.numeric(st_intersects(terx, second))>0 & rbinom(n=nrow(terx),1,det.second)==1 |  
                                  action.late=='chick' &
                                  as.numeric(st_intersects(terx, third))>0 & rbinom(n=nrow(terx),1,det.third)==1, 'countchick'))
 
terx
}

dat<-(example.outcomes(21,5,30,100, 0.9, 28, 0.5, 0.9, 30, 0.9, 0.8, 0.7, 0.5, 85, 100,150,160))

fills<- c("dis" = "blue","countdis" = "blue","inc"="green","countinc"="green","chick" = "purple","countchick" = "purple","fl"="black","fail"= "red","ab"="grey82")
cols<- c("dis" = "grey88","countdis" = "yellow","inc"="grey88","countinc"="yellow","chick" = "grey88","countchick" = "yellow","fl"="grey88","fail"= "grey88","ab"="grey88")
limits <- c("dis","countdis","inc","countinc","chick","countchick","fl","fail","ab")
labs <- c("Display","countdis","Incubate","countinc","Chicks","countchick","Fledged","Nest failed","Attempt abandoned")
breaks <- c("dis","inc","chick","fl","fail","ab")
labels <- c("Display","Incubation","Chicks","Fledged","Nest failed","Attempt abandoned")

mytheme<-theme(
  legend.text = element_text(size = 13),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank(),
  axis.text.y=element_blank(),
  axis.line.x = element_line(arrow=arrow(length = unit(2, 'mm'))),
  axis.line.y = element_line(arrow=arrow(length = unit(2, 'mm'))),
  legend.title=element_blank(),
  panel.background = element_blank())

xlim_tran<-c(-500,1500)
ylim_tran<-c(-500, 4500)

p1<-ggplot(data = dat) +
  geom_sf(data=centre, fill = "grey82") +
  geom_sf(data=second, fill = "grey88") +
  geom_sf(data=third, fill = "grey95") +
  geom_sf(size = 2.5, shape = 21, aes(fill=action.early, colour=action.early, stroke = 2)) + 
  coord_sf(xlim = xlim_tran, ylim = ylim_tran)+
  labs(x = "2km", y = "5km")+
  scale_colour_manual(labels = labs, 
                      values = cols,
                      limits = limits) + 
  scale_fill_manual  (limits = limits, 
                      values = fills, 
                      breaks = breaks, 
                      labels = labels) +
  guides(colour=FALSE) + mytheme
  


#panel.border = element_rect(colour = "grey", fill=NA,size = 1.5),
#panel.grid.major = element_line(colour = "lightgrey", size = 0)) 
p2<-ggplot(data = dat) +
  geom_sf(data=centre, fill = "grey82") +
  geom_sf(data=second, fill = "grey88") +
  geom_sf(data=third, fill = "grey95") +
  geom_sf(size = 2.5, shape = 21, aes(fill=action.late, colour=action.late, stroke = 2)) + 
  coord_sf(xlim = c(-500, 1500), ylim = c(-500, 4500))+
  labs(x = "2km", y = "5km")+
  scale_colour_manual (labels = labs, 
                       values = cols,
                       limits = limits) + 
  scale_fill_manual   (limits = limits,
                       values = fills, 
                       breaks = breaks, 
                       labels = labels) + 
  guides(colour=FALSE) + mytheme
p2
grid.arrange(p1,p2,ncol=1,nrow=2)



                      



