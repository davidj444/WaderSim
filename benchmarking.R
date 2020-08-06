######microbench
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