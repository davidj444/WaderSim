
# This creates the distance bands used in the transects approach
det.centre<-0.9
det.second<-0.8
det.third<-0.7
# this specifies the length of the transects in metres. 2000 is a good idea. This may be made reactable.
size<-1000

# create 0-25m bands either side of two transects 500m apart  
c1 <- cbind(c((size/2)-275, (size/2)-225, (size/2)-225, (size/2)-275), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID ="c")
c2 <- cbind(c((size/2)+225, (size/2)+275, (size/2)+275, (size/2)+225), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID ="c")

# combine the centre bands of each transect into 1 shapefile
centre<-bind(c1, c2) %>% st_as_sf()

# create 25-100m bands either side of two transects 500m apart
s1 <- cbind(c((size/2)-350,(size/2)-275,(size/2)-275,(size/2)-350), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")
s2 <- cbind(c((size/2)-225, (size/2)-150, (size/2)-150, (size/2)-225), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")
s3 <- cbind(c((size/2)+150, (size/2)+225, (size/2)+225,(size/2)+150), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")
s4 <- cbind(c((size/2)+275, (size/2)+350, (size/2)+350, (size/2)+275), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")

# combine the four 25-100m bands into 1 shapefile and convert to SF
sec<-bind(s1,s2,s3,s4) %>% st_as_sf()

# create 100-250m bands either side of two transects
th1<-cbind(c((size/2)-500,(size/2)-350,(size/2)-350,(size/2)-500),    c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")
th2<-cbind(c((size/2)-150, size/2, size/2,(size/2)-150),              c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")
th3<-cbind(c((size/2)+150, size/2, size/2, (size/2)+150),             c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")
th4<-cbind(c((size/2)+350, (size/2)+500, (size/2)+500, (size/2)+350), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")

#combine the four 25-100m bands into 1 shapefile and convert to SF
third<-bind(th1, th2, th3, th4) %>% st_as_sf()

#plot bands
transects<- 
  ggplot()+
  geom_sf(data=centre, fill = "darkgrey", colour = NA) +
  geom_sf(data=sec, fill = "grey", colour = NA) +
  geom_sf(data=third, fill = "lightgrey", colour = NA) +
  geom_segment(aes(x = 250, y = 0, xend = 250, yend = 4000),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 750, y = 0, xend = 750, yend = 4000),linetype = "dashed", colour = "blue", size=0.05) +
  scale_x_continuous(limits = c(-500,1500), breaks = c(-500,0,500,1000,1500,2000)) +
  scale_y_continuous(limits = c(-500,4500), breaks = c(-500,0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000)) +
  scale_linetype_manual("dashed",values=c("dashed"=1)) +
 legend.title=element_blank() +
  theme_minimal()


plot.ters <- function(n,x0,x1,y0,y1,d,trials = 3000){
  for(i in 1:trials){
    t <- cbind(runif(n,x0,x1),runif(n,y0,y1))
    if(min(dist(t)) >= d) return(t)
  }
  return(NA) 
}

plot.ters(30,-500,2500,-500,2500,30)
