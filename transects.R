# assume two transects 4km long, 500m apart
# plot areas 0-25m, 25-100m, and 100-250m from the transect

# create 0-25m bands  
c1 <- cbind(c(225, 275, 275, 225), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID ="c")
c2 <- cbind(c(725, 775, 775, 725), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID ="c")

# plot 25-100m bands 
s1 <- cbind(c(150, 225, 225, 150), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")
s2 <- cbind(c(275, 350, 350, 275), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")
s3 <- cbind(c(650, 725, 725, 650), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")
s4 <- cbind(c(775, 850, 850, 775), c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "s")

# plot 100-250m bands 
th1<-cbind(c(0,150,150,0),         c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")
th2<-cbind(c(350,500,500,350),     c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")
th3<-cbind(c(650,500,500,650),     c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")
th4<-cbind(c(850,1000,1000,850),   c(0, 0, 4000, 4000)) %>% coords2Polygons(ID = "th")

# combine 0-25m bands together and convert to SF
centre<-bind(c1, c2) %>% st_as_sf()
centre
# combine 25-100m bands and convert to SF
second<-bind(s1,s2,s3,s4) %>% st_as_sf()

#combine 100-250m bands and convert to SF
third<-bind(th1, th2, th3, th4) %>% st_as_sf()

## specify items to appear in 'transects' example plot - shades of grey for different detectabilities
## and blue dashed line for survey route

tr.band1<-geom_sf(data=centre, fill = "darkgray", colour = NA)
tr.band2<-geom_sf(data=second, fill = "grey", colour = NA)
tr.band3<-geom_sf(data=third, fill = "lightgrey", colour = NA)
tr.line1<-geom_segment(aes(x = 250, y = 0, xend = 250, yend = 4000),linetype = "dashed", colour = "blue", size=0.05) 
tr.line2<-  geom_segment(aes(x = 750, y = 0, xend = 750, yend = 4000),linetype = "dashed", colour = "blue", size=0.05)
tr.scalex<- scale_x_continuous(limits = c(-500,1500))
tr.scaley<- scale_y_continuous(limits = c(-500,4500))
tr.labs<-    labs(x = "2km", y = "5km")  


# make the transects example plot
transects <- 
  ggplot()+ 
  tr.band1 + tr.band2 + tr.band3 + tr.line1 + tr.line2 + tr.scalex + tr.scaley + tr.labs + mytheme


            
