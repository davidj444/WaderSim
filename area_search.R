# this codes the area search 
# we assume a 2km x 1km area can be searched in a day, and is equivalent to a 4km x 1km area
y1 <- cbind(c(0, 50, 50, 0)     , c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "y")
y2 <- cbind(c(950,1000,1000,950), c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "y")

a1 <- cbind(c(50,75,75,50)    ,c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "a")
a2 <- cbind(c(125,275,275,125),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "a")
a3 <- cbind(c(325,475,475,325),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "b")
a4 <- cbind(c(525,675,675,525),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "a")
a5 <- cbind(c(725,875,875,725),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "a")
a6 <- cbind(c(925,950,950,925),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "a")

b1 <- cbind(c(75,125,125,75)  ,c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "b")
b2 <- cbind(c(275,325,325,275),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "b")
b3 <- cbind(c(475,525,525,475),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "b")
b4 <- cbind(c(675,725,725,675),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "b")
b5 <- cbind(c(875,925,925,875),c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "b")

y<-bind(y1,y2)             %>% st_as_sf()
a<-bind(a1,a2,a3,a4,a5,a6) %>% st_as_sf() 
b<-bind(b1,b2,b3,b4,b5)    %>% st_as_sf()
plot(a)
plot(b)
plot(y)

det.centre<-0.9
det.second<-0.8
det.third<-0.7

a.det <- det.second + det.third*(1-det.centre)
b.det <- det.centre + det.third*(1-det.centre)
y.det <- det.second

ggplot()+
  geom_sf(data=a)+
  geom_sf(data=b)+
  geom_sf(data=y)+
  scale_x_continuous(breaks = c(0,500, 1000))+
  theme_minimal()
