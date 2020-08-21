# this codes the area search 
# we assume a 2km x 1km area can be searched in a day, and is equivalent to a 4km x 1km area

yL <- cbind(c(0, 75, 75, 0)     , c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "a")
yR <- cbind(c(925,1000,1000,925), c(0, 0, 2000, 2000)) %>% coords2Polygons(ID = "a")
yT <- cbind(c(75,925,925,75)  ,c(1925, 1925, 2000, 2000)) %>% coords2Polygons(ID = "a")
yB <- cbind(c(75,925,925,75),c(0, 0, 75, 75)) %>% coords2Polygons(ID = "a")

yin1 <- cbind(c(125,275,275,125),c(75, 75, 1875, 1875)) %>% coords2Polygons(ID = "a")
yin2 <- cbind(c(325,475,475,325),c(125, 125, 1925, 1925)) %>% coords2Polygons(ID = "a")
yin3 <- cbind(c(525,675,675,525),c(75, 75, 1875, 1875)) %>% coords2Polygons(ID = "a")
yin4 <- cbind(c(725,875,875,725),c(125, 125, 1925, 1925)) %>% coords2Polygons(ID = "a")

b1 <- cbind(c(75,125,125,75)  ,c(75, 75, 1925, 1925)) %>% coords2Polygons(ID = "b")
b2 <- cbind(c(275,325,325,275),c(75, 75, 1925, 1925)) %>% coords2Polygons(ID = "b")
b3 <- cbind(c(475,525,525,475),c(75, 75, 1925, 1925)) %>% coords2Polygons(ID = "b")
b4 <- cbind(c(675,725,725,675),c(75, 75, 1925, 1925)) %>% coords2Polygons(ID = "b")
b5 <- cbind(c(875,925,925,875),c(75, 75, 1925, 1925)) %>% coords2Polygons(ID = "b")
b6 <- cbind(c(125,275,275,125),c(1875,1875,1925,1925)) %>% coords2Polygons(ID = "b")
b7 <- cbind(c(525,675,675,525),c(1875,1875,1925,1925)) %>% coords2Polygons(ID = "b")
b8 <- cbind(c(325,475,475,325),c(75,75,125,125)) %>% coords2Polygons(ID = "b")
b9 <- cbind(c(725,875,875,725),c(75,75,125,125)) %>% coords2Polygons(ID = "b")

y<-bind(yT,yB,yL,yR,yin1,yin2,yin3,yin4) %>% st_as_sf()
b<-bind(b1,b2,b3,b4,b5,b6,b7,b8,b9) %>% st_as_sf()

det.centre<-0.9
det.second<-0.8
det.third<-0.7

y.det <- det.second + det.third*(1-det.second)
b.det <- det.centre + det.third*(1-det.centre)


area_search<- 
  ggplot() +
  geom_sf(data=b, fill = "darkgrey", colour = NA) +
  geom_sf(data=y, fill = "grey", colour = NA) +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 1900),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 300, y = 100, xend = 300, yend = 1900),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 500, y = 100, xend = 500, yend = 1900),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 700, y = 100, xend = 700, yend = 1900),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 900, y = 100, xend = 900, yend = 1900),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 100, y = 1900, xend = 300, yend = 1900),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 500, y = 1900, xend = 700, yend = 1900),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 300, y = 100, xend = 500, yend = 100),linetype = "dashed", colour = "blue", size=0.05) +
  geom_segment(aes(x = 700, y = 100, xend = 900, yend = 100),linetype = "dashed", colour = "blue", size=0.05) +
  scale_x_continuous(limits = c(-500,1500), breaks = c(-500,0,500,1000,1500)) +
  scale_y_continuous(limits = c(-500,2500), breaks = c(-500,0,500,1000,1500,2000,2500)) +
  theme_minimal()

# the area_search is done with 'b' and 'y' the same 


area_search
transects


