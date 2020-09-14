# plot the surveyed area of a 2km x 1km area using an area search approach where every 
# part of the site is approached to 100m

# these are sections that are 25-100m of the observer
yL <- cbind(c(0, 75, 75, 0),      c(0, 0, 2000, 2000))       %>% coords2Polygons(ID = "y")
yR <- cbind(c(925,1000,1000,925), c(0, 0, 2000, 2000))       %>% coords2Polygons(ID = "y")
yT <- cbind(c(75,925,925,75),     c(1925, 1925, 2000, 2000)) %>% coords2Polygons(ID = "y")
yB <- cbind(c(75,925,925,75),     c(0, 0, 75, 75))           %>% coords2Polygons(ID = "y")

yin1 <- cbind(c(125,275,275,125), c(75, 75, 1875, 1875))     %>% coords2Polygons(ID = "y")
yin2 <- cbind(c(325,475,475,325), c(125, 125, 1925, 1925))   %>% coords2Polygons(ID = "y")
yin3 <- cbind(c(525,675,675,525), c(75, 75, 1875, 1875))     %>% coords2Polygons(ID = "y")
yin4 <- cbind(c(725,875,875,725), c(125, 125, 1925, 1925))   %>% coords2Polygons(ID = "y")

# combine sections 25-100m of observer & convert to SF
y<-bind(yT,yB,yL,yR,yin1,yin2,yin3,yin4) %>% st_as_sf()

# these sections are within 25m of the observer 
b1 <- cbind(c(75,125,125,75),     c(75, 75, 1925, 1925))  %>% coords2Polygons(ID = "b")
b2 <- cbind(c(275,325,325,275),   c(75, 75, 1925, 1925))  %>% coords2Polygons(ID = "b")
b3 <- cbind(c(475,525,525,475),   c(75, 75, 1925, 1925))  %>% coords2Polygons(ID = "b")
b4 <- cbind(c(675,725,725,675),   c(75, 75, 1925, 1925))  %>% coords2Polygons(ID = "b")
b5 <- cbind(c(875,925,925,875),   c(75, 75, 1925, 1925))  %>% coords2Polygons(ID = "b")
b6 <- cbind(c(125,275,275,125),   c(1875,1875,1925,1925)) %>% coords2Polygons(ID = "b")
b7 <- cbind(c(525,675,675,525),   c(1875,1875,1925,1925)) %>% coords2Polygons(ID = "b")
b8 <- cbind(c(325,475,475,325),   c(75,75,125,125))       %>% coords2Polygons(ID = "b")
b9 <- cbind(c(725,875,875,725),   c(75,75,125,125))       %>% coords2Polygons(ID = "b")

# combine sections 0-25m of observer & convert to SF
b<-bind(b1,b2,b3,b4,b5,b6,b7,b8,b9) %>% st_as_sf()

# account for overlapping detectabilities caused by the more intensive survey method by 
# adding the 100-250m detectability band
y.det <- det.second + det.third*(1-det.second)
b.det <- det.centre + det.third*(1-det.centre)

## specify items to appear in 'transects' example plot - shades of grey for different detectabilities
## and blue dashed line for survey route
as.line1<-  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 1900), linetype = "dashed", colour = "blue", size=0.05)
as.line2<-  geom_segment(aes(x = 300, y = 100, xend = 300, yend = 1900), linetype = "dashed", colour = "blue", size=0.05)
as.line3<-  geom_segment(aes(x = 500, y = 100, xend = 500, yend = 1900), linetype = "dashed", colour = "blue", size=0.05)
as.line4<-  geom_segment(aes(x = 700, y = 100, xend = 700, yend = 1900), linetype = "dashed", colour = "blue", size=0.05)
as.line5<-  geom_segment(aes(x = 900, y = 100, xend = 900, yend = 1900), linetype = "dashed", colour = "blue", size=0.05)
as.line6<-  geom_segment(aes(x = 100, y = 1900, xend = 300, yend = 1900),linetype = "dashed", colour = "blue", size=0.05)
as.line7<-  geom_segment(aes(x = 500, y = 1900, xend = 700, yend = 1900),linetype = "dashed", colour = "blue", size=0.05)
as.line8<-  geom_segment(aes(x = 300, y = 100, xend = 500, yend = 100),  linetype = "dashed", colour = "blue", size=0.05)
as.line9<-  geom_segment(aes(x = 700, y = 100, xend = 900, yend = 100),  linetype = "dashed", colour = "blue", size=0.05)
as.labs<-labs(x = "2km", y = "3km") 
as.scalex<-  scale_x_continuous(limits = c(-500,1500))
as.scaley<-  scale_y_continuous(limits = c(-500,2500)) 
as.band1<-  geom_sf(data=b, fill = "darkgray", colour = NA)
as.band2<-geom_sf(data=y, fill = "grey", colour = NA)

# this makes 'area_search' a ggplot object that can be called e.g. in RShiny
area_search <-
  ggplot() +as.band1 +as.band2 +
  as.line1 + as.line2 + as.line3 + as.line4 + as.line5 + as.line6 + as.line7 + as.line8 + as.line9 +
  as.scalex + as.scaley  + mytheme + arrow + as.labs
  
area_search

ggplot()+arrow


