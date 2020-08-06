
# This creates the distance bands used in the app

# this specifies the length of the transects in metres. 2000 is a good idea. This may be made reactable.
size<-2000

# create 0-25m bands either side of two transects 500m apart  
c1 <- cbind(c((size/2)-275, (size/2)-225, (size/2)-225, (size/2)-275), c(0, 0, size, size))
c1 <- coords2Polygons(c1, ID ="A")
c2 <- cbind(c((size/2)+225, (size/2)+275, (size/2)+275, (size/2)+225), c(0, 0, size, size))
c2 <- coords2Polygons(c2, ID = "A")

# combine the centre bands of each transect into 1 shapefile
centre<-bind(c1, c2)

# create 25-100m bands either side of two transects 500m apart
s1 <- cbind(c((size/2)-350,(size/2)-275,(size/2)-275,(size/2)-350), c(0, 0, size, size))
s1a <- coords2Polygons(s1, ID = "B")
s1a
s2 <- cbind(c((size/2)-225, (size/2)-150, (size/2)-150, (size/2)-225), c(0, 0, size, size))
s2a <- coords2Polygons(s2, ID = "B")
s3 <- cbind(c((size/2)+150, (size/2)+225, (size/2)+225,(size/2)+150), c(0, 0, size, size))
s3a <- coords2Polygons(s3, ID = "B")
s4 <- cbind(c((size/2)+275, (size/2)+350, (size/2)+350, (size/2)+275), c(0, 0, size, size))
s4a <- coords2Polygons(s4, ID = "B")

# combine the four 25-100m bands into 1 shapefile
merge1<-bind(s1a, s2a)
merge2<-bind(s3a, s4a)
sec<-bind(merge1, merge2)

# create 100-250m bands either side of two transects
th1 <- cbind(c((size/2)-500,(size/2)-350,(size/2)-350,(size/2)-500), c(0, 0, size, size))
th1 <- coords2Polygons(th1, ID = "C")
th2 <- cbind(c((size/2)-150, size/2, size/2,(size/2)-150), c(0, 0, size, size))
th2 <- coords2Polygons(th2, ID = "C")
th3 <- cbind(c((size/2)+150, size/2, size/2, (size/2)+150), c(0, 0, size, size))
th3 <- coords2Polygons(th3, ID = "C")
th4 <- cbind(c((size/2)+350, (size/2)+500, (size/2)+500, (size/2)+350), c(0, 0, size, size))
th4 <- coords2Polygons(th4, ID = "C")

#combine the four 25-100m bands into 1 shapefile
merge3<-bind(th1, th2)
merge4<-bind(th3, th4)
third<-bind(merge3, merge4)

#convert to simple features
second<-st_as_sf(sec)
centre<-st_as_sf(centre)
third<-st_as_sf(third)

