library("rgdal")
library("raster")
library("sf")
library("leafsync")
library("dplyr")
library("ggplot2")
library("stars")
library(spatialEco)
library(spatstat.random)
library(data.table)

nsamp=20 # number of samples we want
point.dist.cond = 20 # want meters between points
utm <- st_crs("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")

# euclidean distance function
eucDistance<-function(P1, P2){
  sqrt((((P2[1]-P1[1])^2)+((P2[2]-P1[2])^2)))
}
# C:\Users\jackx\Desktop\cervid-farm\Deb-Holthaus
# read in raster
deer.mgmt.shp<-st_read("./data/shapefiles/shp_bdry_deer_permit_areas/mn_deer_permit_areas.shp")

# subset into our SE region
cwd.test.mand<-subset(deer.mgmt.shp, DPA == 643 | DPA == 645 | DPA == 646 | DPA == 647 | DPA == 648 | DPA == 649 | DPA == 655)
# now let's break this from a multipolygon to a polygon
plot(cwd.test.mand$DPA)
