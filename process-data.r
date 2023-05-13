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
library(geoR)
library(RColorBrewer)
library(spdep)
library(spatialreg)
library(classInt)
nsamp=20 # number of samples we want
point.dist.cond = 20 # want meters between points
utm <- st_crs("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")

# euclidean distance function
# eucDistance<-function(P1, P2){
#   sqrt((((P2[1]-P1[1])^2)+((P2[2]-P1[2])^2)))
# }
# read in data
# cwd.dat<-read.table(
#   "./data/cwd-plus-2010-2023.csv",
#   sep=",", header=TRUE)
# deer.hrvst.dat <- read.table("./data/deer-harvest-master.csv",
#                              sep=",",header=TRUE)

cwd.dat <- subset(cwd.dat, sample_acquisition == "Hunter harvested")
# read in raster
deer.hrvst.shp<-st_read("./data/shapefiles/shp_env_mn_deer_harvest/deer_harvest_2014.shp")
deer.mgmt.shp<-st_read("./data/shapefiles/shp_bdry_deer_permit_areas/mn_deer_permit_areas.shp")
trs.shp<-st_read("./data/shapefiles/shp_plan_mngeo_trs/trs.shp")

# subset into our SE region
cwd.test.mand<-subset(deer.mgmt.shp, DPA == 643 | DPA == 645 | DPA == 646 |
                        DPA == 647 | DPA == 648 | DPA == 649 | DPA == 655)

# now let's break this from a multipolygon to a polygon
trs.cwd.mgmt.shp <-subset(trs.shp, COUN_UC == "FILLMORE" | COUN_UC == "HOUSTON" |
                            COUN_UC == "OLMSTED" | COUN_UC == "WINONA")
plot(trs.cwd.mgmt.shp)
# first we want to make sure there aren't any capital letters in our headers
for( i in 1:length(names(cwd.dat))){
  names(cwd.dat)[i] <- tolower(names(cwd.dat)[i])
}
# subset to 2016 on
# years when we have mandatory sampling
# next let's get rid of capital letters in the data itself

cwd.dat <- subset(cwd.dat, permit_area == 643 | permit_area == 645 | permit_area == 646 |
         permit_area == 647 | permit_area == 648 | permit_area == 649 |
         permit_area == 655)


# grab our coordinates from
fips.coun <- c()
for( i in 1:nrow(cwd.dat)){
  if(cwd.dat[i,]$county == 'Fillmore'){
    fips.coun[i] <- 45
  }
  if(cwd.dat[i,]$county == 'Winona'){
    fips.coun[i] <- 169
  }
  if(cwd.dat[i,]$county == 'Olmsted'){
    fips.coun[i] <- 109
  }
  if(cwd.dat[i,]$county == 'Houston'){
    fips.coun[i] <- 55
  }
}
cwd.dat$fips_coun <- fips.coun

deer.hrvst.trs.cwd.mgmt.shp <-st_intersection(deer.hrvst.shp,trs.cwd.mgmt.shp)
deer.spat<-as(deer.hrvst.trs.cwd.mgmt.shp, "Spatial")
deer.spat.coords<-coordinates(deer.spat)
deer.hrvst.trs.cwd.mgmt.shp$cwd.pos <- c(0)
for(i in 1:nrow(deer.hrvst.trs.cwd.mgmt.shp)){
  for(j in 1:nrow(cwd.dat)){
  if(cwd.dat[j,]$town == deer.hrvst.trs.cwd.mgmt.shp[i,]$TOWN &
     cwd.dat[j,]$section == deer.hrvst.trs.cwd.mgmt.shp[i,]$SECT &
     cwd.dat[j,]$range == deer.hrvst.trs.cwd.mgmt.shp[i,]$RANG){
     deer.hrvst.trs.cwd.mgmt.shp[i,]$cwd.pos = deer.hrvst.trs.cwd.mgmt.shp[i,]$cwd.pos + 1
  }
  }
}

ggplot(deer.hrvst.trs.cwd.mgmt.shp$geometry, aes(fill=deer.hrvst.trs.cwd.mgmt.shp$cwd.pos)) +
  geom_sf() +
  guides(fill=guide_legend(title="cwd positive"), color=guide_legend(show=FALSE)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank())

# # now we can start applying a sar model
# deer.knn <- knearneigh(deer.spat.coords)
# deer.knn2nb = knn2nb(deer.knn)
# deer.list = nbdists(deer.knn2nb, deer.spat.coords)
# deer.dist.vec <- unlist(deer.list)
# upper.bound.75<-0.75*max(deer.dist.vec)
# deer.dnn.nb.75<-dnearneigh(deer.spat.coords, d1=0, d2=upper.bound.75)
# deer.dnn.listw.75 <- nb2listw(deer.dnn.nb.75, style="B", zero.policy=TRUE)
# deer.dnn.car.out.75 <- spautolm(deer.hrvst.trs.cwd.mgmt.shp$cwd.pos~deer.hrvst.trs.cwd.mgmt.shp$BUCKSQMILE,
#                                 data=deer.hrvst.trs.cwd.mgmt.shp$geometry, family="CAR", listw=deer.dnn.listw.75, zero.policy = TRUE)
# deer.dnn.car.fitted.75 = fitted(deer.dnn.car.out.75)
# 
# deer.hrvst.trs.cwd.mgmt.shp$fitted.car <- deer.dnn.car.fitted.75
# brks = seq(0.0005, 0.2, 0.0005)
# color.pallete = rev(brewer.pal(length(brks),"RdBu"))
# # create 75% of max dist breaks
# class.fitted.car.75 = classIntervals(var=deer.hrvst.trs.cwd.mgmt.shp$fitted.car, n=length(brks), style="fixed", fixedBreaks=brks, dataPrecision=4)
# color.code.fitted.car.75 = findColours(class.fitted.car.75, color.pallete)
# 
# ggplot(deer.hrvst.trs.cwd.mgmt.shp$geometry, aes(fill=deer.hrvst.trs.cwd.mgmt.shp$fitted.car)) +
#   geom_sf() +
#   guides(fill=guide_legend(title="cwd positive"), color=guide_legend(show=FALSE)) +
#   theme(panel.grid=element_blank(),
#         panel.background = element_blank())
# 
# plot(deer.hrvst.trs.cwd.mgmt.shp$geometry, col=color.code.fitted.car.75)
