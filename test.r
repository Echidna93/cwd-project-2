library(sf)           
library(terra)          
library(RColorBrewer) 
library(ggplot2)      
library(reshape2)
library(raster)
library(dismo)

# lc=raster::raster("C:\\Users\\jackx\\OneDrive\\Desktop\\Langrange-Movement-R\\tcma_lc_finalv1\\tcma_lc_finalv1.tif")
# # cropped.lc <- crop()
# 
# #rasterToPolygos(lc)
# # pts <- sampleRast(lc, 1000)
# # pts
# # lc
# # hist(lc)
# 
# # plot(lc)
# xmax(lc)
# 
# b <- as(extent(500000, 510000, 5000000, 5010000), 'SpatialPolygons')
# crs(b) <- crs(lc)
# rb <- crop(lc, b)
# 
# plot(rb)
# rb.df<-as.data.frame(rb)
# writeRaster(rb,
#             filename="C:\\Users\\jackx\\OneDrive\\Desktop\\Langrange-Movement-R\\tcma_lc_finalv1\\tcma_10000_by_10000_croppped.tif",
#             overwrite=TRUE)
            

# b <- as(extent(507000, 508000, 5007000, 5008000), 'SpatialPolygons')
# crs(b) <- crs(lc)
# rb <- crop(lc, b)
# writeRaster(rb,
#                         filename="C:\\Users\\jackx\\OneDrive\\Desktop\\Langrange-Movement-R\\tcma_lc_finalv1\\tcma_1000_by_1000_croppped.tif",
#                         overwrite=TRUE)
# plot(rb)
# rb.df<-as.data.frame(rb)

# tclc.df<-as.data.frame(tc.lc.cropped)
# 
# tclc

# lc.df<-as.data.frame(lc)
# lc.df

#' initiates a data frame of deer
#' @param n_initial # number of individuals
#' @param dim dimension of a vector for random assignment of location
#' @param nI # infected individuals to start
#' @export
make_inds <- function(n_initial,
                      min_x_dim,
                      max_x_dim,
                      min_y_dim,
                      max_y_dim,
                      nI){
  id<-1:n_initial
  x<-round(runif(n_initial, min=min_x_dim, max=max_x_dim))
  y<-round(runif(n_initial, min=min_y_dim, max=max_y_dim))
  I<-sample(1:n_initial, nI)
  status<-rep("S", times=n_initial)
  status[I]<-"I"
  inds <- data.frame(id = id,
                     x=x,
                     y=y,
                     status=status,
                     time_step = 1,
                     stringsAsFactors=FALSE) 
  inds
}

# want function that takes in the raster layer
# splits each numeric value into it's own raster
# throws them into a raster stack
# returns the raster stack

make_raster_stack <- function(lc.rast){
  cover_vals <- c()
  cover_vals <- unique(lc.rast)
  for(i in cover_vals){
    
    }
}

lc<-raster("C:\\Users\\jackx\\OneDrive\\Desktop\\cwd-project\\tcma_lc_finalv1\\tcma_1000_by_1000_croppped.tif")
inds<-make_inds(100,
          xmin(lc),
          xmax(lc),
          ymin(lc),
          ymax(lc),
          1)
# note that in order to use ggplot with raster conversion is needed
# first grab the points of the raster
lc.pts <- rasterToPoints(lc, spatial = TRUE)
# now throw those into a data frame
lc.df  <- data.frame(lc.pts)
# set our column names to be something a bit more descriptive
colnames(lc.df)<-c("cover_type", "x", "y", "optional")
ggplot(data=lc.df, aes(x=x, y=y)) + 
  geom_raster(aes(fill=cover_type)) + # tell our aes to fill with raster data types
  geom_point(data=inds, aes(x=x, y=y, color="red"))