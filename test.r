library(sf)           
library(terra)          
library(RColorBrewer) 
library(ggplot2)      
library(reshape2)
library(raster)
library(dismo)

case = 8
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
  # holds coords of placement; acts as home-range "centroid"
  x.init <- x
  y.init <- y
  I<-sample(1:n_initial, nI)
  status<-rep("S", times=n_initial)
  status[I]<-"I"
  inds <- data.frame(id = id,
                     x=x,
                     y=y,
                     x.init=x.init,
                     y.init=y.init,
                     status=status,
                     time_step=1,
                     stringsAsFactors=FALSE) 
  inds
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
#' #' Chooses best possible landscape component to move to
#' #' TODO alter in the case of an make_decision being fed an empty list, make 
#' #' else case
#' #' TODO implement sorting function
#' #' @param 
#' #' @export
#' make_decision<-function(landscape, nbrs){
#'   # assign decision to be the first element by default--make comparison
#'   decision_vec <- c()
#'     #empty list to hold values of neighbors
#'     weights<-c()
#'     landscape_values<-c()
#'     for(i in 1:length(nbrs)){
#'       # append value of neighbor to weight vector
#'       landscape_values[i]<-landscape[nbrs[[i]][1],][nbrs[[i]][2]]
#'       }
#'     # weights <- landscape_values * distance_vector
#'     weights <- landscape_values
#'     # weighted sample is a vector of the indices of nbrs weighted by
#'     # their corresponding values in the landscape matrix
#'     weighted_sample<-sample(c(1:length(nbrs)), size=100, replace=TRUE, prob=weights)
#'     print(weighted_sample)
#'     # print(weighted_sample)
#'     # now we want to randomly sample from this list to get the index
#'     decision_val <- sample(weighted_sample, 1)
#'     decision_vec <- c(nbrs[[decision_val]][1], nbrs[[decision_val]][2])
#'   decision_vec
#' }


makeDecision<-function(lc, lc.agg, nbrs){
  max.var<-0
  max.var.cells<-NULL
  print(length(nbrs))
  
  for(i in 1:length(nbrs)){
    print(nbrs[i])
    
    ## CODE FOR MAKING NBD Matrix FOR GETTING DISTANCE
    
    
    # nbr.ext<-extentFromCells(lc.agg, nbrs)
    # nbr.crop<-crop(lc.agg, nbr.ext)
    # nbr.crop
    
    nbr.ext<-extentFromCells(lc.agg, nbrs[i])
    # grab all of the cells within the extent of the
    # aggregated cell
    cells<-cellsFromExtent(lc, nbr.ext)
    # want to look at proportions of habitat within
    # each neighboring cell
    
    # first we want to see the proportional makeup of each
    # neighboring cell wrgt land cover
    
    # nbr.cover.types<-unique(lc[cells[1:length(cells)]])
    if(var(lc[cells[1:length(cells)]]) > max.var){
    max.var<-var(lc[cells[1:length(cells)]])
    max.var.cells<-cells
    }
  }
  max.var.ext<-extentFromCells(lc, max.var.cells)
  # return selection based on maximal variation
  cellsFromExtent(lc.agg, max.var.ext)
}

getDistance<-function(ind, nbrs){
  nbr.indx<-c(1:nrow(nbrs + 1))
  dist.from.current<-c()
  dist.from.origin<-c()
  for(i in 1:nrow(nbrs)){
    print(nbrs[i])
    to<-xyFromCell(lc.agg, nbrs[i])
    dist.from.current[i]<-pointDistance(c(ind$x, ind$y), c(to[1], to[2]), lonlat=FALSE)
    dist.from.origin[i]<-pointDistance(c(ind$x.init, ind$y.init), c(to[1], to[2]), lonlat=FALSE)
    dist.from.current[nrow(nbrs)]<-1
    dist.from.origin[nrow(nbrs)]<-1
    
    }
  dist.df<-data.frame(nbr.indx=nbr.indx,
                      dist.from.current=dist.from.current,
                      dist.from.origin=dist.from.origin)
  dist.df[dist.df==0]<-1
  dist.df
}

#' Updates individual locations
#' @param data_frame holds data about deer
#' @export
move<-function(inds, lc, lc.agg){
  # for(i in 1:nrow(data_frame)){
  #   nbrs<-get_neighbors(c(data_frame[i,]$x,data_frame[i,]$y), nrow, ncol)
  #   # distance_vector<-unlist(lapply(get_distance_vector(c(data_frame[i,]$x,data_frame[i,]$y), nbrs), get_inverse))
  #   new_loc<-make_decision(landscape=landscape, nbrs=nbrs)
  #   data_frame[i,]$x<-new_loc[[1]]
  #   data_frame[i,]$y<-new_loc[[2]]
  # }
  
  # grab the adjacent tiles
  selection<-makeDecision(lc, lc.agg, nbrs)
  
  print(selection)
  xyFromCell(lc.agg, selection)
}


getNeighbors<-function(lc.agg, ind, case){
  adjacent(lc.agg,
           cellFromXY(lc.agg, c(ind$x, ind$y)),
           directions=case,
           pairs=FALSE)
}

lc<-raster("C:\\Users\\jackx\\OneDrive\\Desktop\\cwd-project\\tcma_lc_finalv1\\tcma_1000_by_1000_croppped.tif")
# arbitrarily setting 25 as our aggregation factor
lc.agg<-aggregate(lc, 10)

inds<-make_inds(5,
          xmin(lc.agg),
          xmax(lc.agg),
          ymin(lc.agg),
          ymax(lc.agg),
          1)
write.csv(inds, "C:\\Users\\jackx\\Desktop\\deerdat.csv", row.names=FALSE)

for(i in 1:20){
  for(i in 1:nrow(inds)){
  nbrs<-getNeighbors(lc.agg, inds[i,], case)
  nbrs
  dist.df<-getDistance(inds[i,], nbrs)
  print(dist.df)
  new.coord<-move(inds[i,], lc, lc.agg)
  inds[i,]$x <- new.coord[1]
  inds[i,]$y <- new.coord[2]
  new.coord
  inds[i,]$time_step = inds[1,]$time_step+1
  print(inds)
  write.table(inds[i,],  "C:\\Users\\jackx\\Desktop\\deerdat.csv",
              row.names=FALSE, sep=",", append=TRUE, col.names=FALSE,
  )
  }
}

data<-read.csv("C:\\Users\\jackx\\Desktop\\deerdat.csv")
lc.pts <- rasterToPoints(lc.agg, spatial = TRUE)
lc.df  <- data.frame(lc.pts)
# set our column names to be something a bit more descriptive
colnames(lc.df)<-c("cover_type", "x", "y", "optional")
ggplot(data=lc.df, aes(x=x, y=y)) + 
  geom_raster(aes(fill=cover_type)) +
  geom_point(data = data, aes(x = x, y = y, color = status, group=id))+
  geom_path(data = data, aes(x = x, y = y, color = status, group=id))
