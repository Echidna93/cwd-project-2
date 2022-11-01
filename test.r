library(sf)           
library(terra)          
library(RColorBrewer) 
library(ggplot2)      
library(reshape2)
library(raster)
library(dismo)
library
case = 8

# b <- as(extent(507000, 508000, 5007000, 5008000), 'SpatialPolygons')

#' initiates a data frame of deer
#' @param n_initial # number of individuals
#' @param dim dimension of a vector for random assignment of location
#' @param nI # infected individuals to 
#' @param lc.agg # aggregated landscape 
#' @export
make_inds <- function(n.initial,
                      min.x.dim,
                      max.x.dim,
                      min.y.dim,
                      max.y.dim,
                      nI,
                      lc.agg){
  id<-1:n.initial
  x<-round(runif(n.initial, min=min.x.dim, max=max.x.dim))
  y<-round(runif(n.initial, min=min.y.dim, max=max.y.dim))
  # holds coords of placement; acts as home-range "centroid"
  x.init <- x
  y.init <- y
  I<-sample(1:n.initial, nI)
  status<-rep("S", times=n.initial)
  status[I]<-"I"
  cell.init<-c()
  for(i in 1:length(x)){
    cell.init[i]<-cellFromXY(lc.agg, c(x[i], y[i]))
  }
  inds <- data.frame(id = id,
                     x=x,
                     y=y,
                     crnt.cell=cell.init,
                     x.init=x.init,
                     y.init=y.init,
                     cell.init=cell.init,
                     status=status,
                     time_step=1,
                     stringsAsFactors=FALSE) 
  inds
}

lc<-raster("C:\\Users\\jackx\\OneDrive\\Desktop\\cwd-project\\tcma_lc_finalv1\\tcma_1000_by_1000_croppped.tif")
makeDecision<-function(lc, lc.agg, ind){
  max.var<-0
  max.var.cells<-NULL
  nbrs<-getNeighbors(ind, lc.agg)
  print(length(nbrs))
  weights<-c()
  landscape.vec<-c()
  # check if centroid is in the nbrs list
  if(!ind$cell.init %in% nbrs){
      nbrs[length(nbrs) + 1] <- ind$cell.init
  }
  nbrs.ext<-extentFromCells(lc.agg, nbrs)
  nbrs<-cellsFromExtent(lc.agg, nbrs.ext)
  nbrs.ext<-extentFromCells(lc.agg,nbrs)
  nbr.crop<-crop(lc.agg, nbrs.ext)
  nbr.crop.dis<-as.array(1/distanceFromPoints(nbr.crop, c(ind$x.init, ind$y.init)))
  print(length(nbr.crop.dis))
  for(i in 1:length(nbrs)){
    ## CODE FOR MAKING NBD Matrix FOR GETTING DISTANCE
    
    # in the case where the initial point (home range centroid)
    # is outside of the visualised movement area
    # we need to tack the centroid into the neighbors vector
    # probably should consider having this be 
    
   
    
    # in the neighbor crop we want to get the distances in the raster q.3214
    
    
    nbr.ext<-extentFromCells(lc.agg, nbrs[i])
    # grab all of the cells within the extent of the
    # aggregated cell
    cells<-cellsFromExtent(lc, nbr.ext)
    # want to look at proportions of habitat within
    # each neighboring cell
    
    # first we want to see the proportional makeup of each
    # neighboring cell wrgt land cover
    
    # nbr.cover.types<-unique(lc[cells[1:length(cells)]])
    landscape.vec[i]<-mean(lc[cells[1:length(cells)]])
  }
  print(length(landscape.vec))
  weights<-landscape.vec*nbr.crop.dis
  weighted.sel<-sample(sample(c(1:length(nbrs)), size=100, replace=TRUE, prob=weights),1)
  nbrs[weighted.sel]
}

#' Updates individual locations
#' @param data_frame holds data about deer
#' @export
move<-function(ind, lc, lc.agg){
  selection<-makeDecision(lc, lc.agg, ind)
  xyFromCell(lc.agg, selection)
}


getNeighbors<-function(ind, lc.agg, case = 8){
  adjacent(lc.agg,
           cellFromXY(lc.agg, c(ind$x, ind$y)),
           directions=case,
           pairs=FALSE)
}

lc<-raster("C:\\Users\\jackx\\OneDrive\\Desktop\\cwd-project\\tcma_lc_finalv1\\tcma_1000_by_1000_croppped.tif")
# arbitrarily setting 25 as our aggregation factor
lc.agg<-aggregate(lc, 10)

# inds<-make_inds(5,
#           xmin(lc.agg),
#           xmax(lc.agg),
#           ymin(lc.agg),
#           ymax(lc.agg),
#           1,
#           lc.agg)
# write.csv(inds, "C:\\Users\\jackx\\Desktop\\deerdat.csv", row.names=FALSE)
# 
# for(i in 1:20){
#   for(i in 1:nrow(inds)){
#   new.coord<-move(inds[i,], lc, lc.agg)
#   inds[i,]$x <- new.coord[1]
#   inds[i,]$y <- new.coord[2]
#   new.coord
#   inds[i,]$time_step = inds[1,]$time_step+1
#   print(inds)
#   write.table(inds[i,],  "C:\\Users\\jackx\\Desktop\\deerdat.csv",
#               row.names=FALSE, sep=",", append=TRUE, col.names=FALSE,
#   )
#   }
# }

data<-read.csv("C:\\Users\\jackx\\Desktop\\deerdat.csv")
lc.pts <- rasterToPoints(lc.agg, spatial = TRUE)
lc.df  <- data.frame(lc.pts)
# set our column names to be something a bit more descriptive
colnames(lc.df)<-c("cover_type", "x", "y", "optional")
ggplot(data=lc.df, aes(x=x, y=y)) + 
  geom_raster(aes(fill=cover_type)) +
  geom_point(data = data, aes(x = x, y = y, color = status, group=id))+
  geom_path(data = data, aes(x = x, y = y, color = status, group=id))
