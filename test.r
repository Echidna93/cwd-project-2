library(sf)           
library(terra)          
library(RColorBrewer) 
library(ggplot2)      
library(reshape2)
library(raster)
library(dismo)
library
case = 8


# TODO figure out a way to take these values in 
# programatically and sort them based on some
# known 

ls.val<-c(1:12)
ls.def<-c("grass",
          "bare soil",
          "buildings",
          "roads",
          "lake", 
          "deciduous canopy",
          "coniferous canopy",
          "agriculture", 
          "emergent wetland",
          "forested wetland",
          "river",
          "extraction")

ls.types<-data.frame(val=ls.val, def=ls.def
)

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
  # randomly generate starting position until
  x<-c()
  y<-c()
  cell.init<-c()
  cell.temp<-NA
  for(i in 1:n.initial){
    repeat{
      x.temp<-round(runif(1, min=min.x.dim, max=max.x.dim))
      y.temp<-round(runif(1, min=min.y.dim, max=max.y.dim))
      cell.temp<-cellFromXY(lc.agg, c(x.temp, y.temp))
      tmp.val<-lc.agg[cell.temp]
      # need to make sure we're not placing any individuals in water elements
      if(!(tmp.val == 5 || tmp.val == 11 ||
           tmp.val == 3 || is.na(tmp.val))){
        x[i]<-x.temp
        y[i]<-y.temp
        cell.init[i]<-cell.temp
        print(lc.agg[cell.temp])
        break
      }
    }
  }
  # holds coords of placement; acts as home-range "centroid"
  x.init <- x
  y.init <- y
  is.thirsty <- rep(c(FALSE), n.initial)
  last.drink <- rep(c(0), n.initial)
  thirst.threshold<-rep(c(1:6), n.initial)
  I<-sample(1:n.initial, nI)
  status<-rep("S", times=n.initial)
  status[I]<-"I"
  data.frame(id = id,
             x=x,
             y=y,
             crnt.cell=cell.init,
             x.init=x.init,
             y.init=y.init,
             cell.init=cell.init,
             status=status,
             last.drink=last.drink,
             is.thirsty=is.thirsty,
             thirst.threshold=thirst.threshold,
             time_step=1,
             stringsAsFactors=FALSE)
}

getLandscapeMetric<-function(nbr){

}

getClosestElementIndex<-function(nbrs, selection){
  which(abs(nbrs - selection) == min(abs(nbrs - selection)))
}


makeDecision<-function(ind, lc.agg, lc){
  selection<-NA
  nbrs<-getNeighbors(ind, lc.agg)
  if(ind$is.thirsty){
    lc.water<-lc.agg == 5 # grab water on landscape
    lc.water[lc.water==0]<-NA # replace everything that isn't water with NA
    dist.to.water<-distance(lc.water)
    if(dist.to.water[ind$crnt.cell]<=20){
      ind$is.thirsty<-FALSE
      ind$last.drink<-0
      lc.hr<-lc.agg==ind$cell.init
      dir.to.hr<-lc.hr[ind$crnt.cell]
      selection<-ind$cell.init + dir.to.hr
      print(paste("this is selection: ", selection, sep=""))
      selection.index<-getClosestElementIndex(nbrs, selection)
      weights<-rep.int(1,length(nbrs))
      weights[selection.index]<-2
      selection<-sample(sample(c(1:length(nbrs)),size=100,replace=TRUE, prob=weights),1)
      selection<-nbrs[selection]
    }
    else{
      dir.to.water<-direction(lc.water)
      selection<-ind$crnt.cell + dir.to.water[ind$crnt.cell]
      print(paste("this is selection: ", selection, sep=""))
      selection.index<-getClosestElementIndex(nbrs, selection)
      weights<-rep.int(1,length(nbrs))
      weights[selection.index]<-2
      selection<-sample(sample(c(1:length(nbrs)),size=100,replace=TRUE, prob=weights),1)
      selection<-nbrs[selection]
    }
    # if(selection %in% nbrs){
    #   print(paste("selection number: ", selection, "in: ", nbrs, sep = " "))
    #   print( selection)
    # }
    # else{
    #   print(paste("selection number: ", selection, "not in: ", nbrs, sep = " "))
    #   print( selection)
    # }
  }
  else{
  weights<-c()
  landscape.vec<-c()
  # check if centroid is in the nbrs list
  if(!ind$cell.init %in% nbrs){
      nbrs[length(nbrs) + 1] <- ind$cell.init
  }
  nbrs.ext<-extentFromCells(lc.agg, nbrs)
  nbrs<-cellsFromExtent(lc.agg, nbrs.ext)
  nbrs<-nbrs[!is.na(nbrs)]# make sure we aren't on an edge
  nbrs.ext<-extentFromCells(lc.agg,nbrs)
  nbr.crop<-crop(lc.agg, nbrs.ext)
  nbr.crop.dis<-as.array(1/distanceFromPoints(nbr.crop, c(ind$x.init, ind$y.init)))
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
  weights<-landscape.vec*nbr.crop.dis
  weighted.sel<-sample(sample(c(1:length(nbrs)), size=100, replace=TRUE, prob=weights),1)
  selection<-nbrs[weighted.sel]
  }
  selection
}

#' Updates individual locations
#' @param data_frame holds data about deer
#' @export
move<-function(ind, lc.agg, lc){
  makeDecision(ind, lc.agg, lc)
}


updateInfectionRast<-function(ind, lc.stack){

  }

getNeighbors<-function(ind, lc.agg, case = 8){
  adjacent(lc.agg,
           ind$crnt.cell,
           directions=case,
           pairs=FALSE)
}

lc<-raster("C:\\Users\\jackx\\OneDrive\\Desktop\\cwd-project\\tcma_lc_finalv1\\tcma_1000_by_1000_croppped.tif")
# arbitrarily setting 25 as our aggregation factor
lc.agg<-aggregate(lc, 10)
# make infection matrix
infctn<-lc.agg
# need to set all values to zero
infctn[1:length(infctn)]<-0
lc.stck<-stack(lc.agg, infctn)
inds<-make_inds(3,
          xmin(lc.agg),
          xmax(lc.agg),
          ymin(lc.agg),
          ymax(lc.agg),
          1,
          lc.agg)
write.csv(inds, "C:\\Users\\jackx\\Desktop\\deerdat.csv", row.names=FALSE)
for(t in 1:20){
  for(i in 1:nrow(inds)){
    thirst.weights<-c(inds[i,]$thirst.threshold, inds[i,]$last.drink)
    inds[i,]$is.thirsty<-sample(sample(c(FALSE, TRUE), size=100, replace=TRUE, prob=thirst.weights),1)
    if(inds[i,]$status=="I"){
      lc.stck[[2]][inds[i,]$crnt.cell] <- lc.stck[[2]][inds[i,]$crnt.cell] + 1
    # check if the current cell is occupied by another individual
    for(j in 1:nrow(inds)){

      if((inds[i,]$crnt.cell == inds[j,]$crnt.cell) & !(inds[i,]$id == inds[j,]$id)){
        inds[j,]$status = "I"
      }
    }
  }
  inds[i,]$crnt.cell<-move(inds[i,], lc.stck[[1]], lc) # grab the new cell
  print(paste("this is current cell:", inds[i,]$crnt.cell, sep=" "))
  new.coord<-xyFromCell(lc.agg, inds[i,]$crnt.cell)
  inds[i,]$x <- new.coord[1]
  inds[i,]$y <- new.coord[2]
  inds[i,]$last.drink=inds[i,]$last.drink+1
  inds[i,]$time_step = inds[1,]$time_step+1
  }
  write.table(inds,  "C:\\Users\\jackx\\Desktop\\deerdat.csv",
              row.names=FALSE, sep=",", append=TRUE, col.names=FALSE)
  #print(inds)
  
}
data<-read.csv("C:\\Users\\jackx\\Desktop\\deerdat.csv")
lc.pts <- rasterToPoints(lc.stck[[1]], spatial = TRUE)
lc.df  <- data.frame(lc.pts)
# set our column names to be something a bit more descriptive
colnames(lc.df)<-c("cover_type", "x", "y", "optional")
lc.plot<-ggplot(data=lc.df, aes(x=x, y=y)) +
  geom_raster(aes(fill=cover_type)) +
  geom_point(data = data, aes(x = x, y = y, color = status, group=id))+
  geom_path(data = data, aes(x = x, y = y, color = status, group=id))
lc.plot
# set our column names to be something a bit more descriptive
# infctn.pts <- rasterToPoints(lc.stck[[2]], spatial = TRUE)
# infctn.df  <- data.frame(infctn.pts)
# colnames(infctn.df)<-c("value", "x", "y", "optional")
# infctn.plot<-ggplot(data=infctn.df, aes(x=x, y=y)) +
#   geom_raster(aes(fill=value))
# infctn.plot