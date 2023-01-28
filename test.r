library(sf)           
library(terra)          
library(RColorBrewer) 
library(ggplot2)      
library(reshape2)
library(raster)
library(dismo)
library(sf)
library(doParallel)
library(foreach)
case = 8
# TODO figure out a way to take these values in 
# programatically and sort them based on some
# known 
ls.val<-c(1:12)
# CONSTANTS
# TODO look into making these changeable by a user in an x11() window?
gamma=0.1 # recovery rate
percep<-2
earth.radius=6371
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
#' initiates a data frame of deer
#' @param n_initial # number of individuals
#' @param dim dimension of a vector for random assignment of location
#' @param nI # infected individuals to 
#' @param lc.agg # aggregated landscape 
#' @export
make_inds <- function(n.initial,
                      x.dim,
                      y.dim,
                      nI,
                      lc.agg){
  id<-1:n.initial
  # randomly generate starting position until
  x.coord.init<-c()
  y.coord.init<-c()
  x.cell.init<-c()
  y.cell.init<-c()
  for(i in 1:n.initial){
    repeat{
      x.tmp<-round(runif(1, min=1, max=x.dim))
      y.tmp<-round(runif(1, min=1, max=y.dim))
      tmp.val<-lc.agg[x.tmp,][y.tmp]
      # need to make sure we're not placing any individuals in water elements
      if(!(tmp.val == 5 || tmp.val == 11 ||
           tmp.val == 3 || is.na(tmp.val))){
        x.coord.init[i]<-xFromCol(lc.agg,x.tmp)
        y.coord.init[i]<-yFromRow(lc.agg,y.tmp)
        x.cell.init[i]<-x.tmp
        y.cell.init[i]<-y.tmp
        break
      }
    }
  }
  # holds coords of placement; acts as home-range "centroid"
  I<-sample(1:n.initial, nI) 
  status<-rep("S", times=n.initial)
  status[I]<-"I"
  data.frame(id = id,
             x.cell.init=x.cell.init,
             y.cell.init=y.cell.init,
             x.coord.init=x.coord.init,
             y.coord.init=y.coord.init,
             x.coord=x.coord.init,
             y.coord=y.coord.init,
             x.cell=x.cell.init,
             y.cell=y.cell.init,
             status=status,
             time_step=1,
             stringsAsFactors=FALSE)
}

getLandscapeMetric<-function(nbr){

}

getClosestElementIndex<-function(nbrs, selection){
  which(abs(nbrs - selection) == min(abs(nbrs - selection)))
}

#' Helper function
#' want it to return a list of infected deer
#' @param data_frame holds data about deer
#' @exportk*
get_infected_deer<-function(data_frame){
  # iterator for empty infected_inds array
  j<-1
  infected_inds<-rep()
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status == "I"){
      infected_inds[[j]]<-c(data_frame[i,]$id,
                            data_frame[i,]$x,
                            data_frame[i,]$y
      )
      j<-j+1
    }
  }
  infected_inds
}

# TODO use the single parameter used in white's code for the location of the deer

#' Updates individual values within dataframe
#' @param data_frame holds data about deer
#' @param infection_matrix holds data about current infectivity levels of landscape
#' @param infectivity_threshold some amount of disease where infection automatically happens (definitely subject to change)
#' @export
update_infection_statuses<-function(data_frame, infection_matrix, infectivity_threshold){
  inf_inds<-get_infected_deer(data_frame)
  # TODO break this off into it's own function, maybe is_same_location
  for(i in 1:nrow(data_frame)){
    # increment time step; used for animation (if desired)
    # TODO investigate breaking this off into it's own helper function
    data_frame[i,]$time_step = data_frame[i,]$time_step + 1
    if(data_frame[i,]$status=="S"){
      if(is_cell_inf_val_above_threshold(infection_matrix[data_frame[i,]$xloc + data_frame[i,]$yloc],infectivity_threshold)){
        data_frame[i,]$status = "I"
      }
      else{
        for(j in 1:length(inf_inds)) {
          if(data_frame[i,]$xloc==inf_inds[[j]][2] & 
             data_frame[i,]$yloc==inf_inds[[j]][3] &
             !(data_frame[i,]$id==inf_inds[[j]][1])){
            data_frame[i,]$status = "I"
          }
        }
      }
    }
  }
  data_frame
}

#' Helper function
#' Determines if current cell value is above acceptable "infection" threshold
#' @param cell_value current cell coordinate(s)
#' @param infection_matrix holds data about current infectivity levels of landscape
#' @param infectivity_threshold some amount of disease where infection automatically happens (definitely subject to change)
#' @export
is_cell_inf_val_above_threshold<-function(cell_value, infectivity_threshold){
  cell_value>=infectivity_threshold
}

#' Updates the matrix that holds infection levels for the landscape
#' @param data_frame holds data about deer
#' @param inf_matrix holds data about current infectivity levels of landscape
#' @export
update_infection_matrix<-function(inf_matrix, data_frame){
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status == "I")
      inf_matrix[data_frame[i,]$xloc, data_frame[i,]$yloc]<-inf_matrix[data_frame[i,]$xloc,data_frame[i,]$yloc] + 1
  }
  max_val<-max(inf_matrix)
  apply(inf_matrix, 2, divide_by_max, max=max_val)
}

# helper function
# divides by max value of matrix
divide_by_max<-function(x, max){
  x / max
}
#' Updates individual locations
#' @param data_frame holds data about deer
#' @export
move<-function(ind, landscape, nrow, ncol){
    nbrs<-getNeighbors(c(ind$x.cell,ind$y.cell), nrow, ncol)
    # new_loc<-nbrs[[round(runif(1,1,length(nbrs)))]]
    new_loc<-makeDecision(landscape, nbrs=nbrs,ind)
    c(new_loc[[1]], new_loc[[2]])
}

#' helper function
eucDistance<-function(P1, P2, earthRadius){
  p1<-earthRadius*(P1*(pi/180))
  p2<-earthRadius*(P2*(pi/180))
  sqrt((((p2[1]-p1[1])^2)+((p2[2]-p1[2])^2)))
}

#' Chooses best possible landscape component to move to
#' TODO alter in the case of an make_decision being fed an empty list, make 
#' else case
#' TODO implement sorting function
#' @param 
#' @export
makeDecision<-function(landscape, nbrs, ind){
  # assign decision to be the first element by default--make comparison
  decision_vec <- c()
    i<-1
    #empty list to hold values of neighbors
    weights<-c()
    print(length(nbrs))
    for(i in 1:length(nbrs)){
      # append value of neighbor to weight vector
      if(6<=landscape[nbrs[[i]][1],][nbrs[[i]][2]] & landscape[nbrs[[i]][1],][nbrs[[i]][2]]<=11){
      weights[i]<-3
      # buildings
      }
      else if(landscape[nbrs[[i]][1],][nbrs[[i]][2]]==3){
        weights[i]<-0
      }
      else if(landscape[nbrs[[i]][1],][nbrs[[i]][2]]==5){
        weights[i]<-1
      }
      else{
        weights[i]<-2
      }
      # if we aren't in an area in individual can't go
      # we want to weight the distance from HR center
      if(!weights[i]==0){
        print(weights)
        inv.distance<-(1/eucDistance(c(xFromCol(lc.agg,nbrs[[i]][1]),
                                      yFromRow(lc.agg,nbrs[[i]][2])),
                                    c(ind$x.coord.init,ind$y.coord.init),
                                    earth.radius))
        if(!is.infinite(inv.distance)){
        weights[i]<-weights[i]*inv.distance}
      }
      
    }
    # weighted sample is a vector of the indices of nbrs weighted by
    # their corresponding values in the landscape matrix
    weighted_sample<-sample(c(1:length(nbrs)), size=100, replace=TRUE, prob=weights)
    # now we want to randomly sample from this list to get the index
    decision_val <- sample(weighted_sample, 1)
    # decision_vec <- c(nbrs[[decision_val]][1], nbrs[[decision_val]][2])
    nbrs[[decision_val]]
}

#' Helper function
#' returns neighborhood of cells as a list of vectors
#' @param loc current coordinates of individual
#' @param nrow # of rows in landscape matrix
#' @param ncol # columns in landscape matrix
#' @export
getNeighbors<-function(loc, nrow, ncol){
  k=1
  l<-list()
  # check if either x,y element of loc is greater than
  # the dimension of the landscape matrix
  for(i in -percep:percep){
    for(j in -percep:percep){
      # case 1 on left or right edge of matrix
      if(!(loc[1]+i < 1 | loc[1]+i > nrow) & !(loc[2]+j < 1 | loc[2]+j > ncol)){
        l[[k]] <- c(loc[1] + i, loc[2] + j)
        k<-k+1
      }
    }
  }
  l
}
# Main simulation
# TODO make main simulation loop, using functional? (lapply)
check_inds_locs<-function(ind, data_frame){
  for(i in 1:nrow(data_frame)){
    if(is_same_location(ind, data_frame[i,]) & is_not_same_id(ind, data_frame[i,])){
      
    }
  }
}

#helper function is location the same
is_same_location<-function(ind1, ind2){
  (ind1$x.cell == ind2$x.cell) & (ind1$y.cell == ind2$y.cell)
}

#helper checks if two ids are the same
is_not_same_id<-function(ind1, ind2){
  !(ind1$id == ind2$id)
}

recover_inds<-function(data_frame, gamma){
  infected<-which(data_frame$status=="I" ) #which individuals are currently infected?
  if(length(infected>0)){
    rec.prob<-runif(length(infected), min=0, max=1)
    for (i in 1:length(infected)){
      if(rec.prob[i]<= gamma){
        data_frame$status[infected[i]]<-"R"
      }
    }
  }
  return(data_frame)
}
# make infection matrix
b <- as(extent(507000, 508000, 5007000, 5008000), 'SpatialPolygons')
lc=raster::raster(".\\tcma_lc_finalv1.tif")
lc.crop <- crop(lc,b)
lc.agg <- aggregate(lc.crop,20)
# convert to matrix
lc.mat<-matrix(lc.agg, nrow=nrow(lc.agg), ncol=ncol(lc.agg))
lc.mat.inf<-matrix(0, nrow=nrow(lc.agg), ncol=ncol(lc.agg))
inds<-make_inds(100,
          nrow(lc.mat),
          ncol(lc.mat),
          1,
          lc.agg)
write.csv(inds,
          "./deerdat.csv",
          row.names=FALSE)
for(t in 1:100){
  for(i in 1:nrow(inds)){
    if(inds[i,]$status=="I"){
      lc.mat.inf[inds[i,]$x.cell + inds[i,]$y.cell] <- lc.mat.inf[inds[i,]$x.cell + inds[i,]$y.cell] + 1
      # check if the current cell is occupied by another individual
    for(j in 1:nrow(inds)){
      if(is_same_location(inds[j,], inds[i,]) & !(inds[j,]$id == inds[i,]$id)){
        inds[j,]$status = "I"
        }
      }
    }
  print(paste("individual", i, "current cell:", inds[i,]$x.cell, inds[i,]$y.cell), sep=" ")
  new.cell<-move(inds[i,], lc.mat, nrow(lc.mat), ncol(lc.mat)) # grab the new cell
  print(paste("updated cell: ", new.cell[1], new.cell[2]), sep=" ")
  inds[i,]$x.cell <- new.cell[1]
  inds[i,]$y.cell <- new.cell[2]
  inds[i,]$time_step = inds[1,]$time_step+1
  inds[i,]$x.coord = xFromCol(lc.agg,new.cell[1])
  inds[i,]$y.coord = yFromRow(lc.agg,new.cell[2])
  }
  write.table(inds,  "./deerdat.csv",
              row.names=FALSE, sep=",", append=TRUE, col.names=FALSE)
}


data<-read.csv("./deerdat.csv")
lc.pts <- rasterToPoints(lc.agg, spatial = TRUE)
lc.df  <- data.frame(lc.pts)
# set our column names to be something a bit more descriptive
ggplot(data=lc.df, aes(x=x, y=y)) + 
  geom_raster(aes(fill=tcma_lc_finalv1)) +
  geom_point(data = data, aes(x = x.coord, y = y.coord, color = "red", group=id))+
  geom_path(data = data, aes(x = x.coord, y = y.coord, color = "red", group=id))