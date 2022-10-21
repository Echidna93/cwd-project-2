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

#' Chooses best possible landscape component to move to
#' TODO alter in the case of an make_decision being fed an empty list, make 
#' else case
#' TODO implement sorting function
#' @param 
#' @export
make_decision<-function(landscape, nbrs, distance_vector){
  # assign decision to be the first element by default--make comparison
  decision_vec <- c()
  if(binary){
    i<-1
    while(!is_habitat(landscape, nbrs[[i]][1], nbrs[[i]][2])){
      #i<-round(runif(1, min=1, max=length(nbrs[1,])))
      i<-i+1
    }
  }
  else{
    #empty list to hold values of neighbors
    weights<-c()
    landscape_values<-c()
    for(i in 1:length(nbrs)){
      # append value of neighbor to weight vector
      landscape_values[i]<-landscape[nbrs[[i]][1],][nbrs[[i]][2]]
    }
    weights <- landscape_values * distance_vector
    # weighted sample is a vector of the indices of nbrs weighted by
    # their corresponding values in the landscape matrix
    weighted_sample<-sample(c(1:length(nbrs)), size=100, replace=TRUE, prob=weights)
    print(weighted_sample)
    # print(weighted_sample)
    # now we want to randomly sample from this list to get the index
    decision_val <- sample(weighted_sample, 1)
    decision_vec <- c(nbrs[[decision_val]][1], nbrs[[decision_val]][2])
  }
  decision_vec
}

#' Helper function
#' returns neighborhood of cells as a list of vectors
#' @param loc current coordinates of individual
#' @param nrow # of rows in landscape matrix
#' @param ncol # columns in landscape matrix
#' @export
get_neighbors<-function(loc, nrow, ncol){
  k=1
  l<-list()
  # check if either x,y element of loc is greater than
  # the dimension of the landscape matrix
  for(i in -2:2){
    for(j in -2:2){
      # case 1 on left or right edge of matrix
      if(!(loc[1]+i < 1 | loc[1]+i > nrow) & !(loc[2]+j < 1 | loc[2]+j > ncol)){
        l[[k]] <- c(loc[1] + i, loc[2] + j)
        k<-k+1
      }
    }
  }
  l
}

get_distance_vector<-function(current_location, neighbors){
  distance_vector<-c()
  for(i in 1:length(neighbors)){
    current_neighbor<-c(neighbors[[i]][1],neighbors[[i]][2])
    distance<-dist(rbind(current_location, current_neighbor))
    # check if we're comparing against the current cell that the individual
    # is occupying
    if(distance==0){
      distance_vector[i]<-1
    }
    else{
      distance_vector[i]<-distance
    }
  }
  distance_vector
}

#' Updates individual locations
#' @param data_frame holds data about deer
#' @export
move<-function(data_frame, landscape, nrow, ncol){
  for(i in 1:nrow(data_frame)){
    nbrs<-get_neighbors(c(data_frame[i,]$x,data_frame[i,]$y), nrow, ncol)
    distance_vector<-unlist(lapply(get_distance_vector(c(data_frame[i,]$x,data_frame[i,]$y), nbrs), get_inverse))
    new_loc<-make_decision(landscape=landscape, nbrs=nbrs, binary, distance_vector)
    data_frame[i,]$x<-new_loc[[1]]
    data_frame[i,]$y<-new_loc[[2]]
  }
  data_frame
}