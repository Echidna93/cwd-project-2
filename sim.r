library(jagsUI)
source("process-data.r")
library(maps)
library(maptools)
library(spdep)
library(rgdal)
library(spatialreg)
library(classInt)
library(RColorBrewer)
library(kableExtra)

# disease matrix
# this will hold the disease matrix that will be processed in another file
# list of variables



# jags.data<-list()
# 
# {sink("wtd.pop.jags"); cat("
# model { # JAGS code begins here
#   # define our s_mu as parameter
#   s_mu~dnorm(0,0.33)
#   # define our s_sd as parm
#   s_sd~dunif(0,2)
#   
#   #likewise for f
#   f_mu~dnorm(0,0.33)
#   # define our s_sd as parm
#   f_sd~dunif(0,2)
#   
#   # convert sd to tau
#   s_tau <- pow(s_sd, -2) 
#   f_tau <- pow(f_sd, -2)
#   
#   # Define the multinomial likelihoods
#   
#   for (i in 1:n.nbs){
#     } #t2
# 
#   # Last column: probability of non-recovery
# } # end JAGS model
# ",fill = TRUE); sink()}
# 
# # Parameters monitored
# parms1 <- c()
# 
# # MCMC settings
# na <- 1000; ni <- 7000; nt <- 1; nb <- 2000; nc <- 3
# 
# # Call JAGS from R (BRT <1 min)
# Brownie_5 <- jagsUI(jags.data, inits = NULL, parms1, "wtd.pop.jags",
#                     n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni,
#                     n.burnin = nb, parallel = TRUE)