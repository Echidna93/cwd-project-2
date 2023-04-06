library(jagsUI)

# for an established CWD zone 10% occurance (https://www.cdc.gov/prions/cwd/occurrence.html#:~:text=The%20infection%20rates%20among%20some,at%20least%20one%20captive%20herd.&text=As%20of%20June%202022%2C%20there,CWD%20in%20free%2Dranging%20cervids.)
# annual death rate CWD+ .75--adjusted daily .002
# annual death rate CWD- .25--adjusted daily .0006

# note that zone 648 had a total harvest of 
# zone 648 stats--note that CWD + not part of the harvest were removed from the CWD+ column here
#     year  CWD+  adult male harvest      fawn male      adult female harvest    fawn female harvest      total harvest   Land area (sq. mi)     Bucks/sq. mile      Antlerless/sq.mile      Total / sq. mile   Rank
#     2016  0
#     2017  0
#     2018  0
#     2019  7    838                     165                 572                    141                    1716          332                       2.52                   2.64              5.17              16
#     2020  5    831                     185                 682                    141                    1839          332                      2.50                    3.04              5.54              18
#     2021  9    766                     158                 499                    134                    1557         332                       2.31                    2.38              4.69              24
#     2022  4    692                     140                 459                    160                    1451         332                       2.08                    2.29              4.37              25


# zone 647
#     year  CWD+ adult male harvest      fawn male      adult female harvest    fawn female harvest      total harvest   Land area (sq. mi)     Bucks/sq. mile      Antlerless/sq.mile      Total / sq. mile    Rank
#     2019   3      706                     124            452                     98                       1380            434                    1.63                1.55                    3.18               41
#     2020   4      662                     112            512                     81                       1367            434                    1.53                1.62                    3.15               40
#     2021   6      640                     109            424                     84                       1257            434                    1.47                1.42                    2.90               40
#     2022   6      584                     108            416                     88                       1196            434                    1.35                1.41                    2.76               42
# # let's turn this into a df


n.cwd.p <- matrix(c(3, 4, 6, 6,
                    7, 5, 9, 4),
                  nrow = 4, ncol = 2, byrow = TRUE)


# year<-c(2019, 2020, 2021, 2022)
# # square miles
# sq.miles <- 332 # this can become a vector as we add more regions later on
# cwd.p<- c(7, 5, 9, 4)
# ttl.harvest<-c(1716, 1839, 1557, 1451)
# n.p.sq.mi<-c(5.17, 5.54, 4.69, 4.37)
# cwd.dat<-data.frame(year,cwd.p, ttl.harvest, n.p.sq.mi)
# # generate our proportion of positivity
# # generate our residuals
# cwd.dat$prop.pos<-cwd.dat$cwd.p / cwd.dat$ttl.harvest
# avg.prop.pos<-cwd.dat$prop.pos / nrow(cwd.dat)
# cwd.dat$prop.pos.resid <- cwd.dat$prop.pos - avg.prop.pos
# 
# prop.pos<-rnorm(1,
#                 mean=mean(cwd.dat$prop.pos.resid),
#                 sd = sd(cwd.dat$prop.pos.resid))
# 
# # create normal distribution of prop
# # generate our residuals
# cwd.dat$n.total<-cwd.dat$n.p.sq.mi
# cwd.dat$n.resid <- cwd.dat$n.total - mean(cwd.dat$n.total)
# n.total <- rnorm(1,
#                  mean=mean(cwd.dat$n.p.sq.mi),
#                  sd = sd(cwd.dat$n.p.sq.mi))
# 
# # T103R10S33
# # Zone 648, Fillmore County
# # 2019 - 2023
# # cwd.dat <- data.frame(cwd.pos<-c(4, 1, 2, 3, 4)
# # ) 
# 
# # beta.i = infction rate
# # I = current infected population
# # S = current susceptible population
# # sigma = shedding rate
# # b = births
# # E = density of Prion
# 
# # parms
# dbar = .002 # death rate for CWD+ deer
# beta.i = .001 # infection rate, individual to individual
# d = .0006 # background mortality rate
# nu = dbar - d # differential death rate (CWD+ death rate - CWD- death rate)
# beta.e = 0.000001 # transmission from soil
# b = .005 # daily birth rate (assuming 2 births per year)
# sigma = .0001 # shedding rate
# # 
# # we can make some coefficients for migration as well
# # this can be a parameter (proxy) for size as well
# m.i = 0.0003 # migration immigration
# m.e = 0.0002 # migration emmigration
# 
# # for now asume that m.e, m.i = m.e, m.i for CWD+, CWD- deer
# 
# # yini
# S = round(n.total * sq.miles, digits = 0)# make this a prop. of land area and n/sq. mile
# E = .00001
# I = round(S * prop.pos, digits = 0)
# 
# out.mat <- data.frame(S = c(),
#               I = c(),
#               E = c(), 
#               t = c())
# 
# rigidode <- function(t, y, parms) {
#   dS <-  S * b + m.i*S - (beta.i*S*I + beta.e*S*E + d*S) - m.e*S # change in susceptible pop
#   dI <-  I + beta.i*S*I + beta.e*S*E - (I*d + I*nu) # change in infected pop.
#   
#   # 
#   
#   
#   dE <-  I * sigma
#   list(c(dS, dI, dE))
# }
# 
# parms<-c(beta.i, d, nu, b, sigma, beta.e)
# yini <- c(S, E, I)
# times <- seq(from = 0, to = 365, by = 5)
# out <- ode (times = times, y = yini, func = rigidode, parms = parms)
#

# disease matrix
# this will hold the disease matrix that will be processed in another file

# list of variables 
jags.data<-list()

{sink("cwd.jags"); cat("
model { # JAGS code begins here
  # define our s_mu as parameter
  s_mu~dnorm(0,0.33)
  # define our s_sd as parm
  s_sd~dunif(0,2)
  
  #likewise for f
  f_mu~dnorm(0,0.33)
  # define our s_sd as parm
  f_sd~dunif(0,2)
  
  # convert sd to tau
  s_tau <- pow(s_sd, -2) 
  f_tau <- pow(f_sd, -2)
  
  # Define the multinomial likelihoods
  
  for (i in 1:n.nbs){
    } #t2

  # Last column: probability of non-recovery
} # end JAGS model
",fill = TRUE); sink()}

# Parameters monitored
parms1 <- c()

# MCMC settings
na <- 1000; ni <- 7000; nt <- 1; nb <- 2000; nc <- 3

# Call JAGS from R (BRT <1 min)
Brownie_5 <- jagsUI(jags.data, inits = NULL, parms1, "cwd.jags",
                    n.adapt = na, n.chains = nc, n.thin = nt, n.iter = ni,
                    n.burnin = nb, parallel = TRUE)