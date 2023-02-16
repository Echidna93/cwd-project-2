library(deSolve)
library(scatterplot3d)

# for an established CWD zone 10% occurance (https://www.cdc.gov/prions/cwd/occurrence.html#:~:text=The%20infection%20rates%20among%20some,at%20least%20one%20captive%20herd.&text=As%20of%20June%202022%2C%20there,CWD%20in%20free%2Dranging%20cervids.)
# annual death rate CWD+ .75--adjusted daily .002
# annual death rate CWD- .25--adjusted daily .0006

rigidode <- function(t, y, parms) {
  dS <-  S * b - (beta.i*S*I + beta.e*S*E + d*S) # change in susceptible pop
  dI <-  I + beta.i*S*I + beta.e*S*E - (I*d + I*nu) # change in infected pop.
  dE <-  I * sigma
  list(c(dS, dI, dE))
}

# beta.i = infction rate
# I = current infected population
# S = current susceptible population
# sigma = shedding rate
# b = births

# parms
dbar = .002 # death rate for CWD+ deer 
beta.i = .001 # infection rate, individual to individual
d = .0006 # background mortality rate
nu = dbar - d # differential death rate (CWD+ death rate - CWD- death rate)
beta.e = 0.00001 # transmission from soil
b = .005 # daily birth rate (assuming 2 births per year)
sigma = .0001 # shedding rate

# yini
S = 10000
E = .00001
I = 5

parms<-c(beta.i, d, nu, b, sigma, beta.e)
yini <- c(S, E, I)
times <- seq(from = 0, to = 365*2, by = 1)
out <- ode (times = times, y = yini, func = rigidode, parms = parms)
head (out, n = 3)

plot(out)
