cwd.dat<-read.table(
  "./data/cwd-plus-2010-2023.csv", 
  sep=",", header=TRUE)

# first we want to make sure there aren't any capital letters in our headers
for( i in 1:length(names(cwd.dat))){
  names(cwd.dat)[i] <- tolower(names(cwd.dat)[i])
}
# subset to 2016 on
# years when we have mandatory sampling
cwd.dat <- subset(cwd.dat, year >= 2016)
# next let's get rid of capital letters in the data itself
cwd.dat <- subset(cwd.dat, sample_acquisition == "Hunter harvested")

cwd.dat
