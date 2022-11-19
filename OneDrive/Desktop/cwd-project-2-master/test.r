library(sf)           
library(terra)          
library(RColorBrewer) 
library(ggplot2)      
library(reshape2)
library(raster)
library(dismo)
library(mvtnorm)
library
case = 8


lc<-raster("C:\\Users\\jackx\\OneDrive\\Desktop\\cwd-project-2-master\\tcma_lc_finalv1\\tcma_1000_by_1000_croppped.tif")
n.initial=50
n.hr=50
nloc=50
nsamp=10
Xmax<-as.numeric(xmax(lc))
Xmin<-as.numeric(xmin(lc))
Ymin<-as.numeric(ymin(lc))
Ymax<-as.numeric(ymax(lc))


hr.rand<-data.frame(x=runif(n.initial*n.hr)*(Xmax-Xmin)+Xmin,y=runif(n.initial*n.hr)*(Ymax-Ymin)+Ymin)
hr.rand$w=1
hr.centroid<-hr.rand[sample(1:nrow(hr.rand), n.initial, replace=FALSE,prob=hr.rand$w),c("x","y")]
ind.loc<-NULL
for(i in 1:n.initial){
  ind.loc<-cbind("id"=i,as.data.frame(rmvnorm(nloc*nsamp,mean=as.matrix(hr.centroid[i,c('x','y')])),sigma=diag(2)*sigma.hr^2))
  names(ind.loc)<-c('id','x','y')
  ind.loc=ind.loc[ind.loc$x<Xmax & ind.loc$x<Ymax & ind.loc$y>Ymin & ind.loc$y<Ymax,]
  ind.loc=ind.loc[1:min(nloc,nrow(ind.loc)),]
  
  ind.loc$disthr=sqrt((ind.loc$x-hr.centroid$x[i])^2+(ind.loc$y-hr.centroid$y[i])^2)
}
lc.pts <- rasterToPoints(lc, spatial = TRUE)
lc.df  <- data.frame(lc.pts)
lc.df
# set our column names to be something a bit more descriptive
colnames(lc.df)<-c("cover_type", "x", "y", "optional")
lc.plot<-ggplot(data=lc.df, aes(x=x, y=y)) +
  geom_raster(aes(fill=cover_type)) +
  geom_point(data = ind.loc, aes(x = x, y = y))
lc.plot