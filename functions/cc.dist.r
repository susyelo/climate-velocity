# function that calculates climate change distance and velocity from Hamman et al. 2015
# Modified by Erin Keleske and Drew Kerkhoff
# November 2, 2017

# Function cc.dist gives output as a data.frame with columns of x-coordinates, y-coordinates,
# climate change distance, log(distance)*100, climate change velocity, and log(velocity)*100
# Input parameters are data.frames of x and y coordinates, and climate variable clim.var

cc.dist <- function(old, rec, clim.var, thresh=0.25, years){
  
# library(SDMTools)       # install package to read and write ESRI ASCII grids
# library(yaImpute)       # install package for k-nearest neighbor (kNN) search
  
  present <- old
  future  <- rec
  
  t <- thresh              # plus/minus threshold to define climate match (user defined)
  t <- 1/(t*2)            # inverse for rounding, double for plus/minus
  
  idxy <- cbind(id=1:nrow(present),present[,1:2])  # data frame of IDs and XY coords
  
  runs <- 5                                        # number of smoothing runs (user defined)
  offsets <- seq(0,1/t,1/(t*runs))[1:runs]         # offset values for runs
  results <- data.frame(id=1:nrow(present))        # empty data frame for results
  
  for (o in offsets) {
    
    p <- round((present$bio1+o)*t)/t     # vector of rounded present climate values 
    f <- round((future$bio1+o)*t)/t      # vector of rounded future climate values 
    u <- unique(p)[order(unique(p))]      # list of unique climate values in p
    
    sid <- c()                            # empty vector for source IDs
    tid <- c()                            # empty vector for target IDs
    d   <- c()                            # empty vector for distances
    
    for(i in u){                          # loop for each unique climate value
      pxy <- idxy[which(p==i),]           # coordinates of i-th combination in present
      fxy <- idxy[which(f==i),]           # coordinates of i-th combination in future
      sid <- c(sid, pxy$id)               # append i-th PC1/PC2 combination to previous 
      
      if(nrow(fxy)>0){                    # kNN search unless no-analogue climate
        knn <- data.frame(ann(as.matrix(fxy[,-1]), as.matrix(pxy[,-1]), k=1)$knnIndexDist)      
        tid <- c(tid, fxy[knn[,1],"id"])  # the IDs of the closest matches  
        d <- c(d, sqrt(knn[,2]))          # their corresponding geographic distances
      }
      else {                              # else statement for no-analogue climates
        tid <- c(tid, rep(NA,nrow(pxy)))  # flag destinations as missing for no analogues
        d <- c(d, rep(NA,nrow(pxy)))     # flag distances as missing for no analogues
      }
    }
    
    d <- cbind(id=sid, distance=d)  
    results <- merge(results, d, by=c("id"))
  }
  
  results$mean=rowMeans(results[2:runs+1])
  
  # writes out log10 velocities and distances multiplied by 100 in data.frame
  # conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
  out=merge(idxy, results[,c(1,runs+2)], by=c("id"), sort=F)
  out$mean[out$mean==Inf] <- NA 
  # out$mean[out$mean==0] <- 0.5  # sets zero distance to 0.5km (1/2 cell size)
  out$Dist=round(out$mean)
  out$Speed=round(out$mean/years)
  out$logDist=round(log10(out$mean)*100)
  out$logSpeed=round(log10(out$mean/years)*100)
  
  return(out)
}


