# function that calculates multivariate climate change distance and velocity from Hamman et al. 2015
# Modified by Erin Keleske
# December 27, 2017

# Function mult.dist gives output as a data.frame with columns of x-coordinates, y-coordinates,
# climate change distance, log(distance)*100, climate change velocity, and log(velocity)*100
# Input parameters are data.frames of x and y coordinates, and climate variable clim.var

mult.dist <- function(old, rec, thresh=0.25, years){
  library(SDMTools)     # install package to read and write ESRI ASCII grids
  library(yaImpute)     # install package for k-nearest neighbor (kNN) search
  
  present1 <- asc2dataframe(old) # principal component grids
  # present2 <- asc2dataframe(old)
  future1  <- asc2dataframe(rec)
  # future2  <- asc2dataframe(rec)
  
  idxy=cbind(id=1:nrow(present1),present1[,1:2])   # data frame of IDs and XY coordinates
  b=(max(present1$var.1)-min(present1$var.1))/120  # bin size for 120 PC1 bins
  
  runs <- 5                                  # number of smoothing runs (user defined)
  offsets <- seq(0,b,b/runs)[1:runs]         # offset values for runs
  results <- data.frame(id=1:nrow(present1)) # empty data frame for results
  
  for (o in offsets) {
    
    p1 <- round((present1$var.1+o)/b)        # convert PC1 to 120 bins via rounding
    p2 <- round((present2$var.1+o)/b)        # convert PC2 to <120 bins via rounding
    f1 <- round((future1$var.1+o)/b)         # same for future PC1
    f2 <- round((future2$var.1+o)/b)         # same for future PC2
    p  <- paste(p1,p2)                       # PC1/PC2 combinations in present climate
    f  <- paste(f1,f2)                       # PC1/PC2 combinations in future climate
    u  <- unique(p)[order(unique(p))]        # list of unique PC1/PC2 combinations
    
    sid <- c()                               # empty vector for source IDs
    tid <- c()                               # empty vector for target IDs
    d   <- c()                               # empty vector for distances
    
    for(i in u){                         # loop for each unique PC1/PC2 combination
      pxy <- idxy[which(p==i),]          # coordinates of i-th combination in present
      fxy <- idxy[which(f==i),]          # coordinates of i-th combination in future
      sid <- c(sid, pxy$id)              # append i-th PC1/PC2 combination to previous 
      
      if(nrow(fxy)>0){                   # kNN search unless no-analogue climate
        knn <- data.frame(ann(as.matrix(fxy[,-1]), as.matrix(pxy[,-1]), k=1)$knnIndexDist)      
        tid <- c(tid, fxy[knn[,1],"id"]) # the IDs of the closest matches  
        d <- c(d, sqrt(knn[,2]))         # their corresponding geographic distances
      }
      else {                             # else statement for no-analogue climates
        tid <- c(tid, rep(NA,nrow(pxy))) # flag destinations as missing for no analogues
        d <- c(d, rep(Inf,nrow(pxy)))    # flag distances as infinity for no analogues
      }
    }
    
    d <- cbind(id=sid, distance=d)  
    results <- merge(results, d, by=c("id"))
  }
  
  results$mean=rowMeans(results[2:runs+1])
  
  # writes out log10 velocities and distances multiplied by 100 in ESRI ASCII format
  # conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
  out=merge(idxy, results[,c(1,runs+2)], by=c("id"), sort=F)
  # out$mean[out$mean==Inf] <- 10000  # sets no analogue to 10,000km
  # out$mean[out$mean==0] <- 0.5  # sets zero distance to 0.5km (1/2 cell size)
  out$logDist=round(log10(out$mean)*100)
  out$logSpeed=round(log10(out$mean/years)*100)
  
}