# Function to calculate 19 BioClim variables and translate from x and y coordinates to
# relative distance from (0,0) in km. 
# Erin Keleske

# Expected input is a data.frame with monthly mean temperatures and column names labeled with 
# the first three letters of each month. **specify what the dataframes are 

bioclim.var <- function(temp, prc){
  
  rows <- nrow(temp)
  
  clim <- data.frame(matrix(ncol=21, nrow=rows))
  colnames(clim) <- c("x", "y", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10","bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
  
  clim$x <- temp$lon
  
  clim$y <- temp$lat
  
  # Annual Mean Temperature (BIO1)
  clim$bio1 <- rowMeans(temp[,-c(1:2)])
  
  # Temperature Seasonality (BIO4)
  # Here the calculation was talking into account x and y values
  clim$bio4 <- (rowSds(as.matrix(temp[,-c(1:2)]), rows=NULL, cols=NULL, na.rm=FALSE))/clim$bio1*100
  
  # Max Temperature of Warmest Month (BIO5)
  clim$bio5 <- pmax(temp$jan, temp$feb, temp$mar, temp$apr, temp$may, temp$jun, temp$jul, temp$aug, temp$sep, temp$oct, temp$nov, temp$dec)
  
  # Min Temperature of Coldest Month (BIO6)
  clim$bio6 <- pmin(temp$jan, temp$feb, temp$mar, temp$apr, temp$may, temp$jun, temp$jul, temp$aug, temp$sep, temp$oct, temp$nov, temp$dec)
  
  # Temperature Annual Range (BIO7)
  clim$bio7 <- clim$bio5 - clim$bio6
  
  # Mean Diurnal Range (BIO2)
  clim$bio2 <- clim$bio1*clim$bio7
  
  # Isothermality (BIO3)
  clim$bio3 <- (clim$bio2/clim$bio7)*100
  
  # Prepare quarter data 
  qrt.prc <- data.frame(matrix(ncol=14, nrow=rows))
  colnames(qrt.prc) <- c("jfm", "fma", "mam", "amj", "mjj", "jja", "jas", "aso", "son", "ond", "ndj", "djf", "colMax", "colMin")
  
  qrt.prc$jfm <- prc$jan + prc$feb + prc$mar
  qrt.prc$fma <- prc$feb + prc$mar + prc$apr
  qrt.prc$mam <- prc$mar + prc$apr + prc$may
  qrt.prc$amj <- prc$apr + prc$may + prc$jun
  qrt.prc$mjj <- prc$may + prc$jun + prc$jul
  qrt.prc$jja <- prc$jun + prc$jul + prc$aug
  qrt.prc$jas <- prc$jul + prc$aug + prc$sep
  qrt.prc$aso <- prc$aug + prc$sep + prc$oct
  qrt.prc$son <- prc$sep + prc$oct + prc$nov
  qrt.prc$ond <- prc$oct + prc$nov + prc$dec
  qrt.prc$ndj <- prc$nov + prc$dec + prc$jan
  qrt.prc$djf <- prc$dec + prc$jan + prc$feb
  
  qrt.prc$colMax = colnames(qrt.prc[,1:12])[apply(qrt.prc[,1:12],1,which.max)]
  qrt.prc$colMin = colnames(qrt.prc[,1:12])[apply(qrt.prc[,1:12],1,which.min)]
  
  qrt.tmp <- data.frame(matrix(ncol=14, nrow=rows))
  colnames(qrt.tmp) <- c("jfm", "fma", "mam", "amj", "mjj", "jja", "jas", "aso", "son", "ond", "ndj", "djf", "colMax", "colMin")
  
  qrt.tmp$jfm <- temp$jan + temp$feb + temp$mar
  qrt.tmp$fma <- temp$feb + temp$mar + temp$apr
  qrt.tmp$mam <- temp$mar + temp$apr + temp$may
  qrt.tmp$amj <- temp$apr + temp$may + temp$jun
  qrt.tmp$mjj <- temp$may + temp$jun + temp$jul
  qrt.tmp$jja <- temp$jun + temp$jul + temp$aug
  qrt.tmp$jas <- temp$jul + temp$aug + temp$sep
  qrt.tmp$aso <- temp$aug + temp$sep + temp$oct
  qrt.tmp$son <- temp$sep + temp$oct + temp$nov
  qrt.tmp$ond <- temp$oct + temp$nov + temp$dec
  qrt.tmp$ndj <- temp$nov + temp$dec + temp$jan
  qrt.tmp$djf <- temp$dec + temp$jan + temp$feb
  
  qrt.tmp$colMax = colnames(qrt.tmp[,1:12])[apply(qrt.tmp[,1:12],1,which.max)]
  qrt.tmp$colMin = colnames(qrt.tmp[,1:12])[apply(qrt.tmp[,1:12],1,which.min)]
  

#  Mean Temperature of Wettest Quarter (BIO8) ---------------------------

  
  x_df<-data.frame(jfm = c("jan" , "feb" , "mar"))
  x_df$fma <- c("feb" , "mar" , "apr")
  x_df$mam <- c("mar" , "apr" , "may")
  x_df$amj <- c("apr" , "may" , "jun")
  x_df$mjj <- c("may" , "jun" , "jul")
  x_df$jja <- c("jun" , "jul" , "aug")
  x_df$jas <- c("jul" , "aug" , "sep")
  x_df$aso <- c("aug" , "sep" , "oct")
  x_df$son <- c("sep" , "oct" , "nov")
  x_df$ond <- c("oct" , "nov" , "dec")
  x_df$ndj <- c("nov" , "dec" , "jan")
  x_df$djf <- c("dec" , "jan" , "feb")
  
  # Loop is the equivalent to the following lines
  #ix <- which(qrt.prc$colMax=="jfm")
  #clim$bio8[ix] <- ((temp$jan + temp$feb + temp$mar)/3)[ix]
    
  for (i in 1:length(x_df)){
    ix <- which(qrt.prc$colMax==colnames(x_df)[i])
    clim$bio8[ix]<-rowMeans(temp[colnames(temp)%in%x_df[,i]])[ix]
  }
  

# Mean Temperature of Driest Quarter (BIO9) -------------------------------

  for (i in 1:length(x_df)){
    ix <- which(qrt.prc$colMin==colnames(x_df)[i])
    clim$bio9[ix]<-rowMeans(temp[colnames(temp)%in%x_df[,i]])[ix]
  }
  

# Mean Temperature of Warmest Quarter (BIO10) -----------------------------

  for (i in 1:length(x_df)){
    ix <- which(qrt.tmp$colMax==colnames(x_df)[i])
    clim$bio10[ix]<-rowMeans(temp[colnames(temp)%in%x_df[,i]])[ix]
  }
  

# Mean Tempeature of Coldest Quarter (BIO11) ------------------------------

  for (i in 1:length(x_df)){
    ix <- which(qrt.tmp$colMin==colnames(x_df)[i])
    clim$bio11[ix]<-rowMeans(temp[colnames(temp)%in%x_df[,i]])[ix]
  }


# Annual Precipitation (BIO12) --------------------------------------------
  
  clim$bio12 <- (prc$jan + prc$feb + prc$mar + prc$apr + prc$may + prc$jun + prc$jul + prc$aug + prc$sep + prc$oct + prc$nov + prc$dec)
  

# Precipitation of Wettest Month (BIO13) ----------------------------------

  clim$bio13 <- pmax(temp$jan, temp$feb, temp$mar, temp$apr, temp$may, temp$jun, temp$jul, temp$aug, temp$sep, temp$oct, temp$nov, temp$dec)
  

# Precipitation of Driest Month (BIO14) -----------------------------------

  clim$bio14 <- pmin(temp$jan, temp$feb, temp$mar, temp$apr, temp$may, temp$jun, temp$jul, temp$aug, temp$sep, temp$oct, temp$nov, temp$dec)
  

# Precipitation Seasonality (BIO15) ---------------------------------------

  prc <- data.frame(apply(prc, 2, function(x) as.numeric(as.character(x))))
  clim$bio15 <- (rowSds(as.matrix(prc[,-c(1:2)]), rows=NULL, cols=NULL, na.rm=TRUE))/(1 + (clim$bio12/12))*100
  
  # Precipitation of Wettest Quarter (BIO16)

  for (i in 1:length(x_df)){
    ix <- which(qrt.prc$colMax==colnames(x_df)[i])
    clim$bio16[ix]<-rowSums(prc[colnames(prc)%in%x_df[,i]])[ix]
  }  

  # Precipitation of Driest Quarter (BIO17)

  for (i in 1:length(x_df)){
    ix <- which(qrt.prc$colMin==colnames(x_df)[i])
    clim$bio17[ix]<-rowSums(prc[colnames(prc)%in%x_df[,i]])[ix]
  }  
  

  # Precipitation of Warmest Quarter (BIO18)

  for (i in 1:length(x_df)){
    ix <- which(qrt.tmp$colMax==colnames(x_df)[i])
    clim$bio18[ix]<-rowSums(prc[colnames(prc)%in%x_df[,i]])[ix]
  }


  # Precipitation of Coldest Quarter (BIO19)
  
  for (i in 1:length(x_df)){
    ix <- which(qrt.tmp$colMin==colnames(x_df)[i])
    clim$bio19[ix]<-rowSums(prc[colnames(prc)%in%x_df[,i]])[ix]
  }
  
  return(clim)
}




