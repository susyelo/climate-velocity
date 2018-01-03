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
  clim$bio1 <- (temp$jan + temp$feb + temp$mar + temp$apr + temp$may + temp$jun + temp$jul + temp$aug + temp$sep + temp$oct + temp$nov + temp$dec)/12
  
  # Temperature Seasonality (BIO4)
  clim$bio4 <- (rowSds(as.matrix(temp), rows=NULL, cols=NULL, na.rm=FALSE))/clim$bio1*100
  
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
  
  # Mean Temperature of Wettest Quarter (BIO8)
  if(qrt.prc$colMax=="jfm"){
    clim$bio8 <- (temp$jan + temp$feb + temp$mar)/3
  } else if (qrt.prc$colMax=="fma"){
    clim$bio8 <- (temp$feb + temp$mar + temp$apr)/3
  } else if (qrt.prc$colMax=="mam"){
    clim$bio8 <- (temp$mar + temp$apr + temp$may)/3
  } else if (qrt.prc$colMax=="amj"){
    clim$bio8 <- (temp$apr + temp$may + temp$jun)/3
  } else if (qrt.prc$colMax=="mjj"){
    clim$bio8 <- (temp$may + temp$jun + temp$jul)/3
  } else if (qrt.prc$colMax=="jja"){
    clim$bio8 <- (temp$jun + temp$jul + temp$aug)/3
  } else if (qrt.prc$colMax=="jas"){
    clim$bio8 <- (temp$jul + temp$aug + temp$sep)/3
  } else if (qrt.prc$colMax=="aso"){
    clim$bio8 <- (temp$aug + temp$sep + temp$oct)/3
  } else if (qrt.prc$colMax=="son"){
    clim$bio8 <- (temp$sep + temp$oct + temp$nov)/3
  } else if (qrt.prc$colMax=="ond"){
    clim$bio8 <- (temp$oct + temp$nov + temp$dec)/3
  } else if (qrt.prc$colMax=="ndj"){
    clim$bio8 <- (temp$nov + temp$dec + temp$jan)/3
  } else if (qrt.prc$colMax=="djf"){
    clim$bio8 <- (temp$dec + temp$jan + temp$feb)/3
  }
  
  # Mean Temperature of Driest Quarter (BIO9)
  if(qrt.prc$colMin=="jfm"){
    clim$bio9 <- (temp$jan + temp$feb + temp$mar)/3
  } else if (qrt.prc$colMin=="fma"){
    clim$bio9 <- (temp$feb + temp$mar + temp$apr)/3
  } else if (qrt.prc$colMin=="mam"){
    clim$bio9 <- (temp$mar + temp$apr + temp$may)/3
  } else if (qrt.prc$colMin=="amj"){
    clim$bio9 <- (temp$apr + temp$may + temp$jun)/3
  } else if (qrt.prc$colMin=="mjj"){
    clim$bio9 <- (temp$may + temp$jun + temp$jul)/3
  } else if (qrt.prc$colMin=="jja"){
    clim$bio9 <- (temp$jun + temp$jul + temp$aug)/3
  } else if (qrt.prc$colMin=="jas"){
    clim$bio9 <- (temp$jul + temp$aug + temp$sep)/3
  } else if (qrt.prc$colMin=="aso"){
    clim$bio9 <- (temp$aug + temp$sep + temp$oct)/3
  } else if (qrt.prc$colMin=="son"){
    clim$bio9 <- (temp$sep + temp$oct + temp$nov)/3
  } else if (qrt.prc$colMin=="ond"){
    clim$bio9 <- (temp$oct + temp$nov + temp$dec)/3
  } else if (qrt.prc$colMin=="ndj"){
    clim$bio9 <- (temp$nov + temp$dec + temp$jan)/3
  } else if (qrt.prc$colMin=="djf"){
    clim$bio9 <- (temp$dec + temp$jan + temp$feb)/3
  }
  
  # Mean Temperature of Warmest Quarter (BIO10)
  if(qrt.tmp$colMax=="jfm"){
    clim$bio10 <- (temp$jan + temp$feb + temp$mar)/3
  } else if (qrt.tmp$colMax=="fma"){
    clim$bio10 <- (temp$feb + temp$mar + temp$apr)/3
  } else if (qrt.tmp$colMax=="mam"){
    clim$bio10 <- (temp$mar + temp$apr + temp$may)/3
  } else if (qrt.tmp$colMax=="amj"){
    clim$bio10 <- (temp$apr + temp$may + temp$jun)/3
  } else if (qrt.tmp$colMax=="mjj"){
    clim$bio10 <- (temp$may + temp$jun + temp$jul)/3
  } else if (qrt.tmp$colMax=="jja"){
    clim$bio10 <- (temp$jun + temp$jul + temp$aug)/3
  } else if (qrt.tmp$colMax=="jas"){
    clim$bio10 <- (temp$jul + temp$aug + temp$sep)/3
  } else if (qrt.tmp$colMax=="aso"){
    clim$bio10 <- (temp$aug + temp$sep + temp$oct)/3
  } else if (qrt.tmp$colMax=="son"){
    clim$bio10 <- (temp$sep + temp$oct + temp$nov)/3
  } else if (qrt.tmp$colMax=="ond"){
    clim$bio10 <- (temp$oct + temp$nov + temp$dec)/3
  } else if (qrt.tmp$colMax=="ndj"){
    clim$bio10 <- (temp$nov + temp$dec + temp$jan)/3
  } else if (qrt.tmp$colMax=="djf"){
    clim$bio10 <- (temp$dec + temp$jan + temp$feb)/3
  }
  
  # Mean Tempeature of Coldest Quarter (BIO11)
  if(qrt.tmp$colMin=="jfm"){
    clim$bio11 <- (temp$jan + temp$feb + temp$mar)/3
  } else if (qrt.tmp$colMin=="fma"){
    clim$bio11 <- (temp$feb + temp$mar + temp$apr)/3
  } else if (qrt.tmp$colMin=="mam"){
    clim$bio11 <- (temp$mar + temp$apr + temp$may)/3
  } else if (qrt.tmp$colMin=="amj"){
    clim$bio11 <- (temp$apr + temp$may + temp$jun)/3
  } else if (qrt.tmp$colMin=="mjj"){
    clim$bio11 <- (temp$may + temp$jun + temp$jul)/3
  } else if (qrt.tmp$colMin=="jja"){
    clim$bio11 <- (temp$jun + temp$jul + temp$aug)/3
  } else if (qrt.tmp$colMin=="jas"){
    clim$bio11 <- (temp$jul + temp$aug + temp$sep)/3
  } else if (qrt.tmp$colMin=="aso"){
    clim$bio11 <- (temp$aug + temp$sep + temp$oct)/3
  } else if (qrt.tmp$colMin=="son"){
    clim$bio11 <- (temp$sep + temp$oct + temp$nov)/3
  } else if (qrt.tmp$colMin=="ond"){
    clim$bio11 <- (temp$oct + temp$nov + temp$dec)/3
  } else if (qrt.tmp$colMin=="ndj"){
    clim$bio11 <- (temp$nov + temp$dec + temp$jan)/3
  } else if (qrt.tmp$colMin=="djf"){
    clim$bio11 <- (temp$dec + temp$jan + temp$feb)/3
  }
  
  # Annual Precipitation (BIO12)
  clim$bio12 <- (prc$jan + prc$feb + prc$mar + prc$apr + prc$may + prc$jun + prc$jul + prc$aug + prc$sep + prc$oct + prc$nov + prc$dec)
  
  # Precipitation of Wettest Month (BIO13)
  clim$bio13 <- pmax(temp$jan, temp$feb, temp$mar, temp$apr, temp$may, temp$jun, temp$jul, temp$aug, temp$sep, temp$oct, temp$nov, temp$dec)
  
  # Precipitation of Driest Month (BIO14)
  clim$bio14 <- pmin(temp$jan, temp$feb, temp$mar, temp$apr, temp$may, temp$jun, temp$jul, temp$aug, temp$sep, temp$oct, temp$nov, temp$dec)
  
  # Precipitation Seasonality (BIO15)
  prc <- data.frame(apply(prc, 2, function(x) as.numeric(as.character(x))))
  clim$bio15 <- (rowSds(as.matrix(prc), rows=NULL, cols=NULL, na.rm=TRUE))/(1 + (clim$bio12/12))*100
  
  # Precipitation of Wettest Quarter (BIO16)
  if(qrt.prc$colMax=="jfm"){
    clim$bio16 <- (prc$jan + prc$feb + prc$mar)
  } else if (qrt.prc$colMax=="fma"){
    clim$bio16 <- (prc$feb + prc$mar + prc$apr)
  } else if (qrt.prc$colMax=="mam"){
    clim$bio16 <- (prc$mar + prc$apr + prc$may)
  } else if (qrt.prc$colMax=="amj"){
    clim$bio16 <- (prc$apr + prc$may + prc$jun)
  } else if (qrt.prc$colMax=="mjj"){
    clim$bio16 <- (prc$may + prc$jun + prc$jul)
  } else if (qrt.prc$colMax=="jja"){
    clim$bio16 <- (prc$jun + prc$jul + prc$aug)
  } else if (qrt.prc$colMax=="jas"){
    clim$bio16 <- (prc$jul + prc$aug + prc$sep)
  } else if (qrt.prc$colMax=="aso"){
    clim$bio16 <- (prc$aug + prc$sep + prc$oct)
  } else if (qrt.prc$colMax=="son"){
    clim$bio16 <- (prc$sep + prc$oct + prc$nov)
  } else if (qrt.prc$colMax=="ond"){
    clim$bio16 <- (prc$oct + prc$nov + prc$dec)
  } else if (qrt.prc$colMax=="ndj"){
    clim$bio16 <- (prc$nov + prc$dec + prc$jan)
  } else if (qrt.prc$colMax=="djf"){
    clim$bio16 <- (prc$dec + prc$jan + prc$feb)
  }
  
  # Precipitation of Driest Quarter (BIO17)
  if(qrt.prc$colMin=="jfm"){
    clim$bio17 <- (prc$jan + prc$feb + prc$mar)
  } else if (qrt.prc$colMin=="fma"){
    clim$bio17 <- (prc$feb + prc$mar + prc$apr)
  } else if (qrt.prc$colMin=="mam"){
    clim$bio17 <- (prc$mar + prc$apr + prc$may)
  } else if (qrt.prc$colMin=="amj"){
    clim$bio17 <- (prc$apr + prc$may + prc$jun)
  } else if (qrt.prc$colMin=="mjj"){
    clim$bio17 <- (prc$may + prc$jun + prc$jul)
  } else if (qrt.prc$colMin=="jja"){
    clim$bio17 <- (prc$jun + prc$jul + prc$aug)
  } else if (qrt.prc$colMin=="jas"){
    clim$bio17 <- (prc$jul + prc$aug + prc$sep)
  } else if (qrt.prc$colMin=="aso"){
    clim$bio17 <- (prc$aug + prc$sep + prc$oct)
  } else if (qrt.prc$colMin=="son"){
    clim$bio17 <- (prc$sep + prc$oct + prc$nov)
  } else if (qrt.prc$colMin=="ond"){
    clim$bio17 <- (prc$oct + prc$nov + prc$dec)
  } else if (qrt.prc$colMin=="ndj"){
    clim$bio17 <- (prc$nov + prc$dec + prc$jan)
  } else if (qrt.prc$colMin=="djf"){
    clim$bio17 <- (prc$dec + prc$jan + prc$feb)
  }
  
  # Precipitation of Warmest Quarter (BIO18)
  if(qrt.tmp$colMax=="jfm"){
    clim$bio18 <- (prc$jan + prc$feb + prc$mar)
  } else if (qrt.tmp$colMax=="fma"){
    clim$bio18 <- (prc$feb + prc$mar + prc$apr)
  } else if (qrt.tmp$colMax=="mam"){
    clim$bio18 <- (prc$mar + prc$apr + prc$may)
  } else if (qrt.tmp$colMax=="amj"){
    clim$bio18 <- (prc$apr + prc$may + prc$jun)
  } else if (qrt.tmp$colMax=="mjj"){
    clim$bio18 <- (prc$may + prc$jun + prc$jul)
  } else if (qrt.tmp$colMax=="jja"){
    clim$bio18 <- (prc$jun + prc$jul + prc$aug)
  } else if (qrt.tmp$colMax=="jas"){
    clim$bio18 <- (prc$jul + prc$aug + prc$sep)
  } else if (qrt.tmp$colMax=="aso"){
    clim$bio18 <- (prc$aug + prc$sep + prc$oct)
  } else if (qrt.tmp$colMax=="son"){
    clim$bio18 <- (prc$sep + prc$oct + prc$nov)
  } else if (qrt.tmp$colMax=="ond"){
    clim$bio18 <- (prc$oct + prc$nov + prc$dec)
  } else if (qrt.tmp$colMax=="ndj"){
    clim$bio18 <- (prc$nov + prc$dec + prc$jan)
  } else if (qrt.tmp$colMax=="djf"){
    clim$bio18 <- (prc$dec + prc$jan + prc$feb)
  }
  
  # Precipitation of Coldest Quarter (BIO19)
  if(qrt.tmp$colMin=="jfm"){
    clim$bio19 <- (prc$jan + prc$feb + prc$mar)
  } else if (qrt.tmp$colMin=="fma"){
    clim$bio19 <- (prc$feb + prc$mar + prc$apr)
  } else if (qrt.tmp$colMin=="mam"){
    clim$bio19 <- (prc$mar + prc$apr + prc$may)
  } else if (qrt.tmp$colMin=="amj"){
    clim$bio19 <- (prc$apr + prc$may + prc$jun)
  } else if (qrt.tmp$colMin=="mjj"){
    clim$bio19 <- (prc$may + prc$jun + prc$jul)
  } else if (qrt.tmp$colMin=="jja"){
    clim$bio19 <- (prc$jun + prc$jul + prc$aug)
  } else if (qrt.tmp$colMin=="jas"){
    clim$bio19 <- (prc$jul + prc$aug + prc$sep)
  } else if (qrt.tmp$colMin=="aso"){
    clim$bio19 <- (prc$aug + prc$sep + prc$oct)
  } else if (qrt.tmp$colMin=="son"){
    clim$bio19 <- (prc$sep + prc$oct + prc$nov)
  } else if (qrt.tmp$colMin=="ond"){
    clim$bio19 <- (prc$oct + prc$nov + prc$dec)
  } else if (qrt.tmp$colMin=="ndj"){
    clim$bio19 <- (prc$nov + prc$dec + prc$jan)
  } else if (qrt.tmp$colMin=="djf"){
    clim$bio19 <- (prc$dec + prc$jan + prc$feb)
  }
  
  return(clim)
}




