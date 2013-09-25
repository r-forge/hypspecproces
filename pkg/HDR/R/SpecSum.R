SpecSum <-
function(x){
#               Function to sumarize spectral data into several broadband regions. 
#                 
#               Argumentss: x - a dataframe containing spectral data. 
#                           Colnames(x)= paste("X", wavelength, sep= "")
#
#               Value
#               x, with average reflection in Green (520-570nm)
#                                             Red (600-650nm)
#                                             NIR (800-900nm)
#                                             SWIR1 (1600-1700nm) 
#                                             SWIR2 (2100-2300nm) 
#                                             NDVI (NIR - R / NIR + R)
#               added as new columns to x.               
#  
#               Hans D Roelofsen, 23 July 2013
  
  #             Identify the columns in x with spectral data
  spec          <- as.numeric(substring(colnames(x)[grep("X", colnames(x))], 2))
  
  #             Identify the columns in the desired spectral wavelength range. The first and last
  #               collumn nearest to the start and end wavelengths are identified. 
  g             <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 520) )], sep= "")): 
                     which(colnames(x) == paste("X", spec[which.min(abs(spec - 570) )], sep= "")))  
  x$G           <- apply(x[, g], 1, mean) # Green 
  
  r             <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 600) )], sep= "")): 
                     which(colnames(x) == paste("X", spec[which.min(abs(spec - 650) )], sep= "")))
  x$R           <- apply(x[, r], 1, mean) # Red
  
  nir           <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 800) )], sep= "")): 
                     which(colnames(x) == paste("X", spec[which.min(abs(spec - 900) )], sep= "")))
  x$NIR         <- apply(x[,nir], 1, mean) # NIR
  
  swir1         <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 1600) )], sep= "")): 
                     which(colnames(x) == paste("X", spec[which.min(abs(spec - 1700) )], sep= "")))
  x$SWIR1       <- apply(x[, swir1], 1, mean) # SWIR1
  
  swir2         <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 2100) )], sep= "")): 
                       which(colnames(x) == paste("X", spec[which.min(abs(spec - 2300) )], sep= "")))
  x$SWIR2       <- apply(x[, swir2], 1, mean) # SWIR2

  x$ndvi        <- (x$NIR - x$R) / (x$NIR + x$R) # NDVI
  
  return(x)
}


# Test edits, door HDR! 22-Sep-2013 13:53
# Test edits, Jan