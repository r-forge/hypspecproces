x <- matrix(c(1:600), 6, 100, dimnames= list(paste("s", c(1:6), sep= ""),
                                           paste("X", c(1:100), sep= "")))
x[, sample(c(1:100), 50, replace= FALSE)] <- NA
x <- cbind(x, matrix(c(1:60), 6, 10))
colnames(x)[101:110] <- letters[1:10]

naInd         <- function(x){
  #             Function to identify spectral bands with NA values
  #             Args
  #               x: a dataframe where the columns containing spectral data are 
  #                  indicated with colname= "X+wavelength". If any(is.na(x))== F
  #                  x is returned      
  #             Value
  #               a list where each element is a vector with indices of the consecutive bands
  #               with no data. 
  #             Hans D Roelofsen, september 2013

  # Check input data
  if(any(is.na(x[, grep("X", colnames(x))])) == F){
    print("This spectral data does not contain NA values")
    return(x)
  }
  if(any(grepl("X", colnames(x))) == F){stop("colnames of x should be of structure X350 etc")}
  
  # Create new numeric where 1 indicates a band with NA data
  na.t          <- numeric(ncol(x[, grep("X", colnames(x))]))
  for(i in 1:length(na.t)){
    na.t[i]       <- ifelse(is.na(x[1, grep("X", colnames(x))[i]]), 1, 0) 
  }
  
  # Identify consecutive ranges of 1's
  dat           <- rle(na.t)
  # Replace the 1's with increasing numbers
  na            <- seq(c(1:length(dat$values)))
  na[which(dat$values == 0)] <- NA
  dat$values    <- na
  # Reconstruct the vector with new values
  ind           <- factor(inverse.rle(dat))
  
  # Create empty list where each element is a vector of consecutive bands with NA's
  na.ind        <- list()
  for(i in 1:length(levels(ind))){
    na.ind[[i]]      <- which(ind == levels(ind)[i])
    names(na.ind)[i] <- paste("p", i, sep= "")
  }
  return(na.ind)
}