IdentifyNoise <-
function(x, waves){
# Function to visually identify spectral bands with low SN ratio from 
#   hyperspectral data
#
# Hans D Roelofsen, July 2013
# 
# Arguments:
# x:      a dataframe containing spectral data. Colnames= X+wavelength
# waves:  a numeric vector with the wavelength positions in nm of each spectral 
#         band. Wavelengths should be integers. length(waves) == ncol(x)
# 
# Value: 
# bad.wl - a numeric vector with the wavelengths that are marked as corrupt
# 
# required library: none
  
  #             helper function that plots mean, min and max of all spectra
  PlotRefl      <- function(refldat, wavelengths){
    if(length(wavelengths) != ncol(refldat)){stop("nbands of x and wavelenghs do not match")}
    
    #             Plots average, min and max of all spectra
    graphics.off()
    x11()
    par(tck= 0.005)
    plot(wavelengths, apply(refldat, 2, mean), type= "l", ylim= c(0,1), axes= F, 
         xlim= c(min(wavelengths), max(wavelengths)), xlab= NA, ylab= NA, xaxs= "i", yaxs= "i")
    
    #             Define tick positions and labels for x axis
    xaxat         <- seq(min(wavelengths[wavelengths %% 50 == 0]), 
                         max(wavelengths[wavelengths %% 50 == 0]), by= 50)
    #             Vertical ablines  
    abline(v= xaxat, col= "lightgray", lty= "dotted")
    #             Define x-axis labels
    xaxlab        <- xaxat
    xaxlab[xaxlab %% 100 != 0] <- NA
    #             Add axes
    axis(1, at= xaxat, labels= xaxlab, cex.axis= 0.75, las= 2, pos= 0)
    axis(2, cex.axis= 0.75, pos= min(waves), las= 2)
    par(tck= 0.02)
    axis(1, at= xaxlab, pos= 0, labels= NA)
    title(xlab= "Wavelength", ylab= "Reflection")
    #             Add min and max spectra
    lines(wavelengths, apply(refldat, 2, min), col= "darkgray")
    lines(wavelengths, apply(refldat, 2, max), col= "darkgray")
    box()
    #             Add legend
    legend("topleft", c("mean", "min- max"), col= c("black", "darkgray"), bty= "n", cex= 0.75,
           lty= rep("solid", 2))
  }
  
  #             Set marker 
  good          <- "n" 
  #             Iterative function to select corrupt wavelengths
  while(good == "n"){
    PlotRefl(refldat= x, wavelengths= waves)
    #             Give the min and max values of the range of wavelengths you
    #               want to remove, seperated by commas
    th            <- as.numeric(unlist(strsplit(readline("Enter wavelength ranges to be removed, seperated by a comma: "), ",")))
    if(length(th) %% 2 != 0){stop("number of range indicators should be multiple of 2")}
    
    #             Create empty numeric that will contain the selected wavelengths
    bad.wl        <- numeric()
    #             Indicate the spectral range legible for selection
    for(i in seq(1, length(th), by= 2)){
      rect(th[i], 0, th[i+1], 1, col="#0000ff22", border= NA)
      #             determine which WaveLengths (wl) are bad
      bad.wl        <- append(bad.wl, c(seq(th[i], th[i+1])))
    }
    #             Iterate untill satisfied with the result
    good          <- readline("Is this an acceptable range of wavelengths to filter? y/n ")
  }
  return(bad.wl)
}
