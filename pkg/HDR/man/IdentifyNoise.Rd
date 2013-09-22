\name{IdentifyNoise}
\alias{IdentifyNoise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify noise in hyperspectral reflectance data.
}
\description{
This function plots average, min and max reflectance and lets the user iteratively identify spectral regions with low SN ratio. 
}
\usage{
IdentifyNoise(x, waves)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a dataframe containing reflectance data in n= ncol(x) bands, for m= nrow(x) samples. 
}
  \item{waves}{
a vector containing the wavelengths of each spectral band
}
}
\details{
colnames(x) should contain the wavelength of each spectral band, in the form: colnames(x)= paste("X", wavelength, sep= "")
}
\value{
badwl: a vector containing the wavelength of each spectral band that was identified with low SN ratio
}
\references{
hdroelofsen.nl
}
\author{
Hans D Roelofsen
}
\note{
Written for spectral processing library of Laboratory of Geo Information and Remote Sensing, Wageningen University
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, waves) 
{
    setwd("D:/dropbox/A_Werk/F_Non-project/AlgemeenWUR/SpecRScripts/SampleData/")
    dat <- read.table("SampleHySpec.ASC", header = T, sep = ";")
    wavelengths <- dget("SampleWavelengths.ASC")
    PlotRefl <- function(x, waves) {
        if (length(waves) != ncol(x)) {
            stop("nbands of x and wavelenghs do not match")
        }
        graphics.off()
        x11()
        par(tck = 0.005)
        plot(waves, apply(x, 2, mean), type = "l", ylim = c(0, 
            1), axes = F, xlim = c(min(waves), max(waves)), xlab = NA, 
            ylab = NA, xaxs = "i", yaxs = "i")
        xaxat <- seq(min(waves[waves\%\%50 == 0]), max(waves[waves\%\%50 == 
            0]), by = 50)
        abline(v = xaxat, col = "lightgray", lty = "dotted")
        xaxlab <- xaxat
        xaxlab[xaxlab\%\%100 != 0] <- NA
        axis(1, at = xaxat, labels = xaxlab, cex.axis = 0.75, 
            las = 2, pos = 0)
        axis(2, cex.axis = 0.75, pos = min(waves), las = 2)
        par(tck = 0.02)
        axis(1, at = xaxlab, pos = 0, labels = NA)
        title(xlab = "Wavelength", ylab = "Reflection")
        lines(waves, apply(x, 2, min), col = "darkgray")
        lines(waves, apply(x, 2, max), col = "darkgray")
        box()
        legend("topleft", c("mean", "min- max"), col = c("black", 
            "darkgray"), bty = "n", cex = 0.75, lty = rep("solid", 
            2))
    }
    good <- "n"
    while (good == "n") {
        PlotRefl(x = dat, waves = wavelengths)
        th <- as.numeric(unlist(strsplit(readline("Enter wavelength ranges to be removed, seperated by a comma: "), 
            ",")))
        if (length(th)\%\%2 != 0) {
            stop("number of range indicators should be multiple of 2")
        }
        bad.wl <- numeric()
        for (i in seq(1, length(th), by = 2)) {
            rect(th[i], 0, th[i + 1], 1, col = "#0000ff22", border = NA)
            bad.wl <- append(bad.wl, c(seq(th[i], th[i + 1])))
        }
        good <- readline("Is this an acceptable range of wavelengths to filter? y/n ")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~hyperspectral }
\keyword{ ~noise }% __ONLY ONE__ keyword per line
