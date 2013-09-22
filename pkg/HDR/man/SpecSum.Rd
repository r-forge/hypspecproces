\name{SpecSum}
\alias{SpecSum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of hyperspectral data into broadband spectral regions
}
\description{
This function generates average reflection over several broadband spectral regions: Green, Red, NIR, SWIR1, SWIR2 and NDVI
}
\usage{
SpecSum(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a dataframe containing reflectance data in n= ncol(x) bands, for m= nrow(x) samples. 
}
}
\details{
colnames(x) should contain the wavelength of each spectral band, in the form: colnames(x)= paste("X", wavelength, sep= "")
}
\value{
x with averaged reflection in Green, Red, NIR, SWIR1, SWIR2 and NDVI per sample appended to the dataframe. ncol(x-output)= ncol(x-input)+6
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
function (x) 
{
    spec <- as.numeric(substring(colnames(x)[grep("X", colnames(x))], 
        2))
    g <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        520))], sep = "")):which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        570))], sep = "")))
    x$G <- apply(x[, g], 1, mean)
    r <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        600))], sep = "")):which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        650))], sep = "")))
    x$R <- apply(x[, r], 1, mean)
    nir <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        800))], sep = "")):which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        900))], sep = "")))
    x$NIR <- apply(x[, nir], 1, mean)
    swir1 <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        1600))], sep = "")):which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        1700))], sep = "")))
    x$SWIR1 <- apply(x[, swir1], 1, mean)
    swir2 <- c(which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        2100))], sep = "")):which(colnames(x) == paste("X", spec[which.min(abs(spec - 
        2300))], sep = "")))
    x$SWIR2 <- apply(x[, swir2], 1, mean)
    x$ndvi <- (x$NIR - x$R)/(x$NIR + x$R)
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~reflectance }
\keyword{ ~hyperspectral }
