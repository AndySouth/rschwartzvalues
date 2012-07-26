\name{rosePlotAddComparison}
\alias{rosePlotAddComparison}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rosePlotAddComparison(dF, names, values, maxValue = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dF}{
%%     ~~Describe \code{dF} here~~
}
  \item{names}{
%%     ~~Describe \code{names} here~~
}
  \item{values}{
%%     ~~Describe \code{values} here~~
}
  \item{maxValue}{
%%     ~~Describe \code{maxValue} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dF, names, values, maxValue = NA) 
{
    if (is.na(maxValue)) 
        maxValue <- max(dF[[values]], na.rm = TRUE)
    fCentX <- 0
    fCentY <- 0
    numValues <- nrow(dF)
    sliceProp <- 1/numValues
    cumulatProps <- seq(0, 1, sliceProp)
    sliceRadii <- dF[[values]]
    for (sliceNum in 1:length(sliceRadii)) {
        n <- sliceProp * 360
        P <- list(x = sliceRadii[sliceNum] * cos(2 * pi * seq(cumulatProps[sliceNum], 
            cumulatProps[sliceNum + 1], length = n, na.rm = TRUE)) + 
            fCentX, y = sliceRadii[sliceNum] * sin(2 * pi * seq(cumulatProps[sliceNum], 
            cumulatProps[sliceNum + 1], length = n, na.rm = TRUE)) + 
            fCentY)
        polygon(c(P$x, fCentX), c(P$y, fCentY), col = NA, border = "black", 
            lwd = 2)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
