\name{rosePlot}
\alias{rosePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rosePlot(dF, names, values, maxValue = NA, textSize = 1, textCol = "black", ringWidth = 1, ringCol = "grey", bgCol = "white", gridInterval = 1, gridCol = "white", gridThick = 1, title = NA, palette = "rainbow")
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
  \item{textSize}{
%%     ~~Describe \code{textSize} here~~
}
  \item{textCol}{
%%     ~~Describe \code{textCol} here~~
}
  \item{ringWidth}{
%%     ~~Describe \code{ringWidth} here~~
}
  \item{ringCol}{
%%     ~~Describe \code{ringCol} here~~
}
  \item{bgCol}{
%%     ~~Describe \code{bgCol} here~~
}
  \item{gridInterval}{
%%     ~~Describe \code{gridInterval} here~~
}
  \item{gridCol}{
%%     ~~Describe \code{gridCol} here~~
}
  \item{gridThick}{
%%     ~~Describe \code{gridThick} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{palette}{
%%     ~~Describe \code{palette} here~~
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
function (dF, names, values, maxValue = NA, textSize = 1, textCol = "black", 
    ringWidth = 1, ringCol = "grey", bgCol = "white", gridInterval = 1, 
    gridCol = "white", gridThick = 1, title = NA, palette = "rainbow") 
{
    if (is.na(maxValue)) 
        maxValue <- max(dF[[values]], na.rm = TRUE)
    fMaxRadius <- maxValue
    fMaxWindow <- fMaxRadius + ringWidth * fMaxRadius/10
    fCircleOuter <- fMaxWindow + ringWidth * fMaxRadius/10
    fCentX <- 0
    fCentY <- 0
    numValues <- nrow(dF)
    if (length(palette) > 1) 
        iColours <- as.character(palette)
    else if (palette == "rainbow") {
        iColours <- rainbow(numValues)
    }
    else if (palette == "grey") {
        iColours <- grey(seq(0.1, 0.9, by = 0.8/numValues))
    }
    plot.new()
    plot.window(xlim = c(-fMaxWindow, fMaxWindow), ylim = c(-fMaxWindow, 
        fMaxWindow), asp = 1)
    symbols(x = fCentX, y = fCentY, circles = fCircleOuter, bg = ringCol, 
        fg = "white", add = TRUE, inches = FALSE)
    symbols(x = fCentX, y = fCentY, circles = fMaxRadius, bg = bgCol, 
        fg = "white", add = TRUE, inches = FALSE)
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
        polygon(c(P$x, fCentX), c(P$y, fCentY), col = iColours[sliceNum], 
            border = gridCol)
        gridIntervals <- seq(from = 0, to = fMaxRadius, by = gridInterval)
        numIntervals <- length(gridIntervals)
        symbols(x = rep(fCentX, numIntervals), y = rep(fCentY, 
            numIntervals), circles = gridIntervals, bg = NA, 
            fg = gridCol, lwd = gridThick, add = TRUE, inches = FALSE)
        middle <- 2 * pi * (cumulatProps[sliceNum] + sliceProp/2)
        radius <- fMaxRadius + 0.5 * (fCircleOuter - fMaxRadius)
        label <- as.character(dF[[names]][sliceNum])
        arctext2(label, center = c(fCentX, fCentY), radius = radius, 
            stretch = 1, cex = textSize, col = textCol, middle = middle, 
            orient = "upright")
    }
    mtext(title)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
