\name{arctext2}
\alias{arctext2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arctext2(x, center = c(0, 0), radius = 1, start = NA, middle = pi/2, end = NA, stretch = 1, orient = "clock", cex = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{center}{
%%     ~~Describe \code{center} here~~
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{middle}{
%%     ~~Describe \code{middle} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{stretch}{
%%     ~~Describe \code{stretch} here~~
}
  \item{orient}{
%%     ~~Describe \code{orient} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, center = c(0, 0), radius = 1, start = NA, middle = pi/2, 
    end = NA, stretch = 1, orient = "clock", cex = NA, ...) 
{
    if (orient == "upright" && middle > pi) 
        orient <- "anticlock"
    oldcex <- par("cex")
    if (is.na(cex)) 
        cex <- oldcex
    par(cex = cex)
    xvec <- strsplit(x, "")[[1]]
    if (orient == "anticlock") 
        xvec <- rev(xvec)
    lenx <- length(xvec)
    xwidths <- stretch * strwidth(xvec)
    charangles <- xwidths/radius
    changrang <- range(charangles)
    charangles[charangles < changrang[2]/2] <- changrang[2]/2
    if (!is.na(end)) 
        start <- end + sum(charangles)
    if (is.na(start)) 
        start <- middle + sum(charangles)/2
    if (orient == "anticlock") 
        start <- (start + pi)\%\%(2 * pi)
    charstart <- c(start, start - cumsum(charangles)[-lenx])
    charpos <- charstart - charangles/2
    xylim <- par("usr")
    plotdim <- par("pin")
    ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
    if (orient == "anticlock") 
        radius <- -radius
    for (xchar in 1:lenx) {
        srt <- 180 * charpos[xchar]/pi - 90
        text(center[1] + radius * cos(charpos[xchar]), center[2] + 
            radius * sin(charpos[xchar]) * ymult, xvec[xchar], 
            adj = c(0.5, 0.5), srt = srt, ...)
    }
    par(cex = oldcex)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
