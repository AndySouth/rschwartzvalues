\name{rosePlotMulti}
\alias{rosePlotMulti}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rosePlotMulti(dF, names, valuesColumns = c(3, 4), textSize = 0.3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dF}{
%%     ~~Describe \code{dF} here~~
}
  \item{names}{
%%     ~~Describe \code{names} here~~
}
  \item{valuesColumns}{
%%     ~~Describe \code{valuesColumns} here~~
}
  \item{textSize}{
%%     ~~Describe \code{textSize} here~~
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
function (dF, names, valuesColumns = c(3, 4), textSize = 0.3, 
    ...) 
{
    oldpar <- par()
    for (plotNum in 1:length(valuesColumns)) {
        if (plotNum\%\%6 == 1) {
            dev.new()
            par(mar = c(1, 0, 1, 0))
            nPanels <- layout(cbind(c(0, 1, 2, 3), c(0, 4, 5, 
                6)), heights = c(lcm(0.5), 1, 1, 1), widths = c(1, 
                1, 1), respect = FALSE)
        }
        colNum <- valuesColumns[plotNum]
        colName <- names(dF)[colNum]
        rosePlot(dF, names = names, values = colName, title = colName, 
            textSize = textSize, ...)
    }
    par(oldPar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
