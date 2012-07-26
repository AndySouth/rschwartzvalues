\name{valueSets}
\alias{valueSets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
valueSets(dFraw, numQs = 21, centering = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dFraw}{
%%     ~~Describe \code{dFraw} here~~
}
  \item{numQs}{
%%     ~~Describe \code{numQs} here~~
}
  \item{centering}{
%%     ~~Describe \code{centering} here~~
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
function (dFraw, numQs = 21, centering = TRUE) 
{
    if (numQs == 21) {
        data(dFlookup21)
        dFlookup <- dFlookup21
    }
    else if (numQs == 57) {
        data(dFlookup57)
        dFlookup <- dFlookup57
    }
    else warning("numQs needs to be 21 or 57 later other options can be added")
    dFrawMeans <- aggregate(dFraw[2:22], by = dFraw[1], mean)
    row.names(dFrawMeans) <- dFrawMeans[, 1]
    dFrawMeans <- dFrawMeans[, -1]
    meanQs <- t(dFrawMeans)
    dFt <- data.frame(meanQs)
    dFout <- dFlookup[-which(duplicated(dFlookup$setName)), -c(1, 
        2, 3)]
    for (i in 1:length(dFt)) dFout[[names(dFt)[i]]] <- NA
    meanTot <- mean(colMeans(dFraw[, -1]))
    meanPerSeg <- sapply(dFt, FUN = mean)
    meanDif <- meanPerSeg - meanTot
    for (setNum in 1:nrow(dFout)) {
        setID <- dFout$setID[setNum]
        qNums <- which(dFlookup$setID == setID)
        results <- sapply(dFt[qNums, ], FUN = mean)
        if (centering) 
            results <- results + meanDif
        dFout[setNum, names(results)] <- results
    }
    return(dFout)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
