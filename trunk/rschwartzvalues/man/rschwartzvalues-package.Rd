\name{rschwartzvalues-package}
\alias{rschwartzvalues-package}
\alias{rschwartzvalues}
\docType{package}
\title{
To conduct and visualise Schwartz values analysis.  
}
\description{
Functions to take repsonses to values survey questions, aggregate these into values sets,
and display the results using rose/circumplex/coxcomb plots.
}
\details{
\tabular{ll}{
Package: \tab rschwartzvalues\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-07-25\cr
License: \tab GNU GPL v2\cr
}
1. \code{valueSets()} to estimate Value Sets from responses to questions.
2. \code{rosePlot()} to plot Value Sets results.
3. \code{valueSetsPlots()} to do 1 & 2 together 
}
\author{
andy south

Maintainer: southandy@gmail.com
}
\references{
http://mypersonality.org/wiki/doku.php?id=schwartz_s_values_survey
}
\keyword{ package }
\seealso{

}
\examples{
labels <- (c('Hedonism','Stimulation','Self-Direction','Universalism','Benevolence','Conformity & Tradition','Security','Power','Achievement'))
scores <- c(1,10,2,3,4,5,6,7,8)
scores2 <- c(2,9,3,2,4,10,6,7,4)
dF <- data.frame(labels=labels, scores=scores, scores2=scores2)
rosePlotMulti(dF, names='labels', values=c(2,3))
}
