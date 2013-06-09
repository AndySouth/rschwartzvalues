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
Version: \tab 1.01\cr
Date: \tab 2013-06-06\cr
License: \tab GPL (>= 2)\cr
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

\examples{
data(dFtestData21)
#to get the numeric results 
valueSets(dFtestData21)
valueSets(dFtestData21, centering=FALSE)
#to create plots for each segment
valueSetsPlots(dFtestData21)
valueSetsPlots(dFtestData21, centering=FALSE)
}
