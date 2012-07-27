\name{rosePlotAddComparison}
\alias{rosePlotAddComparison}

\title{ To add an outline for another dataset to an existing rose plot to facilitate comparison.
}
\description{
An additional test idea to facilitate the comparison of value sets for 2 individuals or groups. 
Once a plot has been produced using \code{rosePlot} this can be called with a 2nd dataset and the outline 
is plotted on top of the existing plot. BEWARE that maxValue must be set to the same as for the original plot.
NOT FULLY IMPLEMENTED YET. 
}
\usage{
rosePlotAddComparison(dF, names, values, maxValue = NA)
}

\arguments{
  \item{dF}{a dataframe containing at least 2 columns, one numeric specifying the diameter of each petal, the other is used to label the circumference of each petal.
}
  \item{names}{the name of the dataframe column containing the names to be used as labels
}
  \item{values}{the name of the dataframe column containing the values to set the relative diameter of each petal
}
  \item{maxValue}{the value to correspond to the maximum diameter of the plot. This must be the same as for the 
}
}

\author{
andy south
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
