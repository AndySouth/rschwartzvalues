\name{valueSetsPlots}
\alias{valueSetsPlots}
\title{To run valueSets analysis and create plots for all of the segments.
}
\description{
Creates multiple plots per page as a first run through. 
User can do it using the individual functions if they want more control.
}
\usage{
valueSetsPlots(dFraw, numQs = 21, centering = TRUE, ...)
}
\arguments{
  \item{dFraw}{A dataframe containing responses to values questions. 
  A first column specifying a group identifier followed by either 21 or 57 columns with the answers to questions.
  1 row per respondent.
}
  \item{numQs}{The number of questions asked in the survey, current options 57 or 21.
}
  \item{centering}{Whether or not to centre the value sets. Set to TRUE as default. 
}
  \item{\dots}{additional parameters to pass to \code{rosePlot()}
}
}

\value{
returns nothing.
}

\author{
andy south
}


\examples{
data(dFtestData21)
#to create plots for each segment
valueSetsPlots(dFtestData21)
valueSetsPlots(dFtestData21, centering=FALSE)
}

\keyword{ aplot }

