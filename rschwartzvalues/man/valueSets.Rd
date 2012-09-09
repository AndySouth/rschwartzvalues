\name{valueSets}
\alias{valueSets}

\title{ To calculate value sets from responses to value questions.
}
\description{
Calculates 'Value sets' from a larger number of question responses. 
Has default lookup tables to deal with 57 question Schwartz values survey and
a reduced 21 question survey asked as a part of the 2011 Welsh Assembly Government Sustainability Survey.
Each value set is calculated from a mean value of the responses to contributing questions (specified in the lookup tables).
Questions not answered or 'I don't know' responses should be coded as NA in the input file
, and will be excluded from the mean calculations.
}
\usage{
valueSets(dFraw, numQs = 21, centering = TRUE, addToNeg = TRUE)
}

\arguments{
  \item{dFraw}{A dataframe containing responses to values questions. 
  A first column specifying a group identifier followed by either 21 or 57 columns with the answers to questions.
  Columns must be in the correct order.
  (Will also work for a single group if there is no identifier column).
  1 row per respondent.
  Questions not answered or 'I don't know' responses should be coded as NA in the input file
  , and will be excluded from the mean calculations.
}
  \item{numQs}{The number of questions asked in the survey, current options 57 or 21.
}
  \item{centering}{Whether or not to centre the value sets. Set to TRUE as default. 
}
  \item{addToNeg}{Whether to add 1 to all responses if the mimimum response is -1.
  This will convert a scale of -1 to 9 to one of 0 to 10 which displays better in plots.
  (negative numbers do not show up in rosePlot()) 
  Set to TRUE as default. 
}
}

\value{
Returns a dataframe containing the value sets which can then be plotted using \code{rosePlot()}
}
\references{
http://mypersonality.org/wiki/doku.php?id=schwartz_s_values_survey
}
\author{
andy south
}

\examples{
data(dFtestData21)
#to get the numeric results 
valueSets(dFtestData21)
valueSets(dFtestData21, centering=FALSE)
}

\keyword{ math }

