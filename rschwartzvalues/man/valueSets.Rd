\name{valueSets}
\alias{valueSets}

\title{ To calculate value sets from responses to value questions.
}
\description{
Calculates 'Value sets' from a larger number of question responses. 
Has default lookup tables to deal with 57 question Schwartz values survey and
a reduced 21 question survey asked as a part of the 2011 Welsh Assembly Government Sustainability Survey.
Each value set is calculated from a mean value of the responses to contributing questions (specified in the lookup tables).
}
\usage{
valueSets(dFraw, numQs = 21, centering = TRUE)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }

