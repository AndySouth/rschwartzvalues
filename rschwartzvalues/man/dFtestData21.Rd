\name{dFtestData21}
\alias{dFtestData21}
\docType{data}
\title{Test data responses for 21 question Schwarz values analysis.}
\description{
Test data with responses to 21 questions (in columns) on a scale of 1 - 5. 
12 responses, 2 responses per group.
Can be analysed using valueSets() or valueSetsPlots().
}
\usage{data(dFtestData21)}
\format{
  A data frame with 12 observations on the following 22 variables.
  \describe{
    \item{\code{segment}}{a factor with levels \code{group1} \code{group2} \code{group3} \code{group4} \code{group5} \code{group6}}
    \item{\code{q1}}{a numeric vector}
    \item{\code{q2}}{a numeric vector}
    \item{\code{q3}}{a numeric vector}
    \item{\code{q4}}{a numeric vector}
    \item{\code{q5}}{a numeric vector}
    \item{\code{q6}}{a numeric vector}
    \item{\code{q7}}{a numeric vector}
    \item{\code{q8}}{a numeric vector}
    \item{\code{q9}}{a numeric vector}
    \item{\code{q10}}{a numeric vector}
    \item{\code{q11}}{a numeric vector}
    \item{\code{q12}}{a numeric vector}
    \item{\code{q13}}{a numeric vector}
    \item{\code{q14}}{a numeric vector}
    \item{\code{q15}}{a numeric vector}
    \item{\code{q16}}{a numeric vector}
    \item{\code{q17}}{a numeric vector}
    \item{\code{q18}}{a numeric vector}
    \item{\code{q19}}{a numeric vector}
    \item{\code{q20}}{a numeric vector}
    \item{\code{q21}}{a numeric vector}
  }
}

\examples{
data(dFtestData21)
#to get the numeric results 
valueSets(dFtestData21)
valueSets(dFtestData21, centering=FALSE)
#to create plots for each segment
valueSetsPlots(dFtestData21)
valueSetsPlots(dFtestData21, centering=FALSE)
}
\keyword{datasets}
