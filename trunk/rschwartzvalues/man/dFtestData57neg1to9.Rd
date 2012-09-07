\name{dFtestData57neg1to9}
\alias{dFtestData57neg1to9}
\docType{data}
\title{Test data responses for 57 question Schwarz values analysis.}
\description{
Test data with responses to 57 questions (in columns) on a scale of -1 to 9 
randomly generated. 
Can be analysed using valueSets() or valueSetsPlots().
To test valueSets() which adds 1 to all responses if the minimum response is -1.
}
\usage{data(dFtestData57neg1to9)}
\format{
  A data frame with 5 observations on the following 58 variables.
  \describe{
    \item{\code{segment}}{a factor with levels \code{group1} \code{group2}}
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
    \item{\code{q22}}{a numeric vector}
    \item{\code{q23}}{a numeric vector}
    \item{\code{q24}}{a numeric vector}
    \item{\code{q25}}{a numeric vector}
    \item{\code{q26}}{a numeric vector}
    \item{\code{q27}}{a numeric vector}
    \item{\code{q28}}{a numeric vector}
    \item{\code{q29}}{a numeric vector}
    \item{\code{q30}}{a numeric vector}
    \item{\code{q31}}{a numeric vector}
    \item{\code{q32}}{a numeric vector}
    \item{\code{q33}}{a numeric vector}
    \item{\code{q34}}{a numeric vector}
    \item{\code{q35}}{a numeric vector}
    \item{\code{q36}}{a numeric vector}
    \item{\code{q37}}{a numeric vector}
    \item{\code{q38}}{a numeric vector}
    \item{\code{q39}}{a numeric vector}
    \item{\code{q40}}{a numeric vector}
    \item{\code{q41}}{a numeric vector}
    \item{\code{q42}}{a numeric vector}
    \item{\code{q43}}{a numeric vector}
    \item{\code{q44}}{a numeric vector}
    \item{\code{q45}}{a numeric vector}
    \item{\code{q46}}{a numeric vector}
    \item{\code{q47}}{a numeric vector}
    \item{\code{q48}}{a numeric vector}
    \item{\code{q49}}{a numeric vector}
    \item{\code{q50}}{a numeric vector}
    \item{\code{q51}}{a numeric vector}
    \item{\code{q52}}{a numeric vector}
    \item{\code{q53}}{a numeric vector}
    \item{\code{q54}}{a numeric vector}
    \item{\code{q55}}{a numeric vector}
    \item{\code{q56}}{a numeric vector}
    \item{\code{q57}}{a numeric vector}
  }
}

\examples{
data(dFtestData57neg1to9)
#to get the numeric results 
valueSets(dFtestData57neg1to9, numQs=57)
valueSets(dFtestData57neg1to9, numQs=57, centering=FALSE)
#to create plots for each segment
valueSetsPlots(dFtestData57neg1to9, numQs=57)
valueSetsPlots(dFtestData57neg1to9, numQs=57, centering=FALSE)
}

\keyword{datasets}
