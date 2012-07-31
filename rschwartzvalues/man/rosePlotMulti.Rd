\name{rosePlotMulti}
\alias{rosePlotMulti}
\title{ To create rose plots for all of the specified columns in a passed dataframe.
}
\description{
Quickly to create a series of rose plots when a dataframe is passed containing one column with the names of petals, 
and a series of columns specifying the petal diameters. One rose plot is created for each column of values. 
}
\usage{
rosePlotMulti(dF, names, valuesColumns = c(3, 4), textSize = 0.3, ...)
}

\arguments{
  \item{dF}{a dataframe containing at least 3 columns, two numeric columns specifying the diameter of petals, another column is used to label the circumference of each petal.
}
  \item{names}{the name of the dataframe column containing the names to be used as labels
}
  \item{valuesColumns}{which column numbers to plot default of 3 & 4th column
}
  \item{textSize}{default small text size of 0.3 for multi-plots
}
  \item{\dots}{additional parameters to pass to \code{rosePlot()}
}
}


\references{
%% ~put references to the literature/web site here ~
}
\author{
andy south
}

\examples{
labels <- (c('Hedonism','Stimulation','Self-Direction','Universalism','Benevolence'
            ,'Conformity & Tradition','Security','Power','Achievement'))
scores <- c(1,10,2,3,4,5,6,7,8)
scores2 <- c(2,9,3,2,4,10,6,7,4)
dF <- data.frame(labels=labels, scores=scores, scores2=scores2)
rosePlotMulti(dF, names='labels', values=c(2,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

