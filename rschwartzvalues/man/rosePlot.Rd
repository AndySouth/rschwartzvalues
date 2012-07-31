\name{rosePlot}
\alias{rosePlot}
\title{ to create a circular plot with petals of different diameters 
}
\description{
When passed a dataframe of names and values the function creates a circular rose plot. 
The number of petals is determined by the number of values, and the diameter of each petal by the relative magnitude of the values.
The names passed are displayed around the circumference of the plot. 
}
\usage{
rosePlot(dF, names, values, maxValue = NA, textSize = 1, textCol = "black"
        , ringWidth = 1, ringCol = "grey", bgCol = "white", gridInterval = 1
        , gridCol = "white", gridThick = 1, title = NA, palette = "rainbow")
}

\arguments{
  \item{dF}{ a dataframe containing at least 2 columns, one numeric specifying the diameter of each petal, the other is used to label the circumference of each petal.
  }
  \item{names}{the name of the dataframe column containing the names to be used as labels
}
  \item{values}{the name of the dataframe column containing the values to set the relative diameter of each petal
}
  \item{maxValue}{the value to correspond to the maximum diameter of the plot. If set to NA the maximum value is got from the data.
  Setting to other than NA allows the scaling to be kept constant between multiple plots
}
  \item{textSize}{a multiplier of the default text size for the circumference labels (default = 1)
}
  \item{textCol}{colour of text for the circumference labels
}
  \item{ringWidth}{width of the coloured ring around the circumference of the plot. (default = 1)
}
  \item{ringCol}{colour of the ring around the circumference of the plot
}
  \item{bgCol}{background colour, default='white'
}
  \item{gridInterval}{interval for the circular grid lines, default = 1, set to 0 for no gridlines
}
  \item{gridCol}{colour of the circular gridlines, default='white'
}
  \item{gridThick}{thickness of the circular gridlines (default = 1)
}
  \item{title}{a simple title to add to the plot
}
  \item{palette}{colours for the petals, options a)a vector of colours, b) 'rainbow', c) 'grey'. default='rainbow'
}
}
\details{ Can use the output from \code{valueSets}
}
\value{ returns nothing
}

\author{
andy south
}

\examples{
labels <- (c('Hedonism','Stimulation','Self-Direction','Universalism','Benevolence'
            ,'Conformity & Tradition','Security','Power','Achievement'))
scores <- c(1,10,2,3,4,5,6,7,8)
dF <- data.frame(labels=labels, scores=scores)
rosePlot(dF, names='labels', values='scores' )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

