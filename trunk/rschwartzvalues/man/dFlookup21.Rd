\name{dFlookup21}
\alias{dFlookup21}
\docType{data}
\title{ A lookup table specifying which of 21 values questions contribute to each of 9 Schwartz value sets. 
}
\description{
This lookup table allows other functions to calculate Schwartz value sets from the repsonses to 21 questions asked as a
part of the 2011 Welsh Assembly Government Sustainability Survey.
}
\usage{data(dFlookup21)}
\format{
  A data frame with 21 observations on the following 5 variables.
  \describe{
    \item{\code{questionIndex}}{an index number for each question}
    \item{\code{questionName}}{The name of each question \code{An exciting life } \code{Being curious } \code{Being forgiving } \code{Being honest } \code{Being independent } \code{Being influential} \code{Being loyal } \code{Being moderate } \code{Being obedient } \code{Being successful } \code{Enjoying life } \code{Family security } \code{Having authority } \code{Honouring parents and elders } \code{Pleasure } \code{Protecting the environment } \code{Respect for tradition} \code{Sense of belonging } \code{Social justice} \code{Unity with nature } \code{Wealth }}
    \item{\code{questionExtra}}{Additional explanantion for each question \code{(achieving goals)} \code{(avoiding extremes of action and feeling)} \code{(correcting injustice, care for the weak)} \code{(dutiful, meeting obligations)} \code{(enjoying food, sex, leisure etc)} \code{(faithful to my friends, group)} \code{(feeling at one with nature)} \code{(feeling that others care about me)} \code{(genuine, sincere)} \code{(gratification of desires)} \code{(having an impact on people and events)} \code{(interested in everything, exploring)} \code{(material possessions, money)} \code{(preservation of time-honoured customs)} \code{(preserving nature)} \code{(safety for loved ones)} \code{(self-reliant, self-sufficient)} \code{(showing respect)} \code{(stimulating experiences)} \code{(the right to lead or command)} \code{(willing to make allowances for others)}}
    \item{\code{setID}}{A code for the value set that each question corresponds to (2 letters). \code{ac} \code{be} \code{co} \code{he} \code{po} \code{sd} \code{se} \code{st} \code{tr} \code{un}}
    \item{\code{setName}}{Name of the value set that each question corresponds to. \code{Achievement} \code{Benevolence} \code{Conformity} \code{Hedonism} \code{Power} \code{Security} \code{Self-Direction} \code{Stimulation} \code{Tradition} \code{Universalism}}
  }
}


\examples{
data(dFlookup21)
dFlookup21
}
\keyword{datasets}
