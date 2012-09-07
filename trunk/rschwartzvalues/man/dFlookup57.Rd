\name{dFlookup57}
\alias{dFlookup57}
\docType{data}
\title{ A lookup table specifying which of 57 values questions contribute to each of 9 Schwartz value sets. 
}
\description{
This lookup table allows other functions to calculate Schwartz value sets from the repsonses to the 57 questions of the Schwartz values survey.
}
\usage{data(dFlookup57)}
\format{
  A data frame with 57 observations on the following 5 variables.
  \describe{
    \item{\code{questionIndex}}{a numeric vector}
    \item{\code{questionName}}{a factor with levels \code{A SPIRITUAL LIFE (emphasis on spiritual not material matters)} \code{A VARIED LIFE (filled with challenge, novelty and change)} \code{A WORLD AT PEACE (free of war and conflict)} \code{A WORLD OF BEAUTY (beauty of nature and the arts)} \code{ACCEPTING MY PORTION IN LIFE (submitting to life's circumstances)} \code{AMBITIOUS (hard working, aspiring)} \code{AN EXCITING LIFE (stimulating experiences)} \code{AUTHORITY (the right to lead or command)} \code{BROADMINDED (tolerant of different ideas and beliefs)} \code{CAPABLE (competent, effective, efficient)} \code{CHOOSING OWN GOALS (selecting own purposes)} \code{CLEAN (neat, tidy)} \code{CREATIVITY (uniqueness, imagination)} \code{CURIOUS (interested in everything, exploring)} \code{DARING (seeking adventure, risk)} \code{DEVOUT (holding to religious faith & belief)} \code{ENJOYING LIFE (enjoying food, sex, leisure, etc.)} \code{EQUALITY (equal opportunity for all)} \code{FAMILY SECURITY (safety for loved ones)} \code{FORGIVING (willing to pardon others)} \code{FREEDOM (freedom of action and thought)} \code{HEALTHY (not being sick physically or mentally)} \code{HELPFUL (working for the welfare of others)} \code{HONEST (genuine, sincere)} \code{HONORING OF PARENTS AND ELDERS (showing respect)} \code{HUMBLE (modest, self effacing)} \code{INDEPENDENT (self reliant, self sufficient)} \code{INFLUENTIAL (having an impact on people and events)} \code{INNER HARMONY (at peace with myself)} \code{INTELLIGENT (logical, thinking)} \code{LOYAL (faithful to my friends, group)} \code{MATURE LOVE (deep emotional & spiritual intimacy)} \code{MEANING IN LIFE (a purpose in life)} \code{MODERATE (avoiding extremes of feeling & action)} \code{NATIONAL SECURITY (protection of my nation from enemies)} \code{OBEDIENT (dutiful, meeting obligations)} \code{PLEASURE (gratification of desires)} \code{POLITENESS (courtesy, good manners)} \code{PRESERVING MY PUBLIC IMAGE (protecting my \"face\")} \code{PRIVACY (the right to have a private sphere)} \code{PROTECTING THE ENVIRONMENT (preserving nature)} \code{RECIPROCATION OF FAVORS (avoidance of indebtedness)} \code{RESPECT FOR TRADITION (preservation of time honored customs)} \code{RESPONSIBLE (dependable, reliable)} \code{SELF-INDULGENT (doing pleasant things)} \code{SELF DISCIPLINE (self restraint, resistance to temptation)} \code{SELF RESPECT (belief in one's own worth)} \code{SENSE OF BELONGING (feeling that others care about me)} \code{SOCIAL JUSTICE (correcting injustice, care for the weak)} \code{SOCIAL ORDER (stability of society)} \code{SOCIAL POWER (control over others, dominance)} \code{SOCIAL RECOGNITION (respect, approval by others)} \code{SUCCESSFUL (achieving goals)} \code{TRUE FRIENDSHIP (close, supportive friends)} \code{UNITY WITH NATURE (fitting into nature)} \code{WEALTH (material possessions, money)} \code{WISDOM (a mature understanding of life)}}
    \item{\code{questionExtra}}{Additional explanantion for each question}
    \item{\code{setID}}{a factor with levels \code{} \code{ac} \code{be} \code{co} \code{he} \code{po} \code{sd} \code{se} \code{st} \code{tr} \code{un}}
    \item{\code{setName}}{a factor with levels \code{ } \code{Achievement} \code{Benevolence} \code{Conformity} \code{Hedonism} \code{Power} \code{Security} \code{Self-Direction} \code{Stimulation} \code{Tradition} \code{Universalism}}
  }
}

\source{
http://mypersonality.org/wiki/doku.php?id=schwartz_s_values_survey
}

\examples{
data(dFlookup57)
dFlookup57
}
\keyword{datasets}
