###############################################################################
### valueSetsPlots() to run valueSets analysis
### and create plots for all of the segments
### multiple plots per page as a first run through
### user can do it using the individual functions if they want more control

### andy south 25/7/2012
### package rschwartzvalues
###############################################################################
valueSetsPlots <- function( dFraw  #dataFrame or could be a text file 
                            , numQs = 21 #21 or 57 or allow user to submit their own  
                            , centering = TRUE #whether to centre means by the mean of all per segment
                            , ... #additional parameters to pass to rosePlot function
                            ) 
{
  
  #analyse questions
  dF <- valueSets( dFraw=dFraw, numQs=numQs, centering=centering)
  
  #do multi-plots 
  rosePlotMulti(dF, names="setName", valuesColumns = c(3:length(dF)), ...)
  
  #return the value sets results, invisibly just in case user wants
  invisible(dF)
  
} # end of valueSetsPlots function