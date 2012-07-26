valueSetsPlots <-
function( dFraw  #dataFrame or could be a text file 
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
  
}
