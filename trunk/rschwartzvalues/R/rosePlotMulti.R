rosePlotMulti <-
function( dF  #dataFrame
                           , names #name of the names column
                           , valuesColumns = c(3,4) #which column numbers to plot default of 3 & 4th column
                           , textSize=0.3 #default small text size for multi-plots  
                           , ... #additional parameters to pass to rosePlot()   
                           )
{
  oldPar <-par() #save previous graphics settings
  
  for( plotNum in 1:length(valuesColumns) )
  {
    if ( plotNum %% 6 == 1) #new sheet at plots 1,7,13 ...
    {
      dev.new()
      par(mar=c(1, 0, 1, 0)) #c(bottom, left, top, right)
      #par(mar=c(3, 0, 3, 0)) #c(bottom, left, top, right)
      nPanels <- layout( cbind(c(0,1,2,3),c(0,4,5,6))
                         , heights=c(lcm(0.5),1,1,1)
                         , widths=c(1,1,1)
                         , respect=FALSE )
    }    
    
    colNum <- valuesColumns[plotNum]
    colName <- names(dF)[colNum]
    #create plot for each column
    rosePlot( dF, names=names, values=colName, title=colName, textSize=textSize, ... )
    
  } 
  
  
  par(oldPar)
}
