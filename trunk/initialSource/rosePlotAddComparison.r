###############################################################################
### rosePlotAddComparison() to add a comparison outline to an existing rose plot
### assumes names are the same as previous
### get it to check correct num values and maybe the order of names
### need to be careful what to do with maxValue - it can't be different from previous plot
### andy south 25/7/2012
### package rschwartzvalues
###############################################################################
rosePlotAddComparison <- function( dF  #dataFrame
                                   , names #name of the names column
                                   , values #name of the values column  
                                   , maxValue = NA #default of NA and is set to max in the data  
                                   ) {
  
  #set maxValue from data if not specified
  if (is.na(maxValue)) maxValue <- max(dF[[values]],na.rm=TRUE)
  
  #! test that numValues and names are the same
  
  
  
  fCentX <- 0
  fCentY <- 0 
  numValues <- nrow(dF)
  
  
  #angle of each slice is equal
  sliceProp <- 1 / numValues
  cumulatProps <- seq(0,1,sliceProp)
  
  #as a first test set the radii to iValues
  sliceRadii <- dF[[values]]
  
  #for each slice
  for ( sliceNum in 1:length(sliceRadii) ) {
    
    #number of points on the circumference of each slice
    #set to the proportion * 360
    n <- sliceProp * 360
    
    #P contains coordinates for the circumference bit of the slice in $x & $y
    P <- list( x= sliceRadii[sliceNum] * cos(2*pi*seq(cumulatProps[sliceNum],cumulatProps[sliceNum+1],length=n,na.rm=TRUE))+ fCentX,
               y= sliceRadii[sliceNum] * sin(2*pi*seq(cumulatProps[sliceNum],cumulatProps[sliceNum+1],length=n,na.rm=TRUE))+ fCentY )
    
    #cat("slice coords", P,"\n")
    
    #testing adding an outline for comparisons
    polygon(c(P$x,fCentX),c(P$y,fCentY),col=NA,border='black',lwd=2)#
    
    #add the concentric white circular gridlines
    #this can be put above and could be set by the user
    #iGridIntervals <- seq(0,fMaxRadius,fMaxRadius/10)
    #iNumIntervals <- length(iGridIntervals)
    #symbols(x=rep(fCentX,iNumIntervals),y=rep(fCentY,iNumIntervals),circles=iGridIntervals,bg=NA,fg='white',add=TRUE,inches=FALSE)
    
  } #end of each slice in a circle 
} #end of rosePlotAddComparison function