rosePlot <-
function( dF  #dataFrame
                      , names #name of the names column
                      , values #name of the values column  
                      , maxValue = NA #default of NA and is set to max in the data
                      , textSize = 1 # multiple for text around the edge
                      , textCol = 'black' #colour of text around edge
                      , ringWidth = 1 #making this much greater than 1 will make it go outside the plot  
                      , ringCol = 'grey' #colour of the ring around the plot 
                      , bgCol = 'white' #background colour of the plot  
                      , gridInterval = 1 #circular grid set to 0 for no grid
                      , gridCol = 'white' #colour of circular grid
                      , gridThick = 1  #thickness of gridLines
                      , title = NA #just a quick & easy title at top
                      , palette ='rainbow'#options a)vector of colours, b) 'rainbow', c) 'grey' 
                      ) {
  
  #checking that the specified names and values columns exist in th data
  if ( ! names %in% names(dF) ) 
    stop("your names column(",names,") does not exist in your data : ", names(dF))
  if ( ! values %in% names(dF) ) 
    stop("your values column(",values,") does not exist in your data : ", names(dF))  
  
  #set maxValue from data if not specified
  if (is.na(maxValue)) maxValue <- max(dF[[values]],na.rm=TRUE)
  
  if (!(maxValue>0)) {
    warning("In rosePlot(), the maximum value for the plot needs to be greater than 0, yours is ",maxValue)
    return()
  }
 
  #just checking minValue for warning
  #plots work with -ve vals but just don't show that petal
  minValue <- min(dF[[values]],na.rm=TRUE) 
  if (minValue<0) {
    warning("In rosePlot(), your data has negative values these will not appear in plots, your min value is ",minValue)
  }  
  
  fMaxRadius <- maxValue #can also have option to set from the max in the data
  #fMaxWindow <- fMaxRadius + 1 #this +1 is lazy
  fMaxWindow <- fMaxRadius + ringWidth*fMaxRadius/10 #10% boundary to include the ring
  fCircleOuter <- fMaxWindow + ringWidth*fMaxRadius/10 #still had to set outer circle bigger
  
  fCentX <- 0
  fCentY <- 0
  
  numValues <- nrow(dF)
  
  #colours for petals
  #this isn't perfect but it does allow
  #a)vector of colours, b) rainbow, c) grey
  
  if(length(palette)>1) iColours<-as.character(palette) else if (palette=='rainbow') {
    iColours <- rainbow(numValues) } else if (palette=='grey') {
      iColours <- grey(seq(0.1,0.9,by=0.8/numValues)) }
  
  
  #par(mai=c(0, 0, 0, 0)) #setting no border 
  plot.new()
  #asp=1 ensures plots stay circular and fit in
  plot.window(xlim=c(-fMaxWindow,fMaxWindow), ylim=c(-fMaxWindow,fMaxWindow), asp=1)
  
  
  #add a ring of colour around the plot
  #this adds one circle and then another of a diff colour inside
  #! I need to make it so width is independent of values passed
  #symbols(x=fCentX,y=fCentY,circles=fMaxWindow+ringWidth,bg=ringCol,fg='white',add=TRUE,inches=FALSE)
  symbols(x=fCentX,y=fCentY,circles=fCircleOuter,bg=ringCol,fg='white',add=TRUE,inches=FALSE)
  symbols(x=fCentX,y=fCentY,circles=fMaxRadius,bg=bgCol,fg='white',add=TRUE,inches=FALSE)
  
  
  #angle of each slice is equal
  sliceProp <- 1 / numValues
  cumulatProps <- seq(0,1,sliceProp)
  #reversing to try to get them to go clockwise
  #cumulatProps <- seq(1,0,-sliceProp)  
  #also I want them to start at 12 oclock rather than 3 
  #so think I need to subtract 0.25
  #causes problems !
  #QcumulatProps <- (cumulatProps+.25) %% 1
  
  #set the radii
  sliceRadii <- dF[[values]]
  #reversing to go clockwise
  #sliceRadii <- rev(dF[[values]])  
  
  #for each slice
  for ( sliceNum in 1:length(sliceRadii) ) {
  #reversing to try to get them to go clockwise
  #for ( sliceNum in length(sliceRadii):1 ) {
    
    #number of points on the circumference of each slice
    #set to the proportion * 360
    n <- sliceProp * 360
    #n <- sliceProp * 10    
    
    #P contains coordinates for the circumference bit of the slice in $x & $y
    #pi/2+ added to start at 12 o'clock
    P <- list( x= sliceRadii[sliceNum] * cos(pi/2+2*pi*seq(cumulatProps[sliceNum],cumulatProps[sliceNum+1],length=n))+ fCentX,
               y= sliceRadii[sliceNum] * sin(pi/2+2*pi*seq(cumulatProps[sliceNum],cumulatProps[sliceNum+1],length=n))+ fCentY )
    
    
    #cat("slice coords", P,"\n")
    
    #plot each slice
    polygon(c(P$x,fCentX),c(P$y,fCentY),col=iColours[sliceNum],border=gridCol)#,col=zColours[sliceNum]) #,col=colours()[tc[i]])
    
    #testing adding an outline for comparisons
    #polygon(c(P$x,fCentX),c(P$y,fCentY),col=NA,border='black')#
    
    #add concentric circular gridlines
    #this can be put above and could be set by the user
    gridIntervals <- seq(from=0,to=fMaxRadius,by=gridInterval)
    numIntervals <- length(gridIntervals)
    symbols(x=rep(fCentX,numIntervals),y=rep(fCentY,numIntervals),circles=gridIntervals,bg=NA,fg=gridCol,lwd=gridThick,add=TRUE,inches=FALSE)
    
    #add the text for the name, pi/2+ added to start at 12 o'clock
    #middle <- pi/2+2*pi * (cumulatProps[sliceNum] + sliceProp/2)
    middle <- (pi/2+2*pi * (cumulatProps[sliceNum] + sliceProp/2)) %% (2*pi)   
    #radius <- fMaxRadius + ringWidth
    radius <- fMaxRadius + 0.5 * (fCircleOuter-fMaxRadius)
    #arctext(iNames[sliceNum],center=c(fCentX,fCentY),radius=radius,stretch=1,cex=1.2,col='black',middle=middle)
    
    #getting the lower labels upright
    label <- as.character(dF[[names]][sliceNum])
    #arctext2(label,center=c(fCentX,fCentY),radius=radius,stretch=1,cex=textSize,col=textCol,middle=middle,orient='upright')   
    #if I was going to use updated arctext
    clockwise <- TRUE
    if (middle>pi) clockwise <- FALSE 
    arctext2(label,center=c(fCentX,fCentY),radius=radius,stretch=1,cex=textSize,col=textCol,middle=middle,clockwise=clockwise)
    
  } #end of each slice in a circle
  
  #just a quick & easy title at top
  mtext(title) #,line=3)
  
  #savePlot("roseBase1.png",type='png')
  #savePlot("roseBase1.pdf",type='pdf')
  
}
