###############################################################################
### rosePlot() creates a circular plot when passed a dataframe of names and values
### andy south 25/7/2012
### package rschwartzvalues
###############################################################################
rosePlot <- function( dF  #dataFrame
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
  
  #set maxValue from data if not specified
  if (is.na(maxValue)) maxValue <- max(dF[[values]],na.rm=TRUE)
  
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
    
    #plot each slice
    polygon(c(P$x,fCentX),c(P$y,fCentY),col=iColours[sliceNum],border=gridCol)#,col=zColours[sliceNum]) #,col=colours()[tc[i]])
    
    #testing adding an outline for comparisons
    #polygon(c(P$x,fCentX),c(P$y,fCentY),col=NA,border='black')#
    
    #add concentric circular gridlines
    #this can be put above and could be set by the user
    gridIntervals <- seq(from=0,to=fMaxRadius,by=gridInterval)
    numIntervals <- length(gridIntervals)
    symbols(x=rep(fCentX,numIntervals),y=rep(fCentY,numIntervals),circles=gridIntervals,bg=NA,fg=gridCol,lwd=gridThick,add=TRUE,inches=FALSE)
    
    #add the text for the name
    middle <- 2 * pi * (cumulatProps[sliceNum] + sliceProp/2)
    #radius <- fMaxRadius + ringWidth
    radius <- fMaxRadius + 0.5 * (fCircleOuter-fMaxRadius)
    #arctext(iNames[sliceNum],center=c(fCentX,fCentY),radius=radius,stretch=1,cex=1.2,col='black',middle=middle)
    
    #getting the lower labels upright
    label <- as.character(dF[[names]][sliceNum])
    arctext2(label,center=c(fCentX,fCentY),radius=radius,stretch=1,cex=textSize,col=textCol,middle=middle,orient='upright')
    
    #if I was going to use updated arctext
    #clockwise <- FALSE
    #if (middle<pi) clockwise <- TRUE 
    #arctext(iNames[sliceNum],center=c(fCentX,fCentY),radius=radius,stretch=1,cex=1.2,col='black',middle=middle,clockwise=clockwise)
    
  } #end of each slice in a circle
  
  #just a quick & easy title at top
  mtext(title) #,line=3)
  
  #savePlot("roseBase1.png",type='png')
  #savePlot("roseBase1.pdf",type='pdf')
  
} #end of rosePlot function