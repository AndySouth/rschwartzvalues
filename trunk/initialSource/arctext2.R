#arctext2
#andy south 5/7/2012
#modifying arctext(plotrix) to be able to reverse text on the curve
#xtra argument: 'orient' options : 'clock', 'anticlock', 'upright'
#upright sets to clockwise above circle and anticlockwise below

arctext2<-function(x,center=c(0,0),radius=1,start=NA,middle=pi/2,end=NA,stretch=1,orient='clock',
 cex=NA, ...) {

 #! if label is on lower half of circle set orient to anticlock
 #! doesn't yet deal with if start or end are set
 if (orient=='upright' && middle > pi) orient <- 'anticlock'  
 
 oldcex <- par("cex")
 if (is.na(cex)) cex <- oldcex
 par(cex=cex)
 xvec <- strsplit(x, "")[[1]]
 #! reversing order of text for anticlockwise
 if (orient=='anticlock') xvec <- rev(xvec)
 
 lenx <- length(xvec)
 xwidths <- stretch * strwidth(xvec)
 charangles <- xwidths/radius
 changrang <- range(charangles)
 charangles[charangles < changrang[2]/2] <- changrang[2]/2 # Make really narrow characters wider
 if (!is.na(end)) start <- end + sum(charangles)
 if (is.na(start)) start <- middle + sum(charangles)/2
 #! change start to other side of circle to account for -ve radius
 if (orient=='anticlock') start <- (start+pi)%%(2*pi)
 
 charstart <- c(start, start - cumsum(charangles)[-lenx])
 charpos <- charstart - charangles/2
 xylim <- par("usr")
 plotdim <- par("pin")
 ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
 #! make radius -ve for anticlock
 if (orient=='anticlock') radius <- -radius

 for (xchar in 1:lenx) {
  srt <- 180*charpos[xchar]/pi - 90
  text(center[1] + radius * cos(charpos[xchar]), 
   center[2] + radius * sin(charpos[xchar]) * ymult, xvec[xchar], 
   adj=c(0.5, 0.5), srt=srt, ...)

 }
 par(cex=oldcex)
}

