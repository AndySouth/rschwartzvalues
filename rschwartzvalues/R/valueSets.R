
valueSets <-
function( dFraw  #dataFrame 
          , numQs = 21 #21 or 57 or later allow user to submit their own lookup table  
          , columns = NULL
          , centering = TRUE #whether to centre means by the mean of all per segment
          , addToNeg =TRUE #whether to add 1 to all vals if the min is -1
        ) 
{
  
  #first get the lookup table
  #later offer the option to submit own lookup table
  #dFlookup21 <- read.csv('lookupTables//21QuestionsLookup.csv')
  dFlookup21 <- dFlookup57 <- NULL #just to appease R CMD CHECK
  if (numQs == 21) {
    data("dFlookup21", envir = environment(),package = "rschwartzvalues")
    dFlookup <- dFlookup21
  }
  else if (numQs == 57)  {
    data("dFlookup57", envir = environment(),package = "rschwartzvalues")
    dFlookup <- dFlookup57
  }
  else warning("numQs needs to be 21 or 57 later other options can be added")
  

  #allow columns to be subsetted from the input dataframe
  #the first one will still be used as the identifier
  if (length(columns)>1)
  {
    dFraw <- dFraw[,columns]
  }
  
  
  
  #test whether the number of questions in the lookup table and the raw data is the same 
  
  #if the data don't have an identifier column add one on so that later bits work
  if (length(dFraw)==numQs) 
  {
    message("adding an identifier column")
    dFtmp <- cbind('ALL',dFraw)
    dFraw <- dFtmp    
  } else if(length(dFraw)!=(1+numQs))
          warning("your input file needs to have one column per question and an optional identifier column\n numColumns=",length(dFraw)," numQs=",numQs," trying to procede anyway")
  
  
  #assume that the input data has 1 identifier column followed by 21 columns with answer to each question
  #respondents in rows
  #column1 has an identifier to subset data by
  #dFraw[index+1]
 
  #check if data have negative elements
  #original Schwarz scale was -1 to +9, convert this to 0 to 10
  if ( addToNeg && min(dFraw[2:(numQs+1)],na.rm=TRUE) == -1 )
  {
    dFraw[2:(numQs+1)] <- dFraw[2:(numQs+1)] + 1
    message("in valueSets() added one to all responses, to turn this off set addToNeg=FALSE")
  }
  
  
  #first calc the means for each segment for each question
  #dFrawMeans <- aggregate(dFraw[2:(numQs+1)],by=dFraw[1], mean)
  dFrawMeans <- aggregate(dFraw[2:(numQs+1)],by=dFraw[1], mean, na.rm = TRUE)  
  #set row names to the segment names
  row.names(dFrawMeans) <- dFrawMeans[,1]
  #remove the segment names column
  dFrawMeans <- dFrawMeans[,-1]
  #transpose data, to a named matrix, should make manipulation easier
  meanQs <- t(dFrawMeans)
  dFt <- data.frame(meanQs)
  #this correctly gives results for each segment
  #po <- sapply(dFt[c(7,8),],FUN=mean) #hard coded
  
  
  #create a blank dataFrame for the results
  #rows for each value set and columns for segments
  #col1 names of the value sets (and/or abbreviation)
  #col 2, 3 etc value sets for each segment
  #first create a column containing the names of the value sets
  #! be careful that I keep things in correct order - unique does do that 
  #dFout <- data.frame(setID=unique(dFlookup$setID),setName=unique(dFlookup$setName))
  #add extra empty columns for each segment
  #for(i in 1:length(dFt)) dFout[[names(dFt)[i]]] <- NA
  #then put the results in for each set
  #this works if I was hardcoding it
  #dFout[which(dFout$setID=='po'),names(po)] <- po
  
  #An easier way may just be to get unique rows from the lookup table
  #misses out duplicate rows, and the 1st 3 columns
  #!note it does retain row numbers from previous
  dFout <- dFlookup[-which(duplicated(dFlookup$setName)),-c(1,2,3)]
  #add extra empty columns for each segment
  for(i in 1:length(dFt)) dFout[[names(dFt)[i]]] <- NA
  
  ############################
  # centering
  meanTot <- mean( colMeans(dFraw[,-1]),na.rm=TRUE ) # misses out first column containing names
  meanPerSeg <- sapply( dFt,FUN=mean,na.rm=TRUE ) # dFt contains mean per Q per seg, this calcs mean of all Qs per seg
  meanDif <- meanPerSeg - meanTot
  
  #for each set calc the mean of the contributing questions
  #GOOD - this works well
  #this is probably flexible enough to cope with th 57 question version too
  for(setNum in 1:nrow(dFout))  {
    
    setID <- dFout$setID[setNum]
    #find which wuestions contribute to this set
    qNums <- which( dFlookup$setID==setID )
    #calculate the means for all segments
    #!!!seems to be a problem here if just one segment
    #it returns 3 elements rather than a single mean
    #actually issue is with dFt and partic dFt[qNums,]
    #seems that when dFt just has one column, subsetting it results in data without names
    #need to try to keep it as a dataframe, AHA! drop=FALSE solves that
    results <- sapply( dFt[qNums, drop = FALSE,], FUN=mean )        
    #results <- sapply( dFt[qNums,],FUN=mean )
    
    #cat(dFt[qNums,])
    
    #if centreing subtract from the totalMean
    if(centering) results <- results + meanDif
    #put into the output dataFrame
    dFout[setNum,names(results)] <- results
    #dFout[setNum,names(dFt)] <- results    
  }
  
  #reorder the output
  #has to be thought of anticlockwise
  #want the reverse of : un,be,co&tr,se,po,ac,he,st,sd
  #is : 1un,2be,3po,4ac,5he,6st,7sd,8co,9se,10tr
  
  #re-ordering the rows
  dFout2 <- dFout[c(7,6,5,4,3,9,10,8,2,1),]
  
  
  return(dFout2)  
}
