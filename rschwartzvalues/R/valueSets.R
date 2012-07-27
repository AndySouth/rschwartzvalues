#!need to modify this to cope with dataframes without the first column
#!and make sure it copes with single segment

valueSets <-
function( dFraw  #dataFrame or could be a text file 
                       , numQs = 21 #21 or 57 or later allow user to submit their own lookup table  
                       , centering = TRUE #whether to centre means by the mean of all per segment
                       ) 
{
  
  #first read in the lookup table
  #later will be stored in package
  #dFlookup21 <- read.csv('lookupTables//21QuestionsLookup.csv')
  #dFlookup57 <- read.csv('lookupTables//57QuestionsLookup.csv')
  if (numQs == 21) {
    data(dFlookup21)
    dFlookup <- dFlookup21
  }
  else if (numQs == 57)  {
    data(dFlookup57)
    dFlookup <- dFlookup57
  }
  else warning("numQs needs to be 21 or 57 later other options can be added")
  
  #dFraw <- read.csv("data//wag21testData.csv")
  
  #test whether the number of questions in the lookup table and the raw data is the same 
  
  #assume that the input data has 1 identifier column followed by 21 columns with answer to each question
  #respondents in rows
  #column1 has an identifier to subset data by
  #dFraw[index+1]
  
  #first try calculating the means for each segment for each question
  #this works
  dFrawMeans <- aggregate(dFraw[2:numQs+1],by=dFraw[1], mean)
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
  meanTot <- mean( colMeans(dFraw[,-1]) ) # misses out first column containing names
  meanPerSeg <- sapply( dFt,FUN=mean ) # dFt contains mean per Q per seg, this calcs mean of all Qs per seg
  meanDif <- meanPerSeg - meanTot
  
  #for each set calc the mean of the contributing questions
  #GOOD - this works well
  #this is probably flexible enough to cope with th 57 question version too
  for(setNum in 1:nrow(dFout))  {
    
    setID <- dFout$setID[setNum]
    #find which wuestions contribute to this set
    qNums <- which( dFlookup$setID==setID )
    #calculate the means for all segments
    results <- sapply( dFt[qNums,],FUN=mean )
    #if centreing subtract from the totalMean
    if(centering) results <- results + meanDif
    #put into the output dataFrame
    dFout[setNum,names(results)] <- results
  }
  
  return(dFout)  
}
