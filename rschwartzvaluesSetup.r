#rschwartzvaluesSetup.r

#andy south 25/7/2012

#creating the package
#this file does not go in the package itself

setwd('C:\\rProjects\\PircPlotJune2012\\')

dFlookup21 <- read.csv('lookupTables//21QuestionsLookup.csv')
dFlookup57 <- read.csv('lookupTables//57QuestionsLookup.csv')

options(device="windows")

setwd('C:\\rProjects\\PircPlotJune2012\\rschwartzvaluesWorkingCopy\\initialSource\\')

#!put all the sourcefiles in here
sourcefiles <- c('rosePlot','rosePlotAddComparison','rosePlotMulti','valueSets','valueSetsPlots','arctext2')

#shell('dir')
for( i in 1 : length(sourcefiles)) source(paste(sourcefiles[i],".r",sep=""))

#adding the datafiles on
components <- c('dFlookup21','dFlookup57',sourcefiles)

setwd('C:\\rProjects\\PircPlotJune2012\\rschwartzvaluesWorkingCopy\\')
#this creates the package folder and all the subfolders
package.skeleton(list=components
                ,name="rschwartzvalues")


