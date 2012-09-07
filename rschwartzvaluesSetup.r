#rschwartzvaluesSetup.r

#!!!!!BEWARE!!!!!!
#take care running this file because it could overwrite stuff

#andy south 25/7/2012

#creating the package
#this file does not go in the package itself

setwd('C:\\rProjects\\PircPlotJune2012\\')

dFlookup21 <- read.csv('lookupTables//21QuestionsLookup.csv')
dFlookup57 <- read.csv('lookupTables//57QuestionsLookup.csv')
#this is how to save to package later
save(dFlookup57, file="rschwartzvaluesWorkingCopy//rschwartzvalues//data//dFlookup57.rda")

dFtestData21 <- read.csv("data//21QTestData6segments.csv")
save(dFtestData21, file="rschwartzvaluesWorkingCopy//rschwartzvalues//data//dFtestData21.rda")
prompt(dFtestData21)

dFtestData57 <- read.csv("data//57QTestData.csv")
save(dFtestData57, file="rschwartzvaluesWorkingCopy//rschwartzvalues//data//dFtestData57.rda")
prompt(dFtestData57)

dFtestData57neg1to9 <- read.csv("data//57QTestDataMinus1to9.csv")
save(dFtestData57neg1to9, file="rschwartzvaluesWorkingCopy//rschwartzvalues//data//dFtestData57neg1to9.rda")
prompt(dFtestData57neg1to9)


options(device="windows")

#setting wd to initialSource
setwd('C:\\rProjects\\PircPlotJune2012\\rschwartzvaluesWorkingCopy\\initialSource\\')

#!put all the sourcefiles in here
sourcefiles <- c('rosePlot','rosePlotAddComparison','rosePlotMulti','valueSets','valueSetsPlots','arctext2')

#shell('dir')
#load each sourcefile into R
for( i in 1 : length(sourcefiles)) source(paste(sourcefiles[i],".r",sep=""))

#adding the datafiles on
components <- c('dFlookup21','dFlookup57',sourcefiles)

setwd('C:\\rProjects\\PircPlotJune2012\\rschwartzvaluesWorkingCopy\\')
#this creates the package folder and all the subfolders
#!BEWARE this could overwrite stuff
#package.skeleton(list=components, name="rschwartzvalues")

#use prompt() to create man pages for individual files


#compiling the package from DOS
#R CMD build rschwartzvalues
