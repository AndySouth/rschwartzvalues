#rschwartzvaluesTesting.r
#andy south 15/8/2012

#commands to test rschwartzvalues

library(plotrix) #for arctext
library(rschwartzvalues) 

setwd('C:\\rProjects\\PircPlotJune2012\\')
source('arctext2.r') #modified arctext to allow changing orientation of circular text
#source('arctextFromJim.r')

options(device="windows")

dev.new()
#windows() #creates a new windows device, allows savePlot

#best way to accept inputs would be as a csv or dataframe

#these are the inputs
iNames <- (c('Hedonism','Stimulation','Self-Direction','Universalism','Benevolence','Conformity & Tradition','Security','Power','Achievement'))
iValues <- c(1,10,2,3,4,5,6,7,8)
iValues2 <- c(2,9,3,2,4,10,6,7,4)
iValues3 <- c(9,9,9,9,9,9,9,9,9)
#iColours <- seq(1:length(iValues))
iColours <- rainbow(length(iValues))


dF <- data.frame(names=iNames, values=iValues, values2=iValues2, values3=iValues3)


#testing
rosePlot(dF, names='names', values='values')
rosePlot(dF, names='names', values='values',maxValue=11)

rosePlot(dF, names='names', values='values')
rosePlotAddComparison(dF, names='names', values='values2')

#trying 6 plots on a page
dev.new()
oldPar <- par(mar=c(1, 1, 1, 1))
nPanels <- layout( cbind(c(0,1,2,3),c(0,4,5,6))
                      , heights=c(lcm(0.5),1,1,1)
                      , widths=c(1,1,1)
                      , respect=FALSE)
#layout.show(nPanels)
#for ( i in 1:6 ) rosePlot(dF, names='names', values='values', textSize=0.3)

#rosePlot(dF, names='names', values='values', textSize=0.3,title='textSize=0.3')
rosePlot(dF, names='names', values='values', textSize=0.3, gridThick=0.3,title='gridThick=0.3')
rosePlot(dF, names='names', values='values', textSize=0.3, ringCol='purple', textCol='white',title="ringCol='purple',textCol='white'" )
rosePlot(dF, names='names', values='values', textSize=0.3, gridCol='grey', bgCol='grey', ringCol='red',title="gridCol='grey',bgCol='grey',ringCol='red'")
rosePlot(dF, names='names', values='values', textSize=0.3, gridInterval=2,title="gridInterval=2")
rosePlot(dF, names='names', values='values', textSize=0.3, ringWidth=0.4,title="ringWidth=0.4")
rosePlot(dF, names='names', values='values', textSize=0.3, palette='grey',title="palette='grey'")

savePlot("roseMultiPlot.pdf",type='pdf')

rosePlot(dF, names='names', values='values', textSize=0.3, palette='grey',title="palette='grey'")
rosePlot(dF, names='names', values='values', textSize=0.3, palette=c('red','green','blue','red','green','blue','red','green','blue'),title="palette=c('red','green','blue','red','green','blue','red','green','blue')")

savePlot("roseMultiPlot2.pdf",type='pdf')  
par(oldPar)



#checking analysis of raw data
dFraw <- read.csv("data//wag21testData.csv")
dFraw <- read.csv("data//wag21testData1segment.csv")
dFraw <- read.csv("data//wag21testData1segment2indivs.csv")
dFraw <- read.csv("data//wag21testData1segmentnoID.csv")
valueSets(dFraw)
valueSets(dFraw, centering=FALSE)
#8 segment input file
dFraw <- read.csv("data//wag21testData8segments.csv")
dFvalueSets <- valueSets(dFraw)
#save the value sets to csv
write.csv( dFvalueSets, "data//wag21testValueSets8segments.csv", row.names = F )
#trying rosePlotMulti
rosePlotMulti( dFvalueSets, names='setName', valuesColumns = c(3:10), textSize=0.3 )

#function to go from raw data to the plots
valueSetsPlots(dFraw)
valueSetsPlots(dFraw,numQs=21)

#try the 57 question version


library(rschwartzvalues)
dFmyData  <- read.csv("data//21QTestData.csv")

dFmyData  <- read.csv("data//21QTestData6segments.csv")
valueSetsPlots(dFmyData,numQs=21) #generates error
dF <- valueSets(dFmyData,numQs=21) #generates -ve values
valueSets(dFmyData,numQs=21,centering=FALSE) #works
valueSetsPlots(dFmyData,numQs=21,maxValue=5,centering=FALSE) #works
rosePlot(dF, names='setName', values='group5')



