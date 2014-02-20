################################################
### PS4 - Dalston Ward - February 20, 2014   ###
################################################

#Start everything by emptying the workspace.  (don't use this if concurrently working on other projects in R!)
rm(list=ls())
#Second, set the working directory as appropriate.  This makes the code cleaner and helps to keep track of any saved files.
setwd("~/Documents/WashU 2nd Year/Applied Stats Programming/Feb 13/ProblemSet4/")
#Third, source my functions from the other script
source("PS4Functions.R")
#Fourth, some packages
library(lattice)
library(spuRs)


############# Section A: Reading in data without a clean format #################

### The goal is to create a generic function that reads in NetLogo data, and then outputs a directy with several .R,.csv, and .pdf files.  

#NetLogoReader - A function for importing and usefully exporting NetLogo data

#This function does a lot.  First, it creates a series of directories in which NetLogo objects will be stored. It creates an R object containing NetLogo Globals information. It creates a .csv file for each type type of Turtle.  It also creates several plots and accompanying .csv files for these plots.  

#Input: NetLogoFile - a charcter string containing a file path to a NetLogo File
#       PlotRow - the row in which the information about plots begins.  In a NetLogo output this is the row that simply contains "PLOTS".  The function can find this row on its own, but this takes several minutes of scanning rows.  Thus, for better performance, I recommend giving this row as input.  Defaults to null.  
#########################WARNING: scanning the file row by row to find the start of the plots section takes around 5 to 10 minutes. 
#########################SECOND WARNING: This function will not work unless the functions contained in PS4Functions.R have been sourced into the current workspace!

#Output: A directory containing all of the relevent information for performing anlaysis on a NetLogo simulation.  A list of global parameter values, .csv files for the Turtle breeds, and plots capturing a the behavior of the simulaiton (with relevent .csv files for the plots)

#Author: Dalston G. Ward 

NetLogoReader <- function(NetLogoFile,PlotRow=NULL){
#First things first: make code that creates the directories
DirName <- scan(file=NetLogoFile, what="", sep=",", n=1, nlines=1, skip=1) #read in the name
DirDate <- scan(file=NetLogoFile, what="", sep=",", n=1, nlines=1, skip=2) #read in the date
DirDate <- strsplit(DirDate," ") #Clean up the time a bit
DirDate <- unlist(DirDate)[1:2] #drop the time zone part, keep the rest
DirDate[1] <- gsub("/","-",DirDate[1]) #The slashes in the date confuse R when it tries to write a directory.  Use dashes instead. 
DirDate[2] <- substr(DirDate[2],1,8) #drop the thousandths of seconds. 
DirDate[2] <- gsub(":",".",DirDate[2]) #replace the colons with periods because OSX won't accept colons in a file name. 
DirDate <- paste0(DirDate,collapse="_") #put the date and time back together.  
DirCombined <- paste(DirName,DirDate,sep="_") #put name and date together
if(!dir.create(DirCombined)) { #This loop creates the directory, and beacuse the ouput of dir.create is an invisible logical, allows for me to report a warning back to the user if it goes wrong.  
  warning("Unable to create a directory!  Subsequent files might not be where you anticipate them.")
}

#Now, create the secondary directories using an sapply loop to do three at once. 
sapply(c("Globals","Turtles","Plots"),function(x) dir.create(file.path(DirCombined,x)) ) #the file.path command links the parent directory to these sub directories.  

#Finally, create the sub-sub directories in a similiar manner.
sapply(c("PositionPlot","WinnersPlot","PolarizationPlot","IncumbentPercentagePlot"),function(x) dir.create(file.path(DirCombined,"Plots",x)) ) #the file.path command links the parent directory, and sub directory to these sub-sub directories.  

### Create the Globals object

StartGlobal <- RowScanner(NetLogoFile,"GLOBALS") #Identify the row of the data where the globals section begins
Globals.names <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartGlobal,na.strings="") #this line of code reads in the row with the names for the globals 
Globals.name.list <- as.list(rep("",length(Globals.names))) #create a list the same length as the number of globals, with a "" as every element.  This will be useful in the scan function, where the what arguement can take a list as an arguement.  
names(Globals.name.list) <- Globals.names #name the elements of the list with the globals names. 

Globals <- scan(NetLogoFile, what=Globals.name.list, sep=",", nlines=1, skip=StartGlobal+1,na.strings="") #this reads in the data on the line with the globals values.  It usese this data to fill in the list defined in the code above.  

#Now, I move to cleaning this thing up! 

Globals <- lapply(Globals, function(x) gsub("\\]|\\[", "",x)) #remove brackets. 
Globals <- lapply(Globals, function(x) gsub("\"", "",x)) #remove extra quote
Globals <- sapply(Globals, strsplit, split=" ") #split up vectors
Globals <- sapply(Globals,ClassChanger) #make the character vectors into numerics or logicals, as appropraite
dump("Globals",file=file.path(DirCombined,"Globals","Globals.R")) #dump the object to the appropriate directory.  




#### read in the TURTLES

#these first few lines create a list which serves as the colnames for the data to be read in. Useful for all subsequent TURTLES objects
StartColnames <- RowScanner(NetLogoFile,"TURTLES")
Turtles.colnames <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartColnames,na.strings="")
Turtles.list <- as.list(rep("",length(Turtles.colnames)))
names(Turtles.list) <- Turtles.colnames #this is now a named list with the same names as the TURTLES section of a NetLogo file

####### create the districts data frame #############
DistrictStart <- RowScanner(NetLogoFile,"{breed districts}")
Districts.data <- scan(NetLogoFile, what=Turtles.list, sep=",", nlines=Globals$n.districts, skip=DistrictStart-1,na.strings="") #nlines is finish-start, which captures all rows from the first {breed district} to the first {breed voters}
Districts.data <- as.data.frame(Districts.data,stringsAsFactors=FALSE)

########## Now, start cleaning up this data: ##########
Districts.data <- DataThinner(Districts.data) #clean out the constant and missing row (as required by bullet point three of question 5) 
Districts.data <- as.data.frame(lapply(Districts.data, function(x) gsub("\\]|\\[", "",x)),stringsAsFactors=FALSE) #get rid of brackets
Districts.data <- as.data.frame(lapply(Districts.data, function(x) gsub("\\}|\\{", "",x)),stringsAsFactors=FALSE) #get rid of curly brackets


####### split up the preference objects. ########
Districts.prefs <-  sapply(Districts.data$district.prefs, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Districts.data$pref.d1 <- sapply(1:nrow(Districts.data),function(i) Districts.prefs[[i]][1] )
Districts.data$pref.d2 <- sapply(1:nrow(Districts.data),function(i) Districts.prefs[[i]][2] )
Districts.data$pref.d3 <- sapply(1:nrow(Districts.data),function(i) Districts.prefs[[i]][3] )

Districts.data$district.prefs <- NULL #remove the original 
write.csv(Districts.data,file.path(DirCombined,"Turtles","Districts.csv"),row.names=FALSE)

####### Move on to the voters ###########

###### read in the data ######
VoterStart <- RowScanner(NetLogoFile,"{breed voters}",start=DistrictStart+Globals$n.districts)
Voters.data <- scan(NetLogoFile, what=Turtles.list, sep=",", nlines=Globals$n.districts*Globals$"num-voters-per-district", skip=VoterStart-1,na.strings="") 
Voters.data <- as.data.frame(Voters.data,stringsAsFactors=FALSE)

###### Clean it up ######
Voters.data <- DataThinner(Voters.data)  #clean out the constant and missing row (as required by bullet point three of question 5) 
Voters.data <- as.data.frame(lapply(Voters.data, function(x) gsub("\\]|\\[", "",x)),stringsAsFactors=FALSE) #get rid of brackets

####### split up the preference objects. ########
Voters.prefs <-  sapply(Voters.data$prefs, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Voters.data$pref.d1 <- sapply(1:nrow(Voters.data),function(i) Voters.prefs[[i]][1] )
Voters.data$pref.d2 <- sapply(1:nrow(Voters.data),function(i) Voters.prefs[[i]][2] )
Voters.data$pref.d3 <- sapply(1:nrow(Voters.data),function(i) Voters.prefs[[i]][3] )

Voters.data$prefs <- NULL #remove the original 

####### split up the salience objects. ########
Voters.sals <-  sapply(Voters.data$this.voter.sal, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Voters.data$this.voter.sal.d1 <- sapply(1:nrow(Voters.data),function(i) Voters.sals[[i]][1] )
Voters.data$this.voter.sal.d2 <- sapply(1:nrow(Voters.data),function(i) Voters.sals[[i]][2] )
Voters.data$this.voter.sal.d3 <- sapply(1:nrow(Voters.data),function(i) Voters.sals[[i]][3] )

Voters.data$this.voter.sal <- NULL #remove the original 
write.csv(Voters.data,file.path(DirCombined,"Turtles","Voters.csv"),row.names=FALSE)

####### Move on to the Activists ###########

###### read in the data ######
ActivistStart <- RowScanner(NetLogoFile,"{breed activists}",start=VoterStart+Globals$n.districts*Globals$"num-voters-per-district")
Activists.data <- scan(NetLogoFile, what=Turtles.list, sep=",", nlines=Globals$n.districts*Globals$"num-activists-per-district", skip=ActivistStart-1,na.strings="") 
Activists.data <- as.data.frame(Activists.data,stringsAsFactors=FALSE)

###### Clean it up ######
Activists.data <- DataThinner(Activists.data) #clean out the constant and missing row (as required by bullet point three of question 5)  
Activists.data <- as.data.frame(lapply(Activists.data, function(x) gsub("\\]|\\[", "",x)),stringsAsFactors=FALSE) #get rid of brackets

####### split up the preference objects. ########
Activists.prefs <-  sapply(Activists.data$prefs, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Activists.data$pref.d1 <- sapply(1:nrow(Activists.data),function(i) Activists.prefs[[i]][1] )
Activists.data$pref.d2 <- sapply(1:nrow(Activists.data),function(i) Activists.prefs[[i]][2] )
Activists.data$pref.d3 <- sapply(1:nrow(Activists.data),function(i) Activists.prefs[[i]][3] )

Activists.data$prefs <- NULL #remove the original 

####### split up the salience objects. ########
Activists.sals <-  sapply(Activists.data$this.act.sal, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Activists.data$this.act.sal.d1 <- sapply(1:nrow(Activists.data),function(i) Activists.sals[[i]][1] )
Activists.data$this.act.sal.d2 <- sapply(1:nrow(Activists.data),function(i) Activists.sals[[i]][2] )
Activists.data$this.act.sal.d3 <- sapply(1:nrow(Activists.data),function(i) Activists.sals[[i]][3] )

Activists.data$this.act.sal <- NULL #remove the original 
write.csv(Activists.data,file.path(DirCombined,"Turtles","Activists.csv"),row.names=FALSE)

####### Move on to the Parties ###########

###### read in the data ######
PartyStart <- RowScanner(NetLogoFile,"{breed parties}",start=ActivistStart+Globals$n.districts*Globals$"num-activists-per-district")
Parties.data <- scan(NetLogoFile, what=Turtles.list, sep=",", nlines=Globals$n.parties, skip=PartyStart-1,na.strings="") 
Parties.data <- as.data.frame(Parties.data,stringsAsFactors=FALSE)

###### Clean it up ######
Parties.data <- DataThinner(Parties.data)  #clean out the constant and missing row (as required by bullet point three of question 5) 
Parties.data <- as.data.frame(lapply(Parties.data, function(x) gsub("\\]|\\[", "",x)),stringsAsFactors=FALSE) #get rid of brackets
Parties.data <- as.data.frame(lapply(Parties.data, function(x) gsub("\\}|\\{", "",x)),stringsAsFactors=FALSE) #get rid of curly brackets

####### split up the preference objects. ########
Parties.mean.positions <-  sapply(Parties.data$mean.position, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Parties.data$mean.position.d1 <- sapply(1:nrow(Parties.data),function(i) Parties.mean.positions[[i]][1] )
Parties.data$mean.position.d2 <- sapply(1:nrow(Parties.data),function(i) Parties.mean.positions[[i]][2] )
Parties.data$mean.position.d3 <- sapply(1:nrow(Parties.data),function(i) Parties.mean.positions[[i]][3] )

Parties.data$mean.position <- NULL #remove the original 

####### split up the salience objects. ########
Parties.enforcement.points <-  sapply(Parties.data$enforcement.point, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Parties.data$enforcement.point.d1 <- sapply(1:nrow(Parties.data),function(i) Parties.enforcement.points[[i]][1] )
Parties.data$enforcement.point.d2 <- sapply(1:nrow(Parties.data),function(i) Parties.enforcement.points[[i]][2] )
Parties.data$enforcement.point.d3 <- sapply(1:nrow(Parties.data),function(i) Parties.enforcement.points[[i]][3] )

Parties.data$enforcement.point <- NULL #remove the original 
write.csv(Parties.data,file.path(DirCombined,"Turtles","Parties.csv"),row.names=FALSE)


####### Move on to the Candidates ###########

###### read in the data ######
CandStart <- RowScanner(NetLogoFile,"{breed cands}",start=PartyStart+Globals$n.parties)
Candidates.data <- scan(NetLogoFile, what=Turtles.list, sep=",", nlines=Globals$n.parties*Globals$n.districts, skip=CandStart-1,na.strings="") 
Candidates.data <- as.data.frame(Candidates.data,stringsAsFactors=FALSE)

###### Clean it up ######
Candidates.data <- DataThinner(Candidates.data)  #clean out the constant and missing row (as required by bullet point three of question 5) 
Candidates.data <- as.data.frame(lapply(Candidates.data, function(x) gsub("\\]|\\[", "",x)),stringsAsFactors=FALSE) #get rid of brackets

####### split up the preference objects. ########
Candidates.positions.obs <-  sapply(Candidates.data$positions.obs, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Candidates.data$position.obs.d1 <- sapply(1:nrow(Candidates.data),function(i) Candidates.positions.obs[[i]][1] )
Candidates.data$position.obs.d2 <- sapply(1:nrow(Candidates.data),function(i) Candidates.positions.obs[[i]][2] )
Candidates.data$position.obs.d3 <- sapply(1:nrow(Candidates.data),function(i) Candidates.positions.obs[[i]][3] )

Candidates.data$positions.obs <- NULL #remove the original 

####### split up the salience objects. ########
Candidates.positions.obs.last <-  sapply(Candidates.data$positions.obs.last, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Candidates.data$position.obs.last.d1 <- sapply(1:nrow(Candidates.data),function(i) Candidates.positions.obs.last[[i]][1] )
Candidates.data$position.obs.last.d2 <- sapply(1:nrow(Candidates.data),function(i) Candidates.positions.obs.last[[i]][2] )
Candidates.data$position.obs.last.d3 <- sapply(1:nrow(Candidates.data),function(i) Candidates.positions.obs.last[[i]][3] )

Candidates.data$positions.obs.last <- NULL #remove the original 
write.csv(Candidates.data,file.path(DirCombined,"Turtles","Candidates.csv"),row.names=FALSE)

#### Now, the plotting section 

### First, the positions plots

#locate the start of the plotting section in the NetLogo file with RowScanner
if(is.null(PlotRow)){
StartPlots <- RowScanner(NetLogoFile,"PLOTS", start=CandStart) ####THIS TAKES A LONG TIME!!!
} else {StartPlots <- PlotRow}

#begin by writing some code to help read in each section (constructing the list that is like the skelton of the dataframes!) 

Plot.names.1 <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartPlots+13,na.strings="") #this line of code reads in the row with the red/blue part of the names for the plot data
Plot.names.1 <- Plot.names.1[-which(is.na(Plot.names.1))] #Remove Missing Values
Plot.names.1 <- lapply(Plot.names.1, function(x) gsub("\"", "",x)) #remove extra quotes
Plot.names.1 <- rep(Plot.names.1,each=4) #make enough repetitions for each element that will go into Plot names 2

Plot.names.2 <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartPlots+14,na.strings="") #this line of code reads in the row with the 2nd part of names. 

Plot.names <-paste(Plot.names.1,Plot.names.2,sep=".") #Now combine the two. 

Plot.names.list <- as.list(rep("",length(Plot.names))) #create a list the same length as the number of globals, with a "" as every element.  This will be useful in the scan function, where the what arguement can take a list as an arguement.  
names(Plot.names.list) <- Plot.names #name the elements of the list with the plot names. 


#This next section actually reads in the data for d1
Plot.data.d1 <- scan(NetLogoFile, what=Plot.names.list, sep=",", nlines=Globals$"global-counter", skip=StartPlots+15,na.strings="") #this reads in the data values.  It usese this data to fill in the list defined in the code above.  
Plot.data.d1 <- as.data.frame(Plot.data.d1,stringsAsFactors=FALSE) #coerce it into a data frame
Plot.data.d1 <- DataThinner(Plot.data.d1)  # remove the constant and missing columns
Plot.data.d1 <- Plot.data.d1[,-c(3,5,7,9,11)] #remove all but one of the x columns, which are all duplicates 
colnames(Plot.data.d1)[1] <- "Period" #this column represents which iteration this row's values come from.  
Plot.data.d1 <- as.data.frame(apply(Plot.data.d1,2,as.numeric))
write.csv(Plot.data.d1,file.path(DirCombined,"Plots","PositionPlot","D1.csv"),row.names=FALSE)


#This starts reading in d2
D2Start <- RowScanner(NetLogoFile,"\"D2\"", start=StartPlots+168+14)
Plot.data.d2 <-  scan(NetLogoFile, what=Plot.names.list, sep=",", nlines=Globals$"global-counter", skip=D2Start+13,na.strings="") #this reads in the data values.  It usese this data to fill in the list defined in the code above.  
Plot.data.d2 <- as.data.frame(Plot.data.d2,stringsAsFactors=FALSE) #coerce it into a data frame
Plot.data.d2 <- DataThinner(Plot.data.d2)  # remove the constant and missing columns
Plot.data.d2 <- Plot.data.d2[,-c(3,5,7,9,11)] #remove all but one of the x columns, which are all duplicates 
colnames(Plot.data.d2)[1] <- "Period" #this column represents which iteration this row's values come from.  
Plot.data.d2 <- as.data.frame(apply(Plot.data.d2,2,as.numeric))
write.csv(Plot.data.d2,file.path(DirCombined,"Plots","PositionPlot","D2.csv"),row.names=FALSE)


#This starts reading in d3
D3Start <- RowScanner(NetLogoFile,"\"D3\"", start=D2Start+168+13)
Plot.data.d3 <-  scan(NetLogoFile, what=Plot.names.list, sep=",", nlines=Globals$"global-counter", skip=D3Start+13,na.strings="") #this reads in the data values.  It usese this data to fill in the list defined in the code above.  
Plot.data.d3 <- as.data.frame(Plot.data.d3,stringsAsFactors=FALSE) #coerce it into a data frame
Plot.data.d3 <- DataThinner(Plot.data.d3)  # remove the constant and missing columns
Plot.data.d3 <- Plot.data.d3[,-c(3,5,7,9,11)] #remove all but one of the x columns, which are all duplicates 
colnames(Plot.data.d3)[1] <- "Period" #this column represents which iteration this row's values come from.  
Plot.data.d3 <- as.data.frame(apply(Plot.data.d3,2,as.numeric))
write.csv(Plot.data.d3,file.path(DirCombined,"Plots","PositionPlot","D3.csv"),row.names=FALSE)


pdf(file=file.path(DirCombined,"Plots","PositionPlot","Positions.pdf"),width=8.5,height=11) #create a graphic device, the size of a standard American piece of paper.  
par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(5,5,5,2)) #set up the device.  I've made it so that I can have three plots stacked on top of each other. 
plot(x=0:168,y=Plot.data.d1$Red.y,type="l",col="red",xaxt="n",ylim=c(min(c(Plot.data.d1$Red.y,Plot.data.d1$Blue.y)),max(c(Plot.data.d1$Red.y,Plot.data.d1$Blue.y)))) #the messy bit sets the y axis limits to the minimum and maximum values.  Otherwise, this just plots the trend for the red candidates over time.  No x axis, because the plots are stacked, and no labels because I'll add these later.
lines(x=0:168,y=Plot.data.d1$Blue.y,col="Blue") #add the blue trend line 
plot(x=0:168,y=Plot.data.d2$Red.y,type="l",col="red",xaxt="n",ylim=c(min(c(Plot.data.d2$Red.y,Plot.data.d2$Blue.y)),max(c(Plot.data.d2$Red.y,Plot.data.d2$Blue.y))),bty="U") #same as above, but with bty="U", which makes it so that the line separating plots isnt drawn twice. 
lines(x=0:168,y=Plot.data.d2$Blue.y,col="Blue")
plot(x=0:168,y=Plot.data.d3$Red.y,type="l",col="red",xaxt="n",ylim=c(min(c(Plot.data.d3$Red.y,Plot.data.d3$Blue.y)),max(c(Plot.data.d3$Red.y,Plot.data.d3$Blue.y))),bty="U")
lines(x=0:168,y=Plot.data.d3$Blue.y,col="Blue")
axis(1,at=seq(10,160,15),outer=TRUE,lab=) #Adds the single x-axis for all three plots at the bottom
mtext("Average Candidate Positions",side=3,outer=TRUE,line=1,cex=1.5,adj=.5) #the title, above everything in the outer margin
mtext("Simulation Period",side=1,outer=TRUE,line=2.5,cex=.75,adj=.5) #Below everything add the x axis label
mtext(c("Dimension 1","Dimension 2","Dimension 3"),side=2,outer=TRUE,line=2.5,cex=.75,at=c(5/6,3/6,1/6)) #Vectorize the Y-axis labels, putting them each in the middle of each plot.  

#All of this code is exactly the same as above only with different data.  
plot(x=0:168,y=Plot.data.d1$RedActivists.y,type="l",col="red",xaxt="n",ylim=c(min(c(Plot.data.d1$RedActivists.y,Plot.data.d1$BlueActivists.y)),max(c(Plot.data.d1$RedActivists.y,Plot.data.d1$BlueActivists.y))))
lines(x=0:168,y=Plot.data.d1$BlueActivists.y,col="Blue")
plot(x=0:168,y=Plot.data.d2$RedActivists.y,type="l",col="red",xaxt="n",ylim=c(min(c(Plot.data.d2$RedActivists.y,Plot.data.d2$BlueActivists.y)),max(c(Plot.data.d2$RedActivists.y,Plot.data.d2$BlueActivists.y))),bty="U")
lines(x=0:168,y=Plot.data.d2$BlueActivists.y,col="Blue")
plot(x=0:168,y=Plot.data.d3$RedActivists.y,type="l",col="red",xaxt="n",ylim=c(min(c(Plot.data.d3$RedActivists.y,Plot.data.d3$BlueActivists.y)),max(c(Plot.data.d3$RedActivists.y,Plot.data.d3$BlueActivists.y))),bty="U")
lines(x=0:168,y=Plot.data.d3$BlueActivists.y,col="Blue")
axis(1,at=seq(10,160,15),outer=TRUE,lab=)
mtext("Average Activist Positions",side=3,outer=TRUE,line=1,cex=1.5,adj=.5)
mtext("Simulation Period",side=1,outer=TRUE,line=2.5,cex=.75,adj=.5)
mtext(c("Dimension 1","Dimension 2","Dimension 3"),side=2,outer=TRUE,line=2.5,cex=.75,at=c(5/6,3/6,1/6))

#Again, the same but with different data 
plot(x=0:168,y=Plot.data.d1$RedVoters.y,type="l",col="red",ylab="Dimension 1",xaxt="n",ylim=c(min(c(Plot.data.d1$RedVoters.y,Plot.data.d1$BlueVoters.y)),max(c(Plot.data.d1$RedVoters.y,Plot.data.d1$BlueVoters.y))))
lines(x=0:168,y=Plot.data.d1$BlueVoters.y,col="Blue")
plot(x=0:168,y=Plot.data.d2$RedVoters.y,type="l",col="red",ylab="Dimension 2",xaxt="n",ylim=c(min(c(Plot.data.d2$RedVoters.y,Plot.data.d2$BlueVoters.y)),max(c(Plot.data.d2$RedVoters.y,Plot.data.d2$BlueVoters.y))),bty="U")
lines(x=0:168,y=Plot.data.d2$BlueVoters.y,col="Blue")
plot(x=0:168,y=Plot.data.d3$RedVoters.y,type="l",col="red",ylab="Dimension 3",xaxt="n",ylim=c(min(c(Plot.data.d3$RedVoters.y,Plot.data.d3$BlueVoters.y)),max(c(Plot.data.d3$RedVoters.y,Plot.data.d3$BlueVoters.y))),bty="U")
lines(x=0:168,y=Plot.data.d3$BlueVoters.y,col="Blue")
axis(1,at=seq(10,160,15),outer=TRUE,lab=)
mtext("Average Voter Positions",side=3,outer=TRUE,line=1,cex=1.5,adj=.5)
mtext("Simulation Period",side=1,outer=TRUE,line=2.5,cex=.75,adj=.5)
mtext(c("Dimension 1","Dimension 2","Dimension 3"),side=2,outer=TRUE,line=2.5,cex=.75,at=c(5/6,3/6,1/6))
dev.off() #close the device, and go check it out! 

### The winners section

#First, pick out the row where the winners section starts
StartWinners <- RowScanner(NetLogoFile,"\"WINNERS\"", start=D3Start+168+13)

winners.names.1 <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartWinners+8,na.strings="") #this line of code reads in the row with the red/blue part of the names for the winners data
winners.names.1 <- winners.names.1[-which(is.na(winners.names.1))] #Remove Missing Values
winners.names.1 <- lapply(winners.names.1, function(x) gsub("\"", "",x)) #remove extra quotes
winners.names.1 <- rep(winners.names.1,each=4) #make enough repetitions for each element that will go into Plot names 2

winners.names.2 <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartWinners+9,na.strings="") #this line of code reads in the row with the 2nd part of names. 
winners.names <-paste(winners.names.1,winners.names.2,sep=".") #Now combine the two. 

winners.names.list <- as.list(rep("",length(winners.names))) #create a list the same length as the number of globals, with a "" as every element.  This will be useful in the scan function, where the what arguement can take a list as an arguement.  
names(winners.names.list) <- winners.names #name the elements of the list with the plot names. 


#This next section actually reads in the data for winners

winners.data <- scan(NetLogoFile, what=winners.names.list, sep=",", nlines=Globals$"global-counter", skip=StartWinners+10,na.strings="") #this reads in the data values.  It usese this data to fill in the list defined in the code above.  
winners.data <- as.data.frame(winners.data,stringsAsFactors=FALSE) #coerce it into a data frame
winners.data <- DataThinner(winners.data)  # remove the constant and missing columns
winners.data <- winners.data[,-c(3,4)] #remove all but one of the x columns, which are all duplicates 
colnames(winners.data)[1] <- "Period" #this column represents which iteration this row's values come from.  
winners.data <- as.data.frame(apply(winners.data,2,as.numeric))
write.csv(winners.data,file.path(DirCombined,"Plots","WinnersPlot","Winner.csv"),row.names=FALSE)

# Make the winners plot.  I choose to make density plots for this one. 

#make the density plots on thier own so that their elements can be used in formatting
Blue.Density <- density(winners.data[,2])
Red.Density <- density(winners.data[,3])

pdf(file=file.path(DirCombined,"Plots","WinnersPlot","Winner.pdf"),width=8.5,height=11) #create a graphic device, the size of a standard American piece of paper.  
plot(x=Blue.Density$x,y=Blue.Density$y,xlim=c(min(c(Blue.Density$x,Red.Density$x)),max(c(Blue.Density$x,Red.Density$x))),ylim=c(0,max(c(Blue.Density$y,Red.Density$y))),col="Blue",type="l",ylab="Kernal Density",xlab="Percentage of \"Winning\" Candidates",bty="n",main="Distribution of the Percentage of each Party's Candidates Winning in each Period") #this again sets the limits of the axes to fit the data and also writes the labels and titles
lines(x=Red.Density$x,y=Red.Density$y,col="red") #the other party 
legend("topright",legend=c("Blue Party","Red Party"),lty=1,col=c("Blue","Red"),bty="n") 
rug(winners.data[,2],col="Blue") #plots where the data points are.  Doesn't look as nice when there isn't as much data....
rug(winners.data[,3],col="Red")
dev.off() #go check this one out too! 

### The Polarization

#First, pick out the row where the winners section starts
StartPolarization <- RowScanner(NetLogoFile,"\"POLARIZATION\"", start=StartWinners+168)

polarization.names.1 <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartPolarization+8,na.strings="") #this line of code reads in the row with the red/blue part of the names for the polarization data
polarization.names.1 <- polarization.names.1[-which(is.na(polarization.names.1))] #Remove Missing Values
polarization.names.1 <- lapply(polarization.names.1, function(x) gsub("\"", "",x)) #remove extra quotes
polarization.names.1 <- rep(polarization.names.1,each=4) #make enough repetitions for each element that will go into Plot names 2

polarization.names.2 <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartPolarization+9,na.strings="") #this line of code reads in the row with the 2nd part of names. 
polarization.names <-paste(polarization.names.1,polarization.names.2,sep=".") #Now combine the two. 

polarization.names.list <- as.list(rep("",length(polarization.names))) #create a list the same length as the number of globals, with a "" as every element.  This will be useful in the scan function, where the what arguement can take a list as an arguement.  
names(polarization.names.list) <- polarization.names #name the elements of the list with the plot names. 


#This next section actually reads in the data for Polarization

polarization.data <- scan(NetLogoFile, what=polarization.names.list, sep=",", nlines=Globals$"global-counter", skip=StartPolarization+10,na.strings="") #this reads in the data values.  It usese this data to fill in the list defined in the code above.  
polarization.data <- as.data.frame(polarization.data,stringsAsFactors=FALSE) #coerce it into a data frame
polarization.data <- DataThinner(polarization.data)  # remove the constant and missing columns
polarization.data <- polarization.data[,-c(3,5)] #remove all but one of the x columns, which are all duplicates 
colnames(polarization.data)[1:2] <- c("Period","CANDIDATES.y") #Rename a couple of columns so that we know better what is represented there. 
polarization.data <- as.data.frame(apply(polarization.data,2,as.numeric))
write.csv(polarization.data,file.path(DirCombined,"Plots","PolarizationPlot","Polarization.csv"),row.names=FALSE)

#make the polarization plot now 

pdf(file=file.path(DirCombined,"Plots","PolarizationPlot","Polarization.pdf"),width=11,height=8.5) #create a graphic device, the size of a standard American piece of paper.  
par(mfrow=c(1,3),oma=c(0,0,3,0),mar=c(5,4,0,2)) #three plots on a single device
plot(y=polarization.data$CANDIDATES.y,x=polarization.data$VOTERS.y,pch="+",xlab="Voter Polarization",ylab="Candidate Polarization") #each candidate polarization value against its correpsonding voter polarization, to see if voters or candidates are more polarized
abline(a=0,b=1,col="grey20") #plot a line representing equal polarization between the two

plot(y=polarization.data$CANDIDATES.y,x=polarization.data$ACTIVISTS.y,pch="+",xlab="Activist Polarization",ylab="Candidate Polarization") #same idea as the above plot, just changing the values 
abline(a=0,b=1,col="grey20")

plot(y=polarization.data$VOTERS.y,x=polarization.data$ACTIVISTS.y,pch="+",xlab="Activist Polarization",ylab="Voter Polarization") #same idea as the other plots, again, new data
abline(a=0,b=1,col="grey20")

mtext("Bivariate Polarization Relationships",side=3,outer=TRUE,line=1,cex=1.5,adj=.5)
dev.off()

### Finally, code to bring in the Incumbent Wins data.  

#First, pick out the row where the winners section starts
StartIncumbents <- RowScanner(NetLogoFile,"\"INCUMBENT\"", start=StartPolarization+168)

incumbent.names <- scan(NetLogoFile, what="", sep=",", nlines=1, skip=StartIncumbents+7,na.strings="") #this line of code reads in the row of column names for this part of the output. 

incumbent.names.list <- as.list(rep("",length(incumbent.names))) #create a list the same length as the number of globals, with a "" as every element.  This will be useful in the scan function, where the what arguement can take a list as an arguement.  
names(incumbent.names.list) <- incumbent.names #name the elements of the list with the incumbent column names. 


#This next section actually reads in the data.

incumbent.data <- scan(NetLogoFile, what=incumbent.names.list, sep=",", nlines=Globals$"global-counter", skip=StartIncumbents+8,na.strings="") #this reads in the data values.  It usese this data to fill in the list defined in the code above.  
incumbent.data <- as.data.frame(incumbent.data,stringsAsFactors=FALSE) #coerce it into a data frame
incumbent.data <- DataThinner(incumbent.data)  # remove the constant and missing columns
colnames(incumbent.data)[1:2] <- c("Period","Incumbent.win.percentage") #Rename the columns
incumbent.data <- as.data.frame(apply(incumbent.data,2,as.numeric)) #make it numeric.
write.csv(incumbent.data,file.path(DirCombined,"Plots","IncumbentPercentagePlot","IncumbentWins.csv"),row.names=FALSE)

## Make the plots for the incumbents data.
pdf(file=file.path(DirCombined,"Plots","IncumbentPercentagePlot","IncumbentWins.pdf"),width=8.5,height=11) #create a graphic device, the size of a standard American piece of paper.  
split.screen(figs=c(2,1)) #Split screen is cool.  It allows me to make graphics with different numbers of plots on each column/row
split.screen(figs=c(1,2),screen=2)
screen(1)
par(mar=c(4,4,3,1))
plot(x=incumbent.data$Period[-1],y=incumbent.data$Incumbent.win.percentage[-1],pch=20,main="Trend in Incumbent Win Percentage with a Loess Smoother",ylab="Winning Percentage",xlab="Simulation period") #make a scatter plot of the percentages over time
smooooth <- loess(incumbent.data$Incumbent.win.percentage[-1]~incumbent.data$Period[-1])
lines(predict(smooooth),col="purple",lwd=2) #plot the predictions from a LOESS smoother to it
screen(3)
par(mar=c(4,4,3,1))
boxplot(list(incumbent.data$Incumbent.win.percentage[1:84],incumbent.data$Incumbent.win.percentage[(168/2)+1:168]),outline=FALSE,names=c("Periods 1-84","Periods 85-168"),ylab="Winning Percentage",main="Win Percentage by Simulation Half") #makes a box and whisker plot for comparing the first and second halves of the simulation
screen(4)
par(mar=c(4,5,3,1))
plot(density(incumbent.data$Incumbent.win.percentage[-1]),xlab="Winning Percentage",ylab="Kernal Density",main="Distribution of Winning Percentages") #a density plot of the entire percentage distribution.  
close.screen( all = TRUE ) #stop the split screening
dev.off()
}

NetLogoReader(NetLogoFile="NetLogo.csv",PlotRow=8531) #It works!!!!

############ JMR Problems #############

##Chapter 4, Problem 3

SquareCubes <- function(n){
    cat("Number    Square    Cube \n\n")
    cat(paste(format(1:n,width=6),format((1:n)^2,width=8),format((1:n)^3,width=5),"\n"),sep="") #Basically, just tweak JMR's code from page 51
}
SquareCubes(7)

##Chapter 4, Problem 4
MulitplicationTable <- function(n=9){ #choose how large you want the table to be.  Default is 9, the standard table I learned in elementary school. 
  sapply(1:n,function(x) 1:n*x ) #Vectorization makes this very simple.  Sapply simplifies the output as well, so that it is a matrix, just like we want.  
}
MulitplicationTable()

## Chapter 7, problem 3
#for Replicability, set a seed
set.seed(1801)
pop <- data.frame(m=rnorm(100,160,20),f=rnorm(100,160,20),gen=1)
next.gen <- function(pop){
  pop$m <- sample(pop$m)
  pop$m <- apply(pop,1,mean)
  pop$f <- pop$m
  pop$gen <- pop$gen+1
  return(pop)
}

next.gen(next.gen(next.gen(pop)))
Generations <- function(ngen=9){
  pop <- data.frame(m=rnorm(100,160,20),f=rnorm(100,160,20),gen=1)
  if(ngen >1){
  for(i in 1:ngen-1){
    pop <- rbind(pop,next.gen(pop[pop$gen==i,]))
  }
  }
  return(pop)
}
gendat <- Generations(ngen=9)
gendat$gen <- as.factor(gendat$gen)
histogram(~m|gen,data=gendat,xlab="Male Height") #There is the plot.  We see that after only about 4 generations, everyone falls into just a single bin.  

##JMR chapter 7, number four 
data(treeg)
colnames(treeg)
treeg$tree.ID <- factor(treeg$tree.ID)
xyplot(height.ft~age,groups=tree.ID,type=c("l"),
       data=treeg,xlab="age (years)",ylab="height (feet)",main="Height on Age for Trees")

