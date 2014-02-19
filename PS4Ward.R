################################################
### PS4 - Dalston Ward - February 20, 2014   ###
################################################

#Start everything by emptying the workspace.  (don't use this if concurrently working on other projects in R!)
rm(list=ls())
#Second, set the working directory as appropriate.  This makes the code cleaner and helps to keep track of any saved files.
setwd("~/Documents/WashU 2nd Year/Applied Stats Programming/Feb 13/ProblemSet4/")
#Third, source my functions from the other script
source("PS4Functions.R")
#Fourth, set the seed for replicability
set.seed(1801)
#Fifth, load some packages
library(plyr)

############# Section A: Reading in data without a clean format #################

### The goal is to create a generic function that reads in NetLogo data, and then outputs a directy with several .R,.csv, and .pdf files.  

#First things first: make code that creates the directories
DirName <- scan(file="NetLogo.csv", what="", sep=",", n=1, nlines=1, skip=1) #read in the name
DirDate <- scan(file="NetLogo.csv", what="", sep=",", n=1, nlines=1, skip=2) #read in the date
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

StartGlobal <- RowScanner("NetLogo.csv","GLOBALS") #Identify the row of the data where the globals section begins
Globals.names <- scan("NetLogo.csv", what="", sep=",", nlines=1, skip=StartGlobal,na.strings="") #this line of code reads in the row with the names for the globals 
Globals.name.list <- as.list(rep("",length(Globals.names))) #create a list the same length as the number of globals, with a "" as every element.  This will be useful in the scan function, where the what arguement can take a list as an arguement.  
names(Globals.name.list) <- Globals.names #name the elements of the list with the globals names. 

Globals <- scan("NetLogo.csv", what=Globals.name.list, sep=",", nlines=1, skip=StartGlobal+1,na.strings="") #this reads in the data on the line with the globals values.  It usese this data to fill in the list defined in the code above.  

#Now, I move to cleaning this thing up! 

Globals <- lapply(Globals, function(x) gsub("\\]|\\[", "",x)) #remove brackets. 
Globals <- lapply(Globals, function(x) gsub("\"", "",x)) #remove extra quote
Globals <- sapply(Globals, strsplit, split=" ") #split up vectors
Globals <- sapply(Globals,ClassChanger) #make the character vectors into numerics or logicals, as appropraite
dump("Globals",file=file.path(DirCombined,"Globals","Globals.R")) #dump the object to the appropriate directory.  




#### read in the TURTLES

#these first few lines create a list which serves as the colnames for the data to be read in. Useful for all subsequent TURTLES objects
StartColnames <- RowScanner("NetLogo.csv","TURTLES")
Turtles.colnames <- scan("NetLogo.csv", what="", sep=",", nlines=1, skip=StartColnames,na.strings="")
Turtles.list <- as.list(rep("",length(Turtles.colnames)))
names(Turtles.list) <- Turtles.colnames #this is now a named list with the same names as the TURTLES section of a NetLogo file

####### create the districts data frame #############
DistrictStart <- RowScanner("NetLogo.csv","{breed districts}")
Districts.data <- scan("NetLogo.csv", what=Turtles.list, sep=",", nlines=Globals$n.districts, skip=DistrictStart-1,na.strings="") #nlines is finish-start, which captures all rows from the first {breed district} to the first {breed voters}
Districts.data <- as.data.frame(Districts.data,stringsAsFactors=FALSE)

########## Now, start cleaning up this data: ##########
Districts.data <- DataThinner(Districts.data) #clean out the constant and missing rows. 
Districts.data <- as.data.frame(lapply(Districts.data, function(x) gsub("\\]|\\[", "",x)),stringsAsFactors=FALSE) #get rid of brackets

####### split up the preference objects. ########
Districts.prefs <-  sapply(Districts.data$district.prefs, strsplit, split=" ") 

#the next three lines create new variables from the separated data
Districts.data$pref.d1 <- sapply(1:nrow(Districts.data),function(i) Districts.prefs[[i]][1] )
Districts.data$pref.d2 <- sapply(1:nrow(Districts.data),function(i) Districts.prefs[[i]][2] )
Districts.data$pref.d3 <- sapply(1:nrow(Districts.data),function(i) Districts.prefs[[i]][3] )

Districts.data$district.prefs <- NULL #remove the original 

####### Move on to the voters ###########

###### read in the data ######
VoterStart <- RowScanner("NetLogo.csv","{breed voters}",start=DistrictStart+Globals$n.districts)
Voters.data <- scan("NetLogo.csv", what=Turtles.list, sep=",", nlines=Globals$n.districts*Globals$"num-voters-per-district", skip=VoterStart-1,na.strings="") 
Voters.data <- as.data.frame(Voters.data,stringsAsFactors=FALSE)

###### Clean it up ######
Voters.data <- DataThinner(Voters.data)  
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

####### Move on to the Activists ###########

###### read in the data ######
ActivistStart <- RowScanner("NetLogo.csv","{breed activists}",start=VoterStart+Globals$n.districts*Globals$"num-voters-per-district")
Activists.data <- scan("NetLogo.csv", what=Turtles.list, sep=",", nlines=Globals$n.districts*Globals$"num-activists-per-district", skip=ActivistStart-1,na.strings="") 
Activists.data <- as.data.frame(Activists.data,stringsAsFactors=FALSE)

###### Clean it up ######
Activists.data <- DataThinner(Activists.data)  
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

####### Move on to the Parties ###########

###### read in the data ######
PartyStart <- RowScanner("NetLogo.csv","{breed parties}",start=ActivistStart+Globals$n.districts*Globals$"num-activists-per-district")
Parties.data <- scan("NetLogo.csv", what=Turtles.list, sep=",", nlines=Globals$n.parties, skip=PartyStart-1,na.strings="") 
Parties.data <- as.data.frame(Parties.data,stringsAsFactors=FALSE)

###### Clean it up ######
Parties.data <- DataThinner(Parties.data)  
Parties.data <- as.data.frame(lapply(Parties.data, function(x) gsub("\\]|\\[", "",x)),stringsAsFactors=FALSE) #get rid of brackets

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

####### Move on to the Candidates ###########

###### read in the data ######
CandStart <- RowScanner("NetLogo.csv","{breed cands}",start=PartyStart+Globals$n.parties)
Candidates.data <- scan("NetLogo.csv", what=Turtles.list, sep=",", nlines=Globals$n.parties*Globals$n.districts, skip=CandStart-1,na.strings="") 
Candidates.data <- as.data.frame(Candidates.data,stringsAsFactors=FALSE)

###### Clean it up ######
Candidates.data <- DataThinner(Candidates.data)  
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

