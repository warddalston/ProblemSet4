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

#### read in the turtles districts
Start <- RowScanner("NetLogo.csv","TURTLES")
Finish <-RowScanner("NetLogo.csv","{breed voters}")
Turtles.colnames <- scan("NetLogo.csv", what="", sep=",", nlines=1, skip=Start,na.strings="")
Turtles.list <- as.list(rep("",length(Turtles.colnames)))
names(Turtles.list) <- Turtles.colnames
Districts.data <- scan("NetLogo.csv", what=Turtles.list, sep=",", nlines=Finish-Start, skip=Start+1,na.strings="")
Districts.data <- as.data.frame(Districts.data)
