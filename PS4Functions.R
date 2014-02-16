########## Functions for use in reading in NetLogo objects ########

#RowScanner - Allows one to determine which row of a dataset begins with a desired string

# Reads in a dataset one row at a time.  It then compares each element of the row to the user supplied target element, and if they match, it breaks and returns the number of rows containing the target.  Useful for generically getting the number of rows to skip when trying to read in specific sections of the data from a Net Logo object.  

#input: file - a string giving a file that can be read by scan()
#       target - a string that the user is searching for
#       tries - the number of rows in the data to search for the target.  Defaults to 10,000.
#       start - the row in which to start looking for the target.  A numeric. Defaults to row 1
#       ... subsequent arguements to pass to scan()

#output: a scalar value, the row number where the target is located (so that if this output goes into scan as the skip arguement, it will begin with the lines below the target, which is ideal with a dataset with headers, like NetLogo data).  

#Author: Dalston G. Ward

RowScanner <- function(file,target,tries=10000,start=1,...){
  start <- start-1
  for(i in start:tries){
    input <- scan(file=file, what="", sep=",", nlines=1, skip=i,quiet=TRUE,...)
    if(any(input%in%target)){
      cat("To get the row you want, skip",i,"lines \n")
      return(i+1)
      break
    }
  }
}

#IdenticalCheck - See if every element of a vector is identical

#This function returns a true or false, based on whether every element of a vector is identical to the first element of the vector (and hence, if all are equal!).  Useful for cleaning up data. 

#input: x - a vector

#output: logical

#Author: Dalston G. Ward

IdenticalCheck <- function(x) {
  all(sapply(x[-1],identical,x[1])) #sapply does the comparison's element wise, and if any of them are not identical (ie a single false), then the over all result is false.  Otherwise, it is true.  
} 

#DataThinner - Gets rid of columns from a data frame that are either constant or all missing. 

#This function takes as input a dataframe, and returns the same data frame with all constant and entirely missing columns removed.  It does this using the "all()" function to identify missing columns and the "identical()" function to identify constant columns.  It then subsets out these rows.  The user has the option to only remove missing columns or to remove both missing and constant columns (entirely missing columns are a subset of entirely constant columns)

#input: x - a data frame
#       reduce - a vector of what to remove.  Can be either "missing" or "both".  Defaults to both.

#output: a reduced data frame

#Author: Dalston G. Ward

DataThinner <- function(x,reduce="both"){
  if(reduce=="both"){
 identical.drop <- apply(x,2,IdenticalCheck) #use IdenticalCheck to pick out columns that are cosntant 
  x <- x[,!identical.drop]
 return(x)
  }
  if(reduce=="missing"){
  missing.drop <- apply(x,2,function(y) all(is.na(y))) # pick out columns that are entirely missing
  x <- x[,!missing.drop]
  return(x)
  }
}