########## Functions for use in reading in NetLogo objects ########

#RowScanner - Allows one to determine which row of a dataset begins with a desired string

# Reads in a dataset one row at a time.  It then compares each element of the row to the user supplied target element, and if they match, it breaks and returns the number of rows containing the target.  Useful for generically getting the number of rows to skip when trying to read in specific sections of the data from a Net Logo object.  

#input: file - a string giving a file that can be read by scan()
#       target - a string that the user is searching for
#       tries - the number of rows in the data to search for the target.  Defaults to 10,000.
#       ... subsequent arguements to pass to scan()

#output: a scalar value, the row number where the target is located (so that if this output goes into scan as the skip arguement, it will begin with the lines below the target, which is ideal with a dataset with headers, like NetLogo data).  

#Author: Dalston G. Ward

RowScanner <- function(file,target,tries=10000,...){
  for(i in 0:tries){
    input <- scan(file=file, what="", sep=",", nlines=1, skip=i,quiet=TRUE,...)
    if(any(input%in%target)){
      cat("To get the row you want, skip",i,"lines \n")
      return(i+1)
      break
    }
  }
}