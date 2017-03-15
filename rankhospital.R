rankhospital <- function(state, outcome, num="best")
{
  ## suppress the warnings
  options(warn = -1)
  set <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  ## for checking if the outcome is valid or not
  if(outcome == "heart attack")
    x <- 11
  else if(outcome == "heart failure")
    x <- 17
  else if(outcome == "pneumonia")
    x <- 23
  else
    stop("Invalid outcome")
  
  ## to split the dataset through state
  splitstate <- split(set, set$State)
  info <- splitstate[[state]]
  
  ## to validate the state
  if(is.null(info))
    stop("Invalid state")
  name <- info[,c(2,x)]
  
  ## to sort the dataset
  ordered <- name[order(as.numeric(name[[2]]),name[[1]],na.last = NA),]
  if(num =="best")
    num <- 1
  if(num =="worst")
    num <- nrow(ordered)
  
  ## to validate the num value
  if(num > nrow(ordered))
    NA
  else
  ordered[num,1]
}