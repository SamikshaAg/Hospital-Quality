best <- function(state, outcome)
{
  dataSet <- read.csv("outcome-of-care-measures.csv")
  if(outcome =="heart attack")
    x <- 11
  else if(outcome == "heart failure")
    x <- 17
  else if(outcome == "pneumonia")
    x <- 23
  else
    stop("Invalid outcome")
  newSet <- split(dataSet,dataSet$State, drop=TRUE)
  newSet1 <- newSet[[state]]
 if(is.null(nrow(newSet1)))
   stop("Invalid state")
  newSet2 <- newSet1[,c(2,x)]
  minimum <- min(as.numeric(as.character(newSet2[[2]])), na.rm=TRUE)
  newSet3 <- split(newSet2, newSet2[[2]])
  newSet4 <- newSet3[[as.character(minimum)]]
 
 if(length(newSet4[[1]])==1)
   newSet4[1]
  else
  {
    newSet5 <- newSet4[1]
    newSet6 <- as.character(sort(newSet5$Hospital.Name))
    newSet6[1]
  }

  newSet4[1]
}