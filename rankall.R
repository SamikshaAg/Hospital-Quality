rankall <- function(outcome, num="best")
{
   set <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
   if(outcome == "heart attack")
     x <- 11
   else if(outcome == "heart failure")
     x <- 17
   else if(outcome == "pneumonia")
     x <- 23
   else
     stop("Invalid outcome")
   
   states <- split(set, set$State)
   name <- lapply(states, function(elt) elt[,c(2,x)])
   temp <- lapply(name, function(dataset) sorting(dataset, num))
   vect1 <- as.character(lapply(temp, as.character))
   vect2 <- names(temp)
   res <- as.data.frame(cbind(vect1,vect2), row.names = vect2)
   colnames(res) <- c("hospital","state")
   res
}

sorting <- function(dataset, num)
{
  newset <- dataset[order(as.numeric(dataset[[2]]),dataset[[1]],na.last = NA),]
  if(num == "best")
    num <- 1
  if(num == "worst")
    num <- nrow(newset)
  newset[num,1]
}