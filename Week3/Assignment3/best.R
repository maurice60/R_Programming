best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% data[,7]) stop("invalid state")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death rate
  t <- data[,7] == state
  if (outcome == "heart attack") d <- data[t, c(2, 11)]
  else if (outcome == "heart failure") d <- data[t, c(2, 17)]
  else if (outcome == "pneumonia") d <- data[t, c(2, 23)]
  r <- d[order(as.numeric(d[,2]), d[,1]),]
  r[1,1]
}
