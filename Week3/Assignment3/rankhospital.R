rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% data[,7]) stop("invalid state")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  ## Return hospital name in that state with the given rank 30-day death rate
  t <- data[,7] == state
  if (outcome == "heart attack") d <- data[t, c(2, 11)]
  else if (outcome == "heart failure") d <- data[t, c(2, 17)]
  else if (outcome == "pneumonia") d <- data[t, c(2, 23)]
  d[,2] <- as.numeric(d[,2])
  r <- d[order(d[,2], d[,1]),]
  if (num == "best") rr = 1
  else if (num == "worst") rr = nrow(na.omit(r))
  else rr = num
  r[rr, 1]
}