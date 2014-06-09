rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  ## For each state, find the hospital of the given rank
  states <- sort(unique(data[,7]))
  #   v = lapply(states, function(x) rankhospital(x, outcome, best))
  v = lapply(states, function(x) {
    t <- data[,7] == x
    if (outcome == "heart attack") d <- data[t, c(2, 11)]
    else if (outcome == "heart failure") d <- data[t, c(2, 17)]
    else if (outcome == "pneumonia") d <- data[t, c(2, 23)]
    d[,2] <- as.numeric(d[,2])
    r <- d[order(d[,2], d[,1]),]
    if (num == "best") rr = 1
    else if (num == "worst") rr = nrow(na.omit(r))
    else rr = num
    r[rr, 1]
  })
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  o <- data.frame(unlist(v), states)
  colnames(o) <- c("hospital", "state")
  o
}