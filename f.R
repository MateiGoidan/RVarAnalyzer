fPcomun <- function(dataset, xVal, yVal) {
  nTotal <- length(dataset$X)
  
  nCondition <- sum(dataset$X == xVal & dataset$Y == yVal)
  
  return(nCondition / nTotal)
}