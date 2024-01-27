fverind <- function(jointDistribution) {
  numbRows <- nrow(jointDistribution)
  numbCols <- ncol(jointDistribution)
  
  X_marginal <- jointDistribution[1:(numbRows - 1), numbCols]
  Y_marginal <- jointDistribution[numbRows, 1:(numbCols - 1)]
  
  productProbabilities <- outer(X_marginal, Y_marginal)
  
  for (i in 1:(numbRows - 1)) {
    for (j in 1:(numbCols - 1)) {
      if (abs(jointDistribution[i, j] - productProbabilities[i, j]) != 0) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}

fvernecor <- function(jointDistribution) {
  numbRows <- nrow(jointDistribution)
  numbCols <- ncol(jointDistribution)
  
  X_marginal <- jointDistribution[numbRows, 1:(numbCols - 1)]
  Y_marginal <- jointDistribution[1:(numbRows - 1), numbCols]
  
  EX <- sum(X_marginal * (1:(numbCols - 1)))
  EY <- sum(Y_marginal * (1:(numbRows - 1)))
  
  covariance <- 0
  for (i in 1:(numbRows - 1)) {
    for (j in 1:(numbCols - 1)) {
      covariance <- covariance + (i - EX) * (j - EY) * jointDistribution[i, j]
    }
  }
  
  return(abs(covariance) == 0)
}
