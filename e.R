fPcond <- function(data, X, Y, xValue, yValue) {
  
  if (!(xValue %in% data[[X]] && yValue %in% data[[Y]])) {
    stop("Valorile date nu există în setul de date pentru variabilele X, Y.")
  }
  
  jointProb <- sum(data$X == xValue & data$Y == yValue) / nrow(data)

  marginalProbY <- sum(data$Y == yValue) / nrow(data)
  
  if (marginalProbY == 0) {
    stop("Probabilitatea marginală a variabilei Y este zero, nu se poate calcula probabilitatea condiționată.")
  }

  return(jointProb / marginalProbY)
}
