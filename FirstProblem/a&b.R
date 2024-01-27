# Functia frepcomgen creaza tabelul repartitiei comune a v. a. X si Y, cu n + 1 linii unde n
# reprezinta numarul probabiliatilor lui X si m + 1 linii unde m reprezinta numarul 
# probabilitatilor lui Y, fara probabilitati asociative. Pe coloana m + 1 se afla valorile
# probabilitatilorlui X si pe linia n + 1 se afla valorile probabilitatilor lui Y. m si n pot
# lua valori mai mici de 10^5.
# 
# Functia fcomplrepcom completeaza tabelul generat de frepcomgen cu probabilitatile asociative

frepcomgen <- function(n, m) {
  xVariables <- table(sample(1:n, 100000, replace = TRUE))
  yVariables <- table(sample(1:m, 100000, replace = TRUE))
  
  X <- 1:n
  X <- lapply(X, function(i) xVariables[as.character(i)]/100000)
  X <- unlist(X)
  
  Y <- 1:m
  Y <- lapply(Y, function(i) yVariables[as.character(i)]/100000)
  Y <- unlist(Y)
  
  jointDistribution <- matrix(nrow = n + 1, ncol = m + 1)
  jointDistribution[1:n, m + 1] <- X
  jointDistribution[n + 1, 1:m] <- Y
  
  return(jointDistribution)
}

fcomplrepcom <- function(jointDistribution) {
  numbRows <- nrow(jointDistribution)
  numbCols <- ncol(jointDistribution)
  
  X <- jointDistribution[1:(numbRows - 1) , numbCols]
  Y <- jointDistribution[numbRows , 1:(numbCols - 1)]
  
  associatedProbabilities <- matrix(outer(X, Y, "*"), nrow = numbRows - 1, ncol = numbCols - 1)
  
  jointDistribution[1:(numbRows - 1), 1:(numbCols - 1)] <- associatedProbabilities
  #print(jointDistribution)
  
  jointDistribution[nrow(jointDistribution), ncol(jointDistribution)] <- 1
  
  return(jointDistribution)
}
