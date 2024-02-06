frepcomgen <- function(n, m) {
  generatedDistribution <- matrix(nrow = n + 2, ncol = m + 2)
  
  generatedDistribution[2:(n + 1), 1] <- 1:n   
  generatedDistribution[1, 2:(m + 1)] <- 1:m   
  
  xVariables <- table(sample(1:n, 100000, replace = TRUE))
  yVariables <- table(sample(1:m, 100000, replace = TRUE))
  
  X <- 1:n
  X <- lapply(X, function(i) xVariables[as.character(i)]/100000)
  X <- unlist(X)
  
  Y <- 1:m
  Y <- lapply(Y, function(i) yVariables[as.character(i)]/100000)
  Y <- unlist(Y)
  
  generatedDistribution[2:(n + 1), m + 2] <- X
  generatedDistribution[n + 2, 2:(m + 1)] <- Y
  
  associatedProbabilities <- matrix(outer(X, Y, "*"), nrow = n, ncol = m)
  generatedDistribution[2:(n + 1), 2:(m + 1)] <- associatedProbabilities
  
  generatedDistribution[n + 2, m + 2] = 1
  
  nullRows <- rep(0, n + 2)
  numbNullRows <- n + 1
  nullCols <- rep(0, m + 2)
  numbNullCols <- m
  
  while(numbNullRows > 0 || numbNullCols > 0) {
    randRow = sample(2:(n + 2), 1)
    randCol = sample(2:(m + 2), 1)
    
    if(nullRows[randRow] == 0 && nullCols[randCol] == 0) {
      nullRows[randRow] <- 1
      numbNullRows <- numbNullRows - 1
      
      generatedDistribution[randRow, randCol] <- NA
    } else if(numbNullCols != 0 && numbNullRows == 0) {
      thirdRows <- list()
      thirdCols <- list() 
      isPossible <- 1
      
      naIndices <- which(is.na(generatedDistribution), arr.ind = TRUE)
      checkRow <- c(randRow, randCol)
      
      if(any(apply(naIndices, 1, function(row) all(row == checkRow)))) {
        isPossible <- 0
      } else {
        for (i in 1:nrow(naIndices)) {
          if(naIndices[i, 1] == randRow) {
            thirdCols <- c(thirdCols, naIndices[i, 2])
          } else if(naIndices[i, 2] == randCol) {
            thirdRows <- c(thirdRows, naIndices[i, 1])
          }
        }
        
        if(length(thirdRows) != 0 && length(thirdCols) != 0) {
          for (i in thirdRows) {
            for (j in thirdCols) {
              if((i != 1 && j != 1) && is.na(generatedDistribution[i,j])) {
                isPossible <- 0
              } 
            }
          }
        }
        
        if(isPossible == 1) {
          nullCols[randCol] <- 1
          numbNullCols <- numbNullCols - 1
          
          generatedDistribution[randRow, randCol] <- NA
        }
      }
    }
  }
  
  return(generatedDistribution)
}

fcomplrepcom <- function(jointDistribution) {
  numbRows <- nrow(jointDistribution)
  numbCols <- ncol(jointDistribution)
  
  if(is.na(jointDistribution[numbRows, numbCols])) {
    jointDistribution[numbRows, numbCols] <- 1
  }
  
  numbNA <- length(which(is.na(jointDistribution))) - 3
  
  while (numbNA != 0) {
    copynumbNA <- numbNA
    
    print(numbNA)
    
    for (i in 2:numbRows) {
      if(length(which(is.na(jointDistribution[i, 2:numbCols]))) == 1) {
        position <- which(is.na(jointDistribution[i, 2:numbCols]), arr.ind = TRUE) + 1
        
        if(position == numbCols) {
          sumRow <- sum(jointDistribution[i,2:(position - 1)])
          jointDistribution[i, position] <- sumRow
          numbNA <- numbNA - 1
          break;
        } else if(position == 2) {
          sumRow <- sum(jointDistribution[i, 3:(numbCols - 1)])
          jointDistribution[i, position] <- jointDistribution[i,numbCols] - sumRow
          numbNA <- numbNA - 1
          break;
        } else if(position == numbCols - 1) {
          sumRow <- sum(jointDistribution[i, 2:(position - 1)])
          jointDistribution[i, position] <- jointDistribution[i,numbCols] - sumRow
          numbNA <- numbNA - 1
          break;
        } else {
          sumRow <- sum(jointDistribution[i, 2:(position - 1)]) + sum(jointDistribution[i, (position + 1):(numbCols - 1)])
          jointDistribution[i, position] <- jointDistribution[i,numbCols] - sumRow
          numbNA <- numbNA - 1
          break;
        }
      }
    }
    
    for (j in 2:numbCols) {
      if(length(which(is.na(jointDistribution[2:numbRows, j]))) == 1) {
        position <- which(is.na(jointDistribution[2:numbRows, j]), arr.ind = TRUE) + 1
        
        if(position == numbRows) {
          sumCol <- sum(jointDistribution[2:(position - 1), j])
          jointDistribution[position, j] <- sumCol
          numbNA <- numbNA - 1
          break;
        } else if(position == 2) {
          sumCol <- sum(jointDistribution[3:(numbRows - 1), j])
          jointDistribution[position, j] <- jointDistribution[numbRows, j] - sumCol
          numbNA <- numbNA - 1
          break;
        } else if (position == numbRows - 1) {
          sumCol <- sum(jointDistribution[2:(position - 1), j])
          jointDistribution[position, j] <- jointDistribution[numbRows, j] - sumCol
          numbNA <- numbNA - 1
          break;
        } else {
          sumCol <- sum(jointDistribution[2:(position - 1), j]) + sum(jointDistribution[(position + 1):(numbRows - 1), j])
          jointDistribution[position, j] <- jointDistribution[numbRows, j] - sumCol
          numbNA <- numbNA - 1
          break;
        }
      }
    }
    
    if(copynumbNA == numbNA) {
      firstNA <- which(is.na(jointDistribution[2:numbRows, 2:numbCols]), arr.ind = TRUE)[1, ]
      jointDistribution[firstNA[1] + 1, firstNA[2] + 1] <- 0
      numbNA <- numbNA - 1
    }
  }
  
  return(jointDistribution)
}

n <- 10
m <- 8

result <- frepcomgen(n, m)

jointDistribution <- fcomplrepcom(result)

# 1. Calculul Covarianței:

# Extrage valorile variabilelor X și Y din matricea jointDistribution
# Extrage probabilitățile asociate pentru X și Y
# Calcularea E(X), E(Y), E(XY)
# Calcularea Cov(5X + 9, -3Y - 2)
# Afișarea rezultatului

XValues <- jointDistribution[2:(n + 1), 1]
YValues <- jointDistribution[1, 2:(m + 1)]

probX <- jointDistribution[2:(n + 1), m + 2]
probY <- jointDistribution[n + 2, 2:(m + 1)]

E_X <- sum((5 * XValues + 9) * probX)
E_Y <- sum((-3 * YValues - 2) * probY)
E_XY <- sum(outer((5 * XValues + 9),(-3 * YValues - 2), "*") * jointDistribution[2:(n + 1), 2:(m + 1)])

Cov_5X_9_minus_3Y_2 <- E_XY - E_X * E_Y

print(Cov_5X_9_minus_3Y_2)

# 2. Calculul Probabilității Condiționate:

# Extrage valorile variabilelor X și Y din matricea jointDistribution
# Extrage probabilitățile asociate pentru X și Y
# Calcularea probabilității condiționate P(0 < X < 0.8 | Y > 0.3)
# Calcularea probabilității P(Y > 0.3)
# Calcularea probabilității condiționate P(0 < X < 0.8 | Y > 0.3)
# Afișarea rezultatului
XValues <- jointDistribution[2:(n + 1), 1]
YValues <- jointDistribution[1, 2:(m + 1)]

probX <- jointDistribution[2:(n + 1), m + 2]
probY <- jointDistribution[n + 2, 2:(m + 1)]

probX_givenY <- jointDistribution[2:(n + 1), 2:(m + 1)] / probY  # Probabilitatea P(X, Y)
probX_givenY_Y_0.3 <- probX_givenY[XValues > 0 & XValues < 0.8, YValues > 0.3]  # Subsetul de valori dorite

probY_0.3 <- sum(probY[YValues > 0.3])

probConditional <- sum(probX_givenY_Y_0.3) / probY_0.3

print(probConditional)

# 3. Calculul Probabilității Comune:

# Calcularea probabilității P(X > 0.2 și Y < 1.7)
# Afișarea rezultatului
probX_and_Y <- jointDistribution[2:(n + 1), 2:(m + 1)]
probX_0.2_Y_1.7 <- probX_and_Y[XValues > 0.2 & YValues < 1.7]

prob_desired <- sum(probX_0.2_Y_1.7)

print(prob_desired)