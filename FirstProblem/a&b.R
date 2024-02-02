# Functia frepcomgen genereaza un tabel al repartitiei comune pentru variabilele aleatoare X si Y, 
# avand n + 2 linii pentru probabilitatile lui X si m + 2 linii pentru probabilitatile lui Y. 

# Pe prima linie si prima coloana sunt indicii probabilitatilor lui X si Y, iar pe linia n + 2 si 
# coloana m + 2 sunt valorile probabilitatilor respective ale lui X si Y.

# Initial, se construieste tabelul cu repartitia comuna. Ulterior, se extrag n + m + 1 valori 
# aleatoare (numarul maxim de valori ce pot fi extrase) si sunt inlocuite cu NA.

# Variabilele n si m pot lua valori pana la 10^5.

# Functia fcomplrepcom completeaza tabelul generat de frepcomgen inlocuid toate inserarile lui NA

# Se ia pe rand fiecare linie si fiecare coloana si se verifica daca se poate sa nu afla valoarea
# probabilitati. In cazul in care se poate, in functie de pozitia in care se afla in matrice,
# calculam valoarea acesteia in functie de suma randului, respectiv coloanei. Daca nu se poate,
# continuam parcurgera in matrice.

# Daca ajungem intr-un punct in care nu se poate deduce valoarea pentru restul variabilelor NA din
# matrice. Inlocuim prima aparitie a lui NA cu 0 pentru a putea continua calculul, astfel garantand
# ca tabelul generat poate fi rezolvat. 

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
    isFound <- FALSE
    
    for (i in 2:numbRows) {
      if(length(which(is.na(jointDistribution[i, 2:numbCols]))) == 1) {
        position <- which(is.na(jointDistribution[i, 2:numbCols]), arr.ind = TRUE) + 1
        
        if(position == numbCols) {
          sumRow <- sum(jointDistribution[i,2:(position - 1)])
          jointDistribution[i, position] <- sumRow
          numbNA <- numbNA - 1
          isFound <- TRUE
          break;
        } else if(position == 2) {
          sumRow <- sum(jointDistribution[i, 3:(numbCols - 1)])
          jointDistribution[i, position] <- jointDistribution[i,numbCols] - sumRow
          numbNA <- numbNA - 1
          isFound <- TRUE
          break;
        } else if(position == numbCols - 1) {
          sumRow <- sum(jointDistribution[i, 2:(position - 1)])
          jointDistribution[i, position] <- jointDistribution[i,numbCols] - sumRow
          numbNA <- numbNA - 1
          isFound <- TRUE
          break;
        } else {
          sumRow <- sum(jointDistribution[i, 2:(position - 1)]) + sum(jointDistribution[i, (position + 1):(numbCols - 1)])
          jointDistribution[i, position] <- jointDistribution[i,numbCols] - sumRow
          numbNA <- numbNA - 1
          isFound <- TRUE
          break;
        }
      }
    }
    
    if(isFound == TRUE) { next; }
    
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

n <- 12
m <- 6

result <- frepcomgen(n, m)
print(result)

result <- fcomplrepcom(result)
print(result)

sum(result[2:(n + 1), 2:(m + 1)])
sum(result[2:(n + 1), m + 2])
sum(result[n + 2, 2:(m + 1)])
