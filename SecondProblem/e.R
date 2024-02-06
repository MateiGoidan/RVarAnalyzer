#Programul foloseste densitatea comuna definita de utilizator si afla densitatile marginale si
#conditionate ale variabilelor
#
#Mai intai trebuie verificat daca functia este pozitiva pe toate valorile domeniului deoarece densitatile
#marginale trebuie sa fie pozitive
#Daca este indeplinita conditia anterioara, trebuie verificata daca integrala are valoarea 1
#
#Daca ambele conditii sunt indeplinite, atunci calculam densitatile marginale si conditionate
#Pentru densitatile marginale, pentru X fixam o valoare X=x si integram f(x, y), unde y variaza
#                          iar pentru Y fixam o valoare Y=y si integram f(x, y) unde x variaza
#Pentru densitatile coditionate, facem raportul dintre densitatea comuna si repartitia marginala
#a variabilei X respectiv Y, afland astfel probabilitatea lui X (sau Y) atunci cand Y (sau X) este cunoscut

if (!requireNamespace("pracma", quietly = TRUE)) install.packages("pracma")
library(pracma)

checkValuesPositive <- function(f, xy, zw, i) {
  check <- TRUE
  for(x in seq(xy[1], xy[2], i))
  {
    for(y in seq(zw[1], zw[2], i))
    {
      if(f(x, y) < 0)
        check <- FALSE
      break
    } 
    if(check == FALSE)
      break
  }
  return(check)
}

checkValueOfIntegral <- function(f, xy, zw) {
  tolerance <- 1e-6
  valueOfIntegral <- integral2(f, xy[1], xy[2], zw[1], zw[2])$Q
  return(abs(valueOfIntegral - 1) < tolerance)
}

fMarginalX <- function(f, x, lowerLimit, higherLimit) integrate(function(y) f(x, y), lowerLimit, higherLimit)$value
fMarginalY <- function(f, y, lowerLimit, higherLimit) integrate(function(x) f(x, y), lowerLimit, higherLimit)$value

fXGivenY <- function(f, x, y, lowerLimit, higherLimit) f(x, y) / fMarginalY(f, y, lowerLimit, higherLimit)
fYGivenX <- function(f, y, x, lowerLimit, higherLimit) f(x, y) / fMarginalX(f, x, lowerLimit, higherLimit)

findMarginalAndConditionalDensities <- function(f, x, y, xy, zw){
  if(!checkValuesPositive(f, xy, zw, 0.01)) {
    print("Eroare! Pe domeniu, functia are valori negative.")
    return()
  }
  if(!checkValueOfIntegral(f, xy, zw)) {
    print("Eroare! Probabilitatea nu este egala cu 1")
    return()
  }
  
  #Densitate marginala variabila continua X
  print(paste("Densitate marginala X:", fMarginalX(f, x, zw[1], zw[2])))
  
  #Densitate marginala variabila continua Y
  print(paste("Densitate marginala Y:", fMarginalY(f, x, xy[1], xy[2])))
  
  #Densitate conditionata de Y
  print(paste("Densitate conditionata de Y = ", y, ": ", fXGivenY(f, x, y, zw[1], zw[2]), sep=""))
  
  #Densitate conditionata de X
  print(paste("Densitate conditionata de X = ", x, ": ", fXGivenY(f, y, x, xy[1], xy[2]), sep=""))
}

#Exemplu
#test1 <- function() {
#  f <- function(x, y) (8 / 3) * x ^ 3 * y
#  findMarginalAndConditionalDensities(f, 0.5, 1.5, c(0, 1), c(1, 2))
#}
#
#test2 <- function() {
#  f<- function(x, y) (3/ 2) * (x ^2 + y ^ 2)
#  findMarginalAndConditionalDensities(f, 0, 0.5, c(0, 1), c(0, 1))
#}
#
#test1()
#test2()
