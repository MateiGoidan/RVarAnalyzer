#Functiile calculeaza media, dispersia, momentele initiale si momentele centrate de ordin 4 pentru variabilele
#unidimensionale si cea bidimensionala
#
#In cod, sunt declarate 3 functii pentru variabilele aleatoare:
#f - functia pentru variabila unidimensionala X
#g - functia pentru variabila unidimensionala Y
#f_xy - functia de densitate comuna pentru variabila bidimensionala (X=x, Y=y)
#
#Pentru a calcula media si momentul initial pentru variabilele unidimensionale avem functia meanAndInitialMoment1
#Pentru a calcula dispersia si momentul centrat pentru variabilele unidimensionale avem functia varianceAndCentralMoment1
#
#Pentru a calcula media si momentul initial pentru variabila bidimensionala avem functile:
#Pentru variabila X: meanAndInitialMoment21
#Pentru variabila Y: meanAndInitialMoment22
#Pentru a calcula dispersia si momentul centrat pentru variabila bidimensionala avem functile:
#Pentru variabila X: varianceAndCentralMoment23
#Pentru variabila Y: varianceAndCentralMoment24
#
#Functiile descrise anterior calculeaza integralele simple si duble asociate mediei, dispersiei, momentelor initiale si centrate


#Unidimensionale
meanAndInitialMoment1 <- function(fun, poww, interval = c(-Inf, Inf)){
  integrate(function(x){ x ^ poww * f(x) }, interval[1], interval[2])$value
}

#Unidimensionale
varianceAndCentralMoment1 <- function(fun, poww, meanOf, interval = c(-Inf, Inf)){
  integrate(function(x){ (x - meanOf) ^ poww * f(x) }, interval[1], interval[2])$value
}

#Bidimensionale
meanAndInitialMoment21 <- function(f, poww, intervalx, intervaly) {
  integrate(
    Vectorize(function(x) {
      integrate(function(y) x ^ poww * f(x, y), intervaly[1], intervaly[2])$value
    }), intervalx[1], intervalx[2]
  )$value
}

#Bidimensionale
meanAndInitialMoment22 <- function(f, poww, intervalx, intervaly) {
  integrate(
    Vectorize(function(y) {
      integrate(function(x) y ^ poww * f(x, y), intervalx[1], intervalx[2])$value
    }), intervaly[1], intervaly[2]
  )$value
}

#Bidimensionale
varianceAndCentralMoment23 <- function(f, poww, meanOfX, intervalx, intervaly) {
  integrate(
    Vectorize(function(x) {
      integrate(function(y) (x - meanOfX) ^ poww * f(x, y), intervaly[1], intervaly[2])$value
    }), intervalx[1], intervalx[2]
  )$value
}

#Bidimensionale
varianceAndCentralMoment24 <- function(f, poww, meanOfY, intervalx, intervaly) {
  integrate(
    Vectorize(function(y) {
      integrate(function(x) (y - meanOfY) ^ poww * f(x, y), intervalx[1], intervalx[2])$value
    }), intervaly[1], intervaly[2]
  )$value
}


initialMomentAndCentralMomentForUnidimensional <- function(f, g, poww){
  meanOfX <- meanAndInitialMoment1(f, 1)
  initialMomentX <- meanAndInitialMoment1(f, poww)
  dispersionOfX <- varianceAndCentralMoment1(f, 2, meanOfX)
  centralMomentX <- varianceAndCentralMoment1(f, poww, meanOfX)
  
  meanOfY <- meanAndInitialMoment1(g, 1)
  initialMomentY <- meanAndInitialMoment1(g, poww)
  dispersionOfY <- varianceAndCentralMoment1(g, 2, meanOfY)
  centralMomentY <- varianceAndCentralMoment1(g, poww, meanOfY)
  
  print("Values of X UNIDIMENSIONAL")
  print(paste("Mean of X", meanOfX))
  print(paste("Initial moment X", initialMomentX))
  print(paste("Dispersion of X", dispersionOfX))
  print(paste("Central moment of X", centralMomentX))
  
  print("Values of Y UNIDIMENSIONAL")
  print(paste("Mean of Y", meanOfY))
  print(paste("Initial moment Y", initialMomentY))
  print(paste("Dispersion of Y", dispersionOfY))
  print(paste("Central moment of Y", centralMomentY))
}

initialMomentAndCentralMomentForBidimensional <- function(f, poww, intervalx, intervaly)
{
  meanOfX <- meanAndInitialMoment21(f, 1, intervalx, intervaly)
  initialMomentX <- meanAndInitialMoment21(f, poww, intervalx, intervaly)
  dispersionOfX <- varianceAndCentralMoment23(f, 2, meanOfX, intervalx, intervaly)
  centralMomentX <- varianceAndCentralMoment23(f, poww, meanOfX, intervalx, intervaly)
  
  meanOfY <- meanAndInitialMoment22(f, 1, intervalx, intervaly)
  initialMomentY <- meanAndInitialMoment22(f, poww, intervalx, intervaly)
  dispersionOfY <- varianceAndCentralMoment24(f, 2, meanOfY, intervalx, intervaly)
  centralMomentY <- varianceAndCentralMoment24(f, poww, meanOfY, intervalx, intervaly)
  
  print("Values of X BIDIMENSIONAL")
  print(paste("Mean of X", meanOfX))
  print(paste("Initial moment X", initialMomentX))
  print(paste("Dispersion of X", dispersionOfX))
  print(paste("Central moment of X", centralMomentX))
  
  print("Values of Y BIDIMENSIONAL")
  print(paste("Mean of Y", meanOfY))
  print(paste("Initial moment Y", initialMomentY))
  print(paste("Dispersion of Y", dispersionOfY))
  print(paste("Central moment of Y", centralMomentY))
}

#Exemplu
#
#f <- function(x) { exp(-x^2) }
#
#g <- function(x) { 2*x + 3 }
#
#f_xy <- function(x, y) {
#  (3 * x^2 + 3 * y^2) / 2
#}
#
#initialMomentAndCentralMomentForUnidimensional(f, g, 4)
#initialMomentAndCentralMomentForBidimensional(f_xy, 4, c(0,1), c(0,1))
