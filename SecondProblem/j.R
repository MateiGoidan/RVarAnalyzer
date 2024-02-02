# https://dlsun.github.io/probability/cov-continuous.html 

fcovandcor <- function(jointDensity, X, Y) {
  fX <- Vectorize(function(y){integrate(function(x){jointDensity(x, y)}, X[1], X[2]) $ value})
  fY <- Vectorize(function(x){integrate(function(y){jointDensity(x, y)}, Y[1], Y[2]) $ value})
  
  FX <- integrate(function(x){x * fX(x)}, X[1], X[2]) $ value
  FY <- integrate(function(y){y * fY(y)}, Y[1], Y[2]) $ value
  
  cat(FX, " ", FY, "\n")
  
  covaration <- integrate(Vectorize(function(y){integrate(function(x){x * y * jointDensity(x, y)}, X[1], X[2]) $ value}), Y[1], Y[2]) $ value - FX * FY
  
  print(covaration)
  
  VarX <- (integrate(Vectorize(function(x){integrate(function(x){x * x * jointDensity(x, x)}, X[1], X[2]) $ value}), X[1], X[2]) $ value) - FX * FX
  VarY <- (integrate(Vectorize(function(y){integrate(function(y){y * y * jointDensity(y, y)}, Y[1], Y[2]) $ value}), Y[1], Y[2]) $ value) - FY * FY
  
  cat(VarX, " ", VarY, "\n")
  
  correlation <- covaration / (VarX * VarY)
  
  print(correlation)
}

# Example
f <- function (x, y) {
  return (3/2 * (x^2+y^2))
}

fcovandcor(f, c(0,1), c(0, 1))
