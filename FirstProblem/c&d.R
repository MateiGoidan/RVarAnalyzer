source('a&b.r')

frepmarginal <- function(jointDistribution) {
  # Stergem ultima linie si coloana ca sa putem calcula mai eficient
  jointDistribution <- jointDistribution[-nrow(jointDistribution), -ncol(jointDistribution)]
  
  # Repartitiile marginale ale lui X si Y sunt sumele de pe linii respectiv coloane
  X <- apply(jointDistribution, 1, sum)
  Y <- apply(jointDistribution, 2, sum)
  
  return(list(X = X, Y = Y))
}

fpropcov <- function(X, Y, a, b, c, d) {
  Z <- c(a * X, b * Y)
  T <- c(c * X, d * Y)
  # Covarianta unei variabile cu o constanta este zero
  if(length(Z) < 2 || length(T) < 2)
    return(0)
  
  # Calculam mediile variabilelor Z si T + media produselor variabilelor Z si T
  # Facem asta in stilul Corectiei Bessel unde luam n-1 in loc de n pentru a obtine o estimare neinclinata a covariantei
  E_ZT = sum(Z*T) / (length(Z) - 1)
  E_Z = sum(Z) / (length(Z) - 1)
  E_T = sum(T) / (length(T) - 1)
  
  # Aplicam formula Cov(X, Y) = E[XY] - E[X]E[Y]
  Cov = E_ZT - (E_Z * E_T)
  
  return(Cov)
}

result <- frepcomgen(7, 10)

result <- fcomplrepcom(result)

c <- frepmarginal(result)

a <- matrix(c$X, nrow = 1)

b <- matrix(c$Y, nrow = 1)

print(a)
print(b)

c <- fpropcov(a, b, 0, 4, 1, 3)

print(c)

print(result)