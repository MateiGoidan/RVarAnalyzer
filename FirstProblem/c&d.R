# Funcția frepmarginal calculează repartițiile marginale pentru două variabile aleatoare, X și Y, date de o repartiție comună.
# Aceasta elimină ultima linie și coloană din repartiția comună pentru a putea calcula mai eficient.
# Apoi, calculează repartițiile marginale ale lui X și Y ca sumele pe linii, respectiv coloane.

# Funcția fpropcov calculează covarianța dintre două variabile aleatoare, Z și T, definite ca Z = aX + bY și T = cX + dY.
# Dacă Z sau T sunt constante (lungimea lor este mai mică de 2), covarianța este zero.
# Dacă Z și T sunt aceeași variabilă aleatoare, covarianța este egală cu varianța acelei variabile.
# În celelalte cazuri, funcția calculează covarianța folosind formula Cov(X, Y) = E[XY] - E[X]E[Y], unde E[XY] este media produselor variabilelor Z și T, iar E[X] și E[Y] sunt mediile variabilelor Z și T, respectiv.

source('a&b.r')

frepmarginal <- function(jointDistribution) {
  # Ştergem ultima linie şi coloană ca să putem calcula mai eficient
  jointDistribution <- jointDistribution[-nrow(jointDistribution), -ncol(jointDistribution)]
  
  # Ştergem şi prima linie şi coloană ca să putem calcula mai eficient
  jointDistribution <- jointDistribution[-1, -1]
  
  # Repartiţiile marginale ale lui X şi Y sunt sumele de pe linii respectiv coloane
  X <- apply(jointDistribution, 1, sum)
  Y <- apply(jointDistribution, 2, sum)
  
  return(list(X = X, Y = Y))
}

fpropcov <- function(X, Y, a, b, c, d) {
  Z <- a * X + b * Y
  T <- c * X + d * Y
  # Covarianţa unei variabile cu o constantă este zero
  if(length(Z) < 2 || length(T) < 2)
    return(0)
  # Covarianţa unei variabile cu ea însăși este egală cu varianța acelei variabile
  if(all(Z == T)) {
    μ2 <- (mean(Z))^2
    E_X2 <- mean(Z^2)
    # Aplicăm formula Var(X) = E(X^2) - μ^2
    Var_X <- E_X2 - μ2
    return(Var_X)
  }
  
  # Calculăm mediile variabilelor Z şi T + media produselor variabilelor Z şi T
  # Facem asta în stilul Corecţiei Bessel unde luăm n-1 în loc de n pentru a obţine o estimare neînclinată a covarianţei
  E_ZT = sum(Z*T) / (length(Z) - 1)
  E_Z = sum(Z) / (length(Z) - 1)
  E_T = sum(T) / (length(T) - 1)
  
  # Aplicăm formula Cov(X, Y) = E[XY] - E[X]E[Y]
  Cov = E_ZT - (E_Z * E_T)
  
  return(Cov)
}

c <- frepmarginal(result)

a <- matrix(c$X, nrow = 1)

b <- matrix(c$Y, nrow = 1)

#print(a)
#print(b)

if(length(a) > length(b)) {
  c <- fpropcov(a[, (ncol(a) - (ncol(a) - ncol(b)))], b, 0, 4, 1, 3)
} else if(length(a) != length(b)) {
  c <- fpropcov(a, b[, (ncol(b) - (ncol(b) - ncol(a)))], 0, 4, 1, 3)
} else {
  c <- fpropcov(a, b, 0, 4, 1, 3)
}

#print(c)

c <- fpropcov(a, a, 0, 4, 4, 0)

#print(c)

c <- fpropcov(1, 2, 0, 4, 4, 0)

#print(c)

#print(result)