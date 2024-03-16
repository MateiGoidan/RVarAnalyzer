# Verifica dacă pachetul 'pracma' este instalat
if (!requireNamespace("pracma", quietly = TRUE)) {
  # Daca nu este instalat, il instaleaza
  install.packages("pracma")
}

library(pracma)

mediaSiDispersia <- function(g, fx, domeniuValori) {
  # y = g(X) e o noua variabila aleatoare
  
  # folosesc formula pt media functiilor de x
  e_y <- integral(Vectorize(function(x){g(x) *fx(x)}),domeniuValori[1],domeniuValori[2])
  
  # pentru dispersie, mai folosim odata formula mediei functiilor de x pentru a afla X^2
  e_y2 <- integral(Vectorize(function(x){x^2 * g(x) *fx(x)}),domeniuValori[1],domeniuValori[2])
  dispersie <- e_y2 - e_y^2
  
  print(paste("Media: ",e_y))
  print(paste("Dispersia:", dispersie))
}

# Examplu
#f1 <- function(x)(x^2)
#f2 <- function(x) (1 * exp(1)^(-1 * x))

#mediaSiDispersia(f1,f2, c(0,Inf)) # 2, 20