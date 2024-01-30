#Programul defineste o densitate comuna, si afla densitatile marginale si conditionate ale variabilelor
#Densitatea comuna a fost definita pentru o distributie normala bivariata
#
#Folosim pachetul mvtnorm pentru a crea un esantion de date din distributia normala bivariata
#
#Pentru densitatile marginale, integram de la -inf la inf peste valorile din distributia normala bivariata
#
#Pentru densitatile coditionate, facem raportul dintre densitatea comuna si repartitia marginala
#a variabilei X respectiv Y, afland astfel probabilitatea lui X (sau Y) atunci cand Y (sau X) este cunoscut

if (!requireNamespace("mvtnorm", quietly = TRUE)) install.packages("mvtnorm")
library(mvtnorm)

f <- function(x, y) {
  mu <- c(0, 0)
  Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  mvtnorm::dmvnorm(cbind(x, y), mean = mu, sigma = Sigma)
}

f_X <- function(x) integrate(function(y) f(x, y), -Inf, Inf)$value
f_Y <- function(y) integrate(function(x) f(x, y), -Inf, Inf)$value

f_X_given_Y <- function(x, y) f(x, y) / f_Y(y)
f_Y_given_X <- function(y, x) f(x, y) / f_X(x)

x_val <- 0
y_val <- 0
cat("Densitatea marginală f_X la x =", x_val, "este", f_X(x_val), "\n")
cat("Densitatea marginală f_Y la y =", y_val, "este", f_Y(y_val), "\n")
cat("Densitatea condiționată f_X_given_Y la x =", x_val, "și y =", y_val, "este", f_X_given_Y(x_val, y_val), "\n")
cat("Densitatea condiționată f_Y_given_X la y =", y_val, "și x =", x_val, "este", f_Y_given_X(y_val, x_val), "\n")
