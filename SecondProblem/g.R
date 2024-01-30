#Programul calculeaza pentru variabilele unidimensionale momentele initiale si momentele centrate
#pornind de la densitatea comuna a distributiei normale bivariate
#
#Pentru momentele initiale se calculeaza E[X^k] si E[Y^k]
#Pentru momentele centrate se calculeaza E[(X - E[X])^k] si E[(Y - E[Y])^k]
#
#Media variabilei bidimensionale este calculata folosind functia colMeans (Functie pentru a calcula media
#pe coloane)
#
#Dispersia variabilei bidimensionale este calculata folosind functia var (Functie pentru a calcula varianta)

if (!requireNamespace("mvtnorm", quietly = TRUE)) install.packages("mvtnorm")
library(mvtnorm)

mu <- c(0, 0) 
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

set.seed(1)
sample_size <- 10000
sample <- mvtnorm::rmvnorm(n = sample_size, mean = mu, sigma = Sigma)

calculate_moments <- function(distribution, order) {
  moments_initial <- numeric(order)
  moments_centered <- numeric(order)
  for (i in 1:order) {
    
    moments_initial[i] <- mean(distribution^i)
    
    moments_centered[i] <- mean((distribution - mean(distribution))^i)
  }
  return(list(initial = moments_initial, centered = moments_centered))
}

moments_X <- calculate_moments(sample[, 1], 4)
moments_Y <- calculate_moments(sample[, 2], 4)

cat("Momentele pentru X: Inițiale -", moments_X$initial, "Centrate -", moments_X$centered, "\n")
cat("Momentele pentru Y: Inițiale -", moments_Y$initial, "Centrate -", moments_Y$centered, "\n")

mean_bidimensional <- colMeans(sample)
dispersia_bidimensional <- var(sample)

cat("Media bidimensionala: ", mean_bidimensional, "\n")
cat("Dispersia bidimensionala: \n")
print(dispersia_bidimensional)
