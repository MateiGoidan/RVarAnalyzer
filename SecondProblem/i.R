#Programul calculeaza diferite probabilitati pentru variabile aleatoare unidimensionale si bidimensionale
#
#Daca variabila este unidimensionala, iar distributia este normala, calculam distributia cumulativa 
#si aflam probabilitatea ca variabila unidimensionala sa se afle in intervalul [lower, upper]
#
#Daca variabila este bidimensionala, iar distributia este bivariata, calculam media, distributia standard,
#si daca parametrul conditional este true, calculam probabilitatea ca variabila X sa se afle in intervalul
#[lower, upper] fiind conditionata de Y
#
#Daca parametrul conditional este false, calculam probabilitatea ca ambele variabile aleatoares sa fie in 
#intervalul specificat

if (!require(MASS)) install.packages("MASS")
library(MASS)

P <- function(type, distribution, params, range = c(-Inf, Inf), conditional = FALSE, cond_value = NULL) {
  if (type == "unidimensional") {
    if (distribution == "normal") {
      mean <- params$mean
      sd <- params$sd
      lower <- range[1]
      upper <- range[2]
      return(pnorm(upper, mean, sd) - pnorm(lower, mean, sd))
    }
  } else if (type == "bidimensional") {
    if (distribution == "normal_bivariate") {
      mean <- c(params$mean_x, params$mean_y)
      sd <- matrix(c(params$sd_x^2, params$correlation * params$sd_x * params$sd_y,
                     params$correlation * params$sd_x * params$sd_y, params$sd_y^2), nrow = 2)
      
      if (conditional && !is.null(cond_value)) {
        mean_cond <- mean[1] + sd[1,2] / sd[2,2] * (cond_value - mean[2])
        sd_cond <- sqrt(sd[1,1] - sd[1,2]^2 / sd[2,2])
        lower <- range[1]
        upper <- range[2]
        return(pnorm(upper, mean_cond, sd_cond) - pnorm(lower, mean_cond, sd_cond))
      } else {
        lower <- c(range[[1]][1], range[[2]][1])
        upper <- c(range[[1]][2], range[[2]][2])
        return(mvtnorm::pmvnorm(lower, upper, mean = mean, sigma = sd))
      }
    }
  } else {
    stop("Tipul variabilei aleatoare specificat nu este valid.")
  }
}

P("unidimensional", "normal", list(mean = 0, sd = 1), range = c(-1, 1))

P("bidimensional", "normal_bivariate", list(mean_x = 0, mean_y = 0, sd_x = 1, sd_y = 1, correlation = 0.5), range = c(-1, 1), conditional = TRUE, cond_value = 0.5)

P("bidimensional", "normal_bivariate", list(mean_x = 0, mean_y = 0, sd_x = 1, sd_y = 1, correlation = 0), range = list(c(-1, 1), c(-1, 1)))
