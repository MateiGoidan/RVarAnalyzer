if (!require('plotly')) install.packages('plotly')
if (!require('dplyr')) install.packages('dplyr')

library(plotly)
library(dplyr)

set.seed(1) 
n <- 3; m <- 4; k <- 2

data <- expand.grid(X = 1:n, Y = 1:m, Z = 1:k) %>%
  mutate(Probability = runif(n * m * k))

plot_ly(data, x = ~X, y = ~Y, z = ~Z, type = 'scatter3d', mode = 'markers',
        marker = list(size = ~Probability*40, color = ~Probability, colorscale = 'Viridis')) %>%
  layout(title = 'Reprezentarea vizuala a variabilei tridimensionale X, Y, Z',
         scene = list(xaxis = list(title = 'X'),
                      yaxis = list(title = 'Y'),
                      zaxis = list(title = 'Z')))

marginal_X <- aggregate(Probability ~ X, data = data, FUN = sum)
marginal_Y <- aggregate(Probability ~ Y, data = data, FUN = sum)
marginal_Z <- aggregate(Probability ~ Z, data = data, FUN = sum)

print("Distributia marginala a lui X:")
print(marginal_X)
print("Distributia marginala a lui Y:")
print(marginal_Y)
print("Distributia marginala a lui Z:")
print(marginal_Z)
