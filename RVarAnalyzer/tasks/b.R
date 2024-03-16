# Install and load required libraries
library(plotly)

# Define the function to be integrated
f <- function(x, y) {
  return(x^3 * sin(y) + 3)
}

# Define the limits of integration
x_limits <- c(0, 1)
y_limits <- c(0, 2)

# Create a grid of x and y values
x_vals <- seq(x_limits[1], x_limits[2], length.out = 100)
y_vals <- seq(y_limits[1], y_limits[2], length.out = 100)

# Create a matrix of z values based on the function
z_vals <- outer(x_vals, y_vals, Vectorize(function(x, y) f(x, y)))

# Create a 3D surface plot
plot <- plot_ly(
  x = x_vals,
  y = y_vals,
  z = z_vals,
  type = "surface",
)

# Customize the layout
layout <- list(
  scene = list(
    xaxis = list(title = "X-axis"),
    yaxis = list(title = "Y-axis"),
    zaxis = list(title = "Z-axis")
  )
)

# Combine the plot and layout
fig <- list(data = plot, layout = layout)

# Display the graph
fig
