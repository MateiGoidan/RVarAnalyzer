# RVarAnalyzer

## Description

R - Shiny app that allows users to craft random variables from personalized probability density functions, construct marginal and conditional densities, and visually depict distribution nuances.

## Features

1. Parsing of a function given as input in the app.
2. [Geometrical plotting](https://github.com/MateiGoidan/RVarAnalyzer/blob/main/RVarAnalyzer/tasks/b.R) of the function.
3. Checks if a given two-variable function 𝑓(𝑥,𝑦) can be integrated using [**Fubini’s theorem**](https://github.com/MateiGoidan/RVarAnalyzer/blob/main/RVarAnalyzer/tasks/a%26(c%26d).R) by computing the double integral in two different orders and verifying if the results match.
4. Determines whether a given function 𝑓(𝑥,𝑦) is a [probability density function (PDF)](https://github.com/MateiGoidan/RVarAnalyzer/blob/main/RVarAnalyzer/tasks/a%26(c%26d).R) by checking if it is non-negative and if its double integral equals 1.
5. Generates continuous **random variables** based on a user-defined probability density function [**using the rejection sampling method**](https://github.com/MateiGoidan/RVarAnalyzer/blob/main/RVarAnalyzer/tasks/a%26(c%26d).R), returning the results in a matrix format.
6. Compute the [conditioned probability](https://github.com/MateiGoidan/RVarAnalyzer/blob/main/RVarAnalyzer/tasks/e.R) for both random variables.
7. Compute the [mean and variance](https://github.com/MateiGoidan/RVarAnalyzer/blob/main/RVarAnalyzer/tasks/h.R).

## Contributors
- [Goidan Matei](https://github.com/MateiGoidan)
- [Roseti Bogdan](https://github.com/RBogdanDev)
- [Baciu Rares](https://github.com/RBogdanDev)
- [Sanda Tiberiu](https://github.com/SandaMarian-Tiberiu)
