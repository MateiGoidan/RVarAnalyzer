####
# Cerinta: Crearea unei funcții P care permite calculul diferitelor tipuri de
# probabilități asociate unei variabile aleatoare continue
#
# Functie: myP(f, p)
#    - unde f este o functie densitate de probabilitate (pdf)
#    - iar p este un string ce reprezinta probabilitatea (conditionata sau independenta).
#
# Obligatoriu, var se va afla in stanga operatorului


myP <- function(f, p) {
  possibleOperations = c("<=",">=","=","<",">")
  
  #Separa stringul dat pentru a-l putea utiliza
  parseExpression <- function(expression) {
    expression <- gsub(" ", "", expression)
    for(operation in possibleOperations) {
      split <- unlist(strsplit(expression, operation, fixed = TRUE))
      splitSize <- length(split)
      
      if (splitSize == 2) {
        return (c(split[1], operation, split[2]))
      }
    }
    return(c(-1))
  }
  
  #Calculam cdf P(X <= bound)
  cdf <- function(bound) { 
    return(integrate(f, -Inf, bound)$value)
  }
  
  #Transformam din string in double
  computeBound <- function(bound) {
    resolve <- switch(bound,
                      "-Inf" = -Inf,
                      "+Inf" = +Inf,
                      as.double(bound))
    return (resolve)
  }
  
  #Calculul probabilitatii
  evaluate <- function(operator, bound) {
    bound = computeBound(bound)
    integral <- cdf(bound)
    
    #Pentru > si >= scadem cdf-ul si ramane restul
    answer <- switch(
      operator,
      "=" = 0,
      "<=" = integral,
      "<" = integral,
      ">=" = 1 - integral,
      ">" = 1 - integral)
    
    return(answer)
    
  }
  
  independentProbability <- function(expression) {
    parameters <- parseExpression(expression)
    
    if(length(parameters) != 3)
      return("Eroare la parsarea probabilitatii")
    
    operator <- parameters[2]
    bound <- parameters[3]
    
    print(evaluate(operator, bound))
  }
  
  conditionalProbability <- function(expression1, expression2) {
    parameter1 <- parseExpression(expression1)
    parameter2 <- parseExpression(expression2)
    operation1 <- parameter1[2]
    operation2 <- parameter2[2]
    bound1 <- parameter1[3]
    bound2 <- parameter2[3]
    
    #Calculam P(A | B)
    answer1 <- evaluate(operation1, bound1)
    answer2 <- evaluate(operation2, bound2)
    
    #Daca una din ele este 0, atunci intersectia da 0
    if(answer1 == 0)
      return(0);
    if(answer2 == 0)
      return ("Cannot divide by zero")
    
    
    #Cazul 1: Probabilitate imposibila P(x <= 1 | x >= 10) = 0
    if (operation1 %in% c("<=","<") && operation2 %in% c(">=", ">") && bound1 >= bound2)
      return (0);
    
    #Cazul 2: Probabilitate imposibila P(x > 10 | x < 2) = 0
    if (operation1 %in% c(">=",">") && operation2 %in% c("<=", "<") && bound1 >=bound2)
      return (0);
    
    #Cazul 3: Acelasi tip de operator, iar intersectia este intervalul mai mic P(x > 5 | x > 10) -> (5, inf)
    if(operation1 %in% c(">=",">") && operation2 %in% c(">=",">"))
      if(bound1 > bound2)
        return(answer1 / answer2)
    else return (1);
    
    #Cazul 4: Acelasi tip de operator, iar intersectia este intervalul mai mic P(x < 5 | x < 10) -> (-inf, 5)
    if(operation1 %in% c("<=","<") && operation2 %in% c("<=","<"))
      if(bound1 < bound2)
        return(answer1 / answer2)
    else return (1)
    
    return ((cdf(computeBound(bound2))-cdf(computeBound(bound1)))/answer2)
    
  }
  
  parts = unlist(strsplit(p, "|", fixed = TRUE))
  len = paste(length(parts))
  switch(len,
         "0" = return("Eroare"),
         "1" = return(independentProbability(p)),
         "2" = return(conditionalProbability(parts[1], parts[2])),
  )
  return ("eroare");
  
}

#Exemplu
#g <- function (x) {
#  f <- 0.1 * (3 * (x ^ 2) + 1)
#  f[x < 0] = 0
#  f[x > 2]=0
#  return(f)
#}
#
#h <- function(x) (dunif(x))
#myP(g,"x>1|x<1.5") # 0.5897078
#myP(h,"x>0.6") # 0.4