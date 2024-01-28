#fPcond - Calcularea Probabilității Condiționate
#Descriere
#Funcția fPcond este folosită pentru a calcula probabilitatea condiționată 
#pentru două variabile aleatoare discrete, X și Y, date valorile xValue și yValue.

#Parametri
#data: Un obiect de tip dataframe care conține datele pentru variabilele X și Y.
#X: Numele coloanei corespunzătoare variabilei X în dataframe.
#Y: Numele coloanei corespunzătoare variabilei Y în dataframe.
#xValue: Valoarea pentru variabila X pentru care se calculează probabilitatea condiționată.
#yValue: Valoarea pentru variabila Y pentru care se calculează probabilitatea condiționată.

#ezultat
#Funcția returnează valoarea probabilității condiționate P(X=xValue | Y=yValue).

#Excepții
#Dacă valorile xValue sau yValue nu există în setul de date pentru variabilele X, respectiv Y, funcția va arunca o excepție cu mesajul 
#"Valorile date nu există în setul de date pentru variabilele X, Y."
#Dacă probabilitatea marginală a variabilei Y este zero, funcția va arunca o excepție cu mesajul 
#"Probabilitatea marginală a variabilei Y este zero, nu se poate calcula probabilitatea condiționată."

fPcond <- function(data, X, Y, xValue, yValue) {
  
  if (!(xValue %in% data[[X]] && yValue %in% data[[Y]])) {
    stop("Valorile date nu există în setul de date pentru variabilele X, Y.")
  }
  
  jointProb <- sum(data$X == xValue & data$Y == yValue) / nrow(data)

  marginalProbY <- sum(data$Y == yValue) / nrow(data)
  
  if (marginalProbY == 0) {
    stop("Probabilitatea marginală a variabilei Y este zero, nu se poate calcula probabilitatea condiționată.")
  }

  return(jointProb / marginalProbY)
}
