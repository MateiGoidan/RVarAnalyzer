# fPcomun - Calculează frecvența condiționată a unei perechi de valori într-un set de date.

# Descriere:
# Această funcție calculează frecvența condiționată a unei perechi de valori (xVal și yVal) într-un set de date dat.
# Frecvența condiționată reprezintă proporția de date care îndeplinesc simultan condițiile xVal și yVal față de numărul total de observații.

# Parametri:
# - dataset: Setul de date pe care se efectuează calculul. Se așteaptă ca setul de date să conțină coloanele 'X' și 'Y'.
# - xVal: Valoarea pentru coloana 'X' care definește condiția pentru calcul.
# - yVal: Valoarea pentru coloana 'Y' care definește cealaltă condiție pentru calcul.

# Rezultat:
# Funcția returnează o valoare numerică reprezentând frecvența condiționată calculată conform formulei:
#   frecvența condiționată = (numărul de observații care îndeplinesc condițiile xVal și yVal) / (numărul total de observații în setul de date).

# Exemplu de utilizare:
# dataset <- data.frame(X = c(1, 2, 3, 4, 5), Y = c('A', 'B', 'A', 'B', 'A'))
# rezultat <- fPcomun(dataset, 3, 'A')
# print(rezultat)  # Output: 0.2 (20% din observații îndeplinesc condițiile x=3 și y='A')

fPcomun <- function(dataset, xVal, yVal) {
  nTotal <- length(dataset$X)
  
  nCondition <- sum(dataset$X == xVal & dataset$Y == yVal)
  
  return(nCondition / nTotal)
}
dataset <- data.frame(X = c(1, 2, 3, 4, 5), Y = c('A', 'B', 'A', 'B', 'A'))
rezultat <- fPcomun(dataset, 3, 'A')
print(rezultat)  