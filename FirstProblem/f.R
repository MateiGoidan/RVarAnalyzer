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


fPcomun <- function(dataset, xVal, yVal) {
  # Numărul total de observații în setul de date
  nTotal <- length(dataset$X)
  
  # Numărul de observații care îndeplinesc condiția X == xVal și Y == yVal
  nCondition <- sum(dataset$X == xVal & dataset$Y == yVal)
  
  return(nCondition / nTotal)
}

# Generare de date de exemplu
set.seed(123)  # Setarea unei semințe pentru reproducibilitate
X <- sample(1:3, 100, replace = TRUE)  # Exemplu pentru variabila X
Y <- sample(4:6, 100, replace = TRUE)  # Exemplu pentru variabila Y

# Creare unui set de date
dataset <- data.frame(X = X, Y = Y)

# Calculul probabilității pentru perechea (2, 5)
prob <- fPcomun(dataset, 2, 5)

# Afișarea rezultatului
cat("Probabilitatea pentru perechea (2, 5) este:", prob, "\n")
