# Funcția Fubini: Această metodă verifică dacă o funcție bidimensională f(x, y) poate fi integrată folosind teorema lui Fubini. Funcția f(x, y) este introdusă de utilizator.
# Funcția Fubini calculează integrala dublă a funcției f(x, y) în două moduri diferite: 
# O dată integrând mai întâi în raport cu x, apoi în raport cu y (notat I_xy), și o dată integrând mai întâi în raport cu y, apoi în raport cu x (notat I_yx).
# Dacă ambele valori sunt egale și finite, funcția returnează valoarea integralei și un mesaj care confirmă că teorema lui Fubini poate fi aplicată.
# Altfel, funcția returnează un mesaj care indică faptul că funcția nu respectă condițiile de integrabilitate.

# Funcția DensitateProb: Această funcție verifică dacă o funcție bidimensională f(x, y) este o densitate de probabilitate.
# În primul rând, verifică dacă toate valorile funcției sunt non-negative. Apoi, calculează integrala dublă a funcției f(x, y).
# Dacă integrala este egală cu 1, funcția returnează un mesaj care confirmă că f(x, y) este o densitate de probabilitate.
# Altfel, funcția returnează un mesaj care indică faptul că f(x, y) nu este o densitate de probabilitate.

# Funcția generate_random_va: Această funcție generează un obiect de tip variabilă aleatoare continuă pornind de la o densitate de probabilitate introdusă de utilizator.
# Funcția poate genera variabile aleatoare unidimensionale sau bidimensionale, în funcție de alegerea utilizatorului. Funcția generează un număr de n seturi de variabile aleatoare, fiecare cu m variabile generate.
# Variabilele aleatoare sunt generate folosind metoda de respingerii:
# Pentru fiecare variabilă aleatoare, funcția generează perechi aleatoare (x, y) până când găsește o pereche care satisface condiția f(x, y) > u respectiv g(x) > u, unde u este o variabilă aleatoare uniformă.
# Variabilele aleatoare generate sunt returnate sub formă de matrice, cu n rânduri și m coloane.



# Solicită utilizatorului să introducă funcția f(x, y)
input1 <- readline(prompt="Introduceți funcția: ")

# Înlocuiește toate instanțele de 'x' și 'y' precedate de un număr cu '*x' și '*y'
input1 <- gsub("(\\d)([xy])", "\\1*\\2", input1)

# Înlocuiește toate instanțele de 'x' și 'y' urmate de un număr cu 'x*' și 'y*'
input1 <- gsub("([xy])(\\d)", "\\1*\\2", input1)

# Înlocuiește toate instanțele de '*' care sunt precedate sau urmate de o paranteză sau de '/'
input1 <- gsub("(\\*\\B)", "", input1)
input1 <- gsub("(\\B\\*)", "", input1)

f <- function(x, y) {
  return(eval(parse(text=input1)))
}

# Solicită utilizatorului să introducă funcția g(x)
input2 <- readline(prompt="Introduceți funcția: ")

# Înlocuiește toate instanțele de 'x' și 'y' precedate de un număr cu '*x' și '*y'
input2 <- gsub("(\\d)([x])", "\\1*\\2", input2)

# Înlocuiește toate instanțele de 'x' și 'y' urmate de un număr cu 'x*' și 'y*'
input2 <- gsub("([x])(\\d)", "\\1*\\2", input2)

# Înlocuiește toate instanțele de '*' care sunt precedate sau urmate de o paranteză sau de '/'
input2 <- gsub("(\\*\\B)", "", input2)
input2 <- gsub("(\\B\\*)", "", input2)

g <- function(x) {
  return(eval(parse(text=input2)))
}

Fubini <- function(x_lower, x_upper, y_lower, y_upper) {
  # Calculăm integrala dublă a funcției f(x, y) integrând mai întâi în raport cu x, apoi în raport cu y.
  I_xy <- integrate(function(x) {
    sapply(x, function(x) {
      integrate(function(y) f(x, y), y_lower, y_upper)$value
    })
  }, x_lower, x_upper)
  
  # Calculăm integrala dublă a funcției f(x, y) integrând mai întâi în raport cu y, apoi în raport cu x.
  I_yx <- integrate(function(y) {
    sapply(y, function(y) {
      integrate(function(x) f(y, x), x_lower, x_upper)$value
    })
  }, y_lower, y_upper)
  
  # Verificăm dacă funcția este integrabilă Riemann.
  if(is.null(I_xy$value) || is.null(I_yx$value)) {
    return("Funcţia nu este integrabilă Riemann.\n")
  }
  
  # Verificăm dacă funcția respectă condiția de integrabilitate.
  if(I_xy$value != I_yx$value || abs(I_xy$value) == Inf || abs(I_yx$value) == Inf) {
    return("Funcţia nu respectă condiția de integrabilitate.\n")
  }
  
  # Dacă funcția respectă condițiile de integrabilitate, returnăm valoarea integralei și un mesaj de confirmare.
  return(paste("Funcţia poate fi rezolvată cu teorema lui Fubini!\nValoarea integralei este: ", I_xy$value, "\n"))
}

DensitateProb <- function(x_lower, x_upper, y_lower, y_upper) {
  # Verificăm non-negativitatea
  is_non_negative <- all(outer(seq(x_lower, x_upper, length.out = x_upper - x_lower), seq(y_lower, y_upper, length.out = y_upper - y_lower), Vectorize(f)) >= 0)
  
  if(!is_non_negative)
    return("Funcţia f(x, y) nu este densitate de probabilitate.\n")
  
  # Calculăm integrala dublă a funcției
  I_xy <- integrate(function(x) {
    sapply(x, function(x) {
      integrate(function(y) f(x, y), y_lower, y_upper)$value
    })
  }, x_lower, x_upper)
  
  if(I_xy$value != 1)
    return("Funcţia f(x, y) nu este densitate de probabilitate.\n")
  
  return("Funcţia f(x, y) este densitate de probabilitate.\n")
}

generate_random_va <- function(n, m, domeniu) {
  # Corectăm greşelile de input
  if(m > 1)
    m <- 2
  else m <- 1
  
  if(n <= 0)
    n <- 1
  
  # Extragem domeniul/domeniile pe care o să generăm
  if(!is.null(domeniu$Y)) {
    X <- domeniu$X
    Y <- domeniu$Y
  } else X <- domeniu$X
  
  data <- matrix(nrow = n, ncol = m)
  for (i in 1:n) {
    repeat {
      if(is.null(Y)) {
        # Generăm un număr aleatoriu uniform pentru x care aparţin domeniului X
        x <- runif(1, X[1], X[2])
        # Generăm un număr aleatoriu uniform pentru y care aparţin domeniului X
        y <- runif(1, X[1], X[2])
      } else {
        # Generăm un număr aleatoriu uniform pentru x care aparţin domeniului X
        x <- runif(1, X[1], X[2])
        # Generăm un număr aleatoriu uniform pentru y care aparţin domeniului Y
        y <- runif(1, Y[1], Y[2])
      }
      
      # Verificăm ce a ales utilizatorul
      if(ncol(data) > 1) {
        # Dacă utilizatorul a ales v a bidimensională
        if (runif(1) < f(x, y)) {
          data[i, ] <- c(x, y)
          break
        } # Dacă utilizatorul a ales v a unidimensională
      } else if(y < g(x)) {
        data[i][1] <- x
        break
      }
    }
  }
  return(data)
}

# Solicită utilizatorului să introducă şi domeniul lui x respectiv y
X <- readline(prompt="Introduceți domeniul lui x: ")
Y <- readline(prompt="Introduceți domeniul lui y: ")

# Împărţim șirurile de caractere în vectori de șiruri mai mici
X <- strsplit(X, " ")[[1]]
Y <- strsplit(Y, " ")[[1]]

# Convertim șirurile de caractere în numere
X <- as.numeric(X)
Y <- as.numeric(Y)

# Aranjăm elementel dacă au fost date greşit
X <- sort(X)
Y <- sort(Y)

# Testăm Fubini
result <- cat(Fubini(X[1], X[2], Y[1], Y[2]))

# Testăm DensitateProb
result <- cat(DensitateProb(X[1], X[2], Y[1], Y[2]))

# Testăm generate_random_va pe v a bidimensionale
d <- list(X = X, Y = Y)
result <- generate_random_va(2, 2, d)

print(result)

# Testăm generate_random_va pe v a unidimensionale
d <- list(X = X)
result <- generate_random_va(2, 1, d)

print(result)