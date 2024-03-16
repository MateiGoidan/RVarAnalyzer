# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(plotly)
library(pracma)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  navbarPage(
    "RVarAnalyzer",
    
    tabPanel(
      "Unidimensional",
      
      sidebarPanel(
        h2("Unidimensional Variable Tool"),
        
        textInput("fx", "Function:", value = "x^2"),
        
        fluidRow(
          column(3,
                 numericInput("x11", "X values", value = 0),
          ),
          
          column(3,
                 numericInput("x12", "", value = 1),
          )
        ),
        
        actionButton("btn1", "Submit", class = "btn-primary"),
        
        
        
        h4("Construire variabile aleatoare"),
        actionButton("btn3", "Unidimensionale", class = "btn-primary"),
        
        textInput("gx", "Introduceti g(x) pentru a calcula media si densitatea v. a. gX", value = "exp(1)^(-x)"),
        actionButton("btn5", "Submit", class = "btn-primary")
      ),
      
      mainPanel(
        fluidRow(
          column(5, 
                 tableOutput("randTable1"),
          ),
          
          column(5, 
                 verbatimTextOutput("valMD1")
          )
        )
      ),
    ),
    
    tabPanel(
      "Bidimensional",
      
      sidebarPanel(
        h2("Bidimensional Variable Tool"),
        
        h5("Visualize your function's graph and explore details for your randomly generated continuous variables."),
        
        textInput("fxy", "Double Variable Function", value = "(3x^2 + 3y^2) / 2"),
        
        fluidRow(
          column(3,
                 numericInput("x1", "X values", value = 0),
          ),
          
          column(3,
                 numericInput("x2", "", value = 1),
          ),
          
          column(3,
                 numericInput("y1", "Y values", value = 0),
          ),
          
          column(3,
                 numericInput("y2", "", value = 1),
          ),
        ),
        
        fluidRow(
          column(3,
                 actionButton("btn2", "Submit", class = "btn-primary"),
          ),
          column(9,
                 checkboxInput("info", "Show details"),
          )
        ),
        
        textOutput("validatePositive"),
        textOutput("validateIntegral"),
        
        h3("Generate Random Variables"),
        
        h5("Generate a continuous random variable object using the function as the probability density."),
        
        actionButton("btn4", "Generate", class = "btn-primary")
      ),
      
      mainPanel(
        plotlyOutput(outputId = "graph"),
        
        fluidRow(
          column(8,
                 conditionalPanel(
                   condition = "input.info == true",
                   h3("Information:"),
                   verbatimTextOutput("validateFubini"),
                   verbatimTextOutput("validateDensitate"),
                   verbatimTextOutput("calcBidm"),
                 ),
          ),
          
          column(4,
                 conditionalPanel(
                   condition = "input.btn4 > 0",
                   h3("Random Variables:"),
                 ),
          tableOutput("randTable2"),
          )
        ),
      ),
    )
  ),
)

# Define Server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  expresion1 <- reactive({
    # Solicită utilizatorului să introducă funcția g(x)
    input2 <- input$fx
    
    # Înlocuiește toate instanțele de 'x' și 'y' precedate de un număr cu '*x' și '*y'
    input2 <- gsub("(\\d)([x])", "\\1*\\2", input2)
    
    # Înlocuiește toate instanțele de 'x' și 'y' urmate de un număr cu 'x*' și 'y*'
    input2 <- gsub("([x])(\\d)", "\\1*\\2", input2)
    
    # Înlocuiește toate instanțele de '*' care sunt precedate sau urmate de o paranteză sau de '/'
    input2 <- gsub("(\\*\\B)", "", input2)
    input2 <- gsub("(\\B\\*)", "", input2)
    
    return(input2)
  })
  
  expresion2 <- reactive({
    # Assuming 'fxy' is the id of your text input
    input1 <- input$fxy
    
    # Înlocuiește toate instanțele de 'x' și 'y' precedate de un număr cu '*x' și '*y'
    input1 <- gsub("(\\d)([xy])", "\\1*\\2", input1)
    
    # Înlocuiește toate instanțele de 'x' și 'y' urmate de un număr cu 'x*' și 'y*'
    input1 <- gsub("([xy])(\\d)", "\\1*\\2", input1)
    
    # Înlocuiește toate instanțele de '*' care sunt precedate sau urmate de o paranteză sau de '/'
    input1 <- gsub("(\\*\\B)", "", input1)
    input1 <- gsub("(\\B\\*)", "", input1)
    
    return(input1)
  })
  
  expresion3 <- reactive({
    # Solicită utilizatorului să introducă funcția g(x)
    input3 <- input$fx
    
    # Înlocuiește toate instanțele de 'x' și 'y' precedate de un număr cu '*x' și '*y'
    input3 <- gsub("(\\d)([x])", "\\1*\\2", input3)
    
    # Înlocuiește toate instanțele de 'x' și 'y' urmate de un număr cu 'x*' și 'y*'
    input3 <- gsub("([x])(\\d)", "\\1*\\2", input3)
    
    # Înlocuiește toate instanțele de '*' care sunt precedate sau urmate de o paranteză sau de '/'
    input3 <- gsub("(\\*\\B)", "", input3)
    input3 <- gsub("(\\B\\*)", "", input3)
    
    return(input3)
  })
  
  g <- function(x) {
    eval(parse(text = expresion1()))
  }
  
  f <- function(x, y) {
    eval(parse(text = expresion2()))
  }
  
  h <- function(x) {
    eval(parse(text = expresion1()))
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
      return("The function can not be Riemann integrated.\n")
    }
    
    # Verificăm dacă funcția respectă condiția de integrabilitate.
    if(I_xy$value != I_yx$value || abs(I_xy$value) == Inf || abs(I_yx$value) == Inf) {
      return("The function does not satisfy the integrability condition.\n")
    }
    
    # Dacă funcția respectă condițiile de integrabilitate, returnăm valoarea integralei și un mesaj de confirmare.
    return(paste("The function can be solved using Fubini's theorem!\nThe value of the integral is: ", I_xy$value, "\n"))
  }
  
  
  DensitateProb <- function(x_lower, x_upper, y_lower, y_upper) {
    # Verificăm non-negativitatea
    is_non_negative <- all(outer(seq(x_lower, x_upper, length.out = x_upper - x_lower), seq(y_lower, y_upper, length.out = y_upper - y_lower), Vectorize(f)) >= 0)
    
    if(!is_non_negative)
      return("The function f(x, y) is not the probability density of the joint distribution.\n")
    
    # Calculăm integrala dublă a funcției
    I_xy <- integrate(function(x) {
      sapply(x, function(x) {
        integrate(function(y) f(x, y), y_lower, y_upper)$value
      })
    }, x_lower, x_upper)
    
    tolerance <- 1e-10 
    
    if((I_xy$value - 1) < tolerance)
      return(paste("The function f(x, y) is the probability density of the joint distribution.\n"))
    
    return(paste("The function f(x, y) is not the probability density of the joint distribution.\n"))
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
        if(is.null(domeniu$Y)) {
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
  
  checkValuesPositive <- function(f, xy, zw, i) {
    check <- TRUE
    for(x in seq(xy[1], xy[2], i))
    {
      for(y in seq(zw[1], zw[2], i))
      {
        if(f(x, y) < 0)
          check <- FALSE
        break
      } 
      if(check == FALSE)
        break
    }
    return(check)
  }
  
  checkValueOfIntegral <- function(f, xy, zw) {
    tolerance <- 1e-6
    valueOfIntegral <- integral2(f, xy[1], xy[2], zw[1], zw[2])$Q
    return(abs(valueOfIntegral - 1) < tolerance)
  }
  
  fMarginalX <- function(f, x, lowerLimit, higherLimit) integrate(function(y) f(x, y), lowerLimit, higherLimit)$value
  fMarginalY <- function(f, y, lowerLimit, higherLimit) integrate(function(x) f(x, y), lowerLimit, higherLimit)$value
  
  fXGivenY <- function(f, x, y, lowerLimit, higherLimit) f(x, y) / fMarginalY(f, y, lowerLimit, higherLimit)
  fYGivenX <- function(f, y, x, lowerLimit, higherLimit) f(x, y) / fMarginalX(f, x, lowerLimit, higherLimit)
  
  #Unidimensionale
  meanAndInitialMoment1 <- function(fun, poww, interval = c(-Inf, Inf)){
    integrate(function(x){ x ^ poww * f(x) }, interval[1], interval[2])$value
  }
  
  #Unidimensionale
  varianceAndCentralMoment1 <- function(fun, poww, meanOf, interval = c(-Inf, Inf)){
    integrate(function(x){ (x - meanOf) ^ poww * f(x) }, interval[1], interval[2])$value
  }
  
  #Bidimensionale
  meanAndInitialMoment21 <- function(f, poww, intervalx, intervaly) {
    integrate(
      Vectorize(function(x) {
        integrate(function(y) x ^ poww * f(x, y), intervaly[1], intervaly[2])$value
      }), intervalx[1], intervalx[2]
    )$value
  }
  
  #Bidimensionale
  meanAndInitialMoment22 <- function(f, poww, intervalx, intervaly) {
    integrate(
      Vectorize(function(y) {
        integrate(function(x) y ^ poww * f(x, y), intervalx[1], intervalx[2])$value
      }), intervaly[1], intervaly[2]
    )$value
  }
  
  #Bidimensionale
  varianceAndCentralMoment23 <- function(f, poww, meanOfX, intervalx, intervaly) {
    integrate(
      Vectorize(function(x) {
        integrate(function(y) (x - meanOfX) ^ poww * f(x, y), intervaly[1], intervaly[2])$value
      }), intervalx[1], intervalx[2]
    )$value
  }
  
  #Bidimensionale
  varianceAndCentralMoment24 <- function(f, poww, meanOfY, intervalx, intervaly) {
    integrate(
      Vectorize(function(y) {
        integrate(function(x) (y - meanOfY) ^ poww * f(x, y), intervalx[1], intervalx[2])$value
      }), yValues[1], yValues[2]
    )$value
  }
  
  mediaSiDispersia <- function(g, fx, domeniuValori) {
    # y = g(X) e o noua variabila aleatoare
    
    # folosesc formula pt media functiilor de x
    e_y <- integral(Vectorize(function(x){g(x) *fx(x)}),domeniuValori[1],domeniuValori[2])
    
    # pentru dispersie, mai folosim odata formula mediei functiilor de x pentru a afla X^2
    e_y2 <- integral(Vectorize(function(x){x^2 * g(x) *fx(x)}),domeniuValori[1],domeniuValori[2])
    dispersie <- e_y2 - e_y^2
    
    result <- c(e_y, dispersie)
    return(result)
  }
  
  fcovandcor <- function(jointDensity, X, Y) {
    fX <- function(y){integrate(function(x){jointDensity(x, y)}, X[1], X[2]) $ value}
    fY <- function(x){integrate(function(y){jointDensity(x, y)}, Y[1], Y[2]) $ value}
    
    FX <- integrate(function(x){x * fX(x)}, X[1], X[2]) $ value
    FY <- integrate(function(y){y * fY(y)}, Y[1], Y[2]) $ value
    
    cat(FX, " ", FY, "\n")
    
    covaration <- integrate(Vectorize(function(y){integrate(function(x){x * y * jointDensity(x, y)}, X[1], X[2]) $ value}), Y[1], Y[2]) $ value - FX * FY
    
    VarX <- (integrate(Vectorize(function(x){integrate(function(x){x * x * jointDensity(x, x)}, X[1], X[2]) $ value}), X[1], X[2]) $ value) - FX * FX
    VarY <- (integrate(Vectorize(function(y){integrate(function(y){y * y * jointDensity(y, y)}, Y[1], Y[2]) $ value}), Y[1], Y[2]) $ value) - FY * FY
    
    correlation <- covaration / (VarX * VarY)
    
    result <- c(covaration, correlation)
  }
  
  observeEvent(input$btn1, {
    output$txtout1 <- renderText({
      paste(resultg())
    })
  })
  
  observeEvent(input$btn5, {
    xLower <- input$x11
    xUpper <- input$x12
    
    xValues <- c(xLower, xUpper)
    
    resultMD <- mediaSiDispersia(h, g, xValues)
    answer <- c(paste("Mean: ", resultMD[1]), paste("Variance: ", resultMD[2]))
    
    output$valMD1 <- renderText({
      paste(answer, collapse = "\n")
    })
  })
  
  observeEvent(input$btn2, {
    xLower <- input$x1
    xUpper <- input$x2
    yLower <- input$y1
    yUpper <- input$y2
    
    resultF <- Fubini(xLower, xUpper, yLower, yUpper)
    resultD <- DensitateProb(xLower, xUpper, yLower, yUpper)
    
    xValues <- c(xLower, xUpper)
    yValues <- c(yLower, yUpper)
    
    valCovAndCor <- fcovandcor(f, xValues, yValues)
    answerCovAndCor <- c(paste("Covariance: ", valCovAndCor[1]), paste("Correlation Coefficient: ", valCovAndCor[2]))
    
    output$validateFubini <- renderText({
      paste(resultF)
    })
    
    output$validateDensitate <- renderText({
      paste(resultD)
    })
    
    if(!checkValuesPositive(f, xValues, yValues, 0.01)) {
      output$validatePositive <- renderText({
        paste("Error: In the domain, the function has negative values.")
      })
    } else {
      output$validatePositive <- renderText({
        paste("")
      })
    }
    
    if(!checkValueOfIntegral(f, xValues, yValues)) {
      output$validateIntegral <- renderText({
        paste("Error: The probability is not equal to 1.")
      })
    } else {
      output$validateIntegral <- renderText({
        paste("")
      })
    }
    
    output$calcBidm <- renderText({
      paste(answerCovAndCor, collapse = "\n")
    })
    
    output$graph<- renderPlotly({
      x_limits <- c(xLower, xUpper)
      y_limits <- c(xLower, yUpper)
      
      x_vals <- seq(x_limits[1], x_limits[2], length.out = 100)
      y_vals <- seq(y_limits[1], y_limits[2], length.out = 100)
      
      z_vals <- outer(x_vals, y_vals, Vectorize(function(x, y) f(x, y)))
      
      plot_ly(
        x = x_vals,
        y = y_vals,
        z = z_vals,
        type = "surface",
      )
    })
  })
  
  observeEvent(input$btn3, {
    xLower <- input$x1
    xUpper <- input$x2
    
    n <- 5
    m <- 1
    d <- list(X = c(xLower, xUpper))
    
    resultRandom <- generate_random_va(n, m, d)
    
    output$randTable1 <- renderTable({
      resultRandom
    })
  })
  
  observeEvent(input$btn4, {
    xLower <- input$x1
    xUpper <- input$x2
    yLower <- input$y1
    yUpper <- input$y2
    
    n <- 5
    m <- 2
    d <- list(X = c(xLower, xUpper), Y = c(yLower, yUpper))
    
    resultRandom <- generate_random_va(n, m, d)
    
    output$randTable2 <- renderTable({
      resultRandom
    })
  })
}

shinyApp(ui = ui, server = server)
