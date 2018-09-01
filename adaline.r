trainAdaline <- function(X, y, eta, tol, maxEpocas) {
  diminX <- dim(X) 
  line <- diminX[1]
  column <- diminX[2]
  
  weights <- matrix(runif((column + 1),  -0.5, + 0.5), ncol = 1)
  X <- cbind(1, X)
  numberEpocas <- 0
  errorEpocas <- tol + 1
  
  errorVector <- matrix(nrow = 1, ncol = maxEpocas)
  while ((numberEpocas < maxEpocas) && (errorEpocas > tol)) {
    errorAux <- 0
    randomX <- sample(line)
    
    for (i in 1:line) {
      randomI <- randomX[i]
      yHat <- 1.0 * ((X[randomI , ] %*% weights))
      error <- y[randomI] - yHat
      deltaWeight <- eta * error * X[randomI, ]
      weights <- weights + deltaWeight
      errorAux <- errorAux + error * error
    }
    
    numberEpocas <- numberEpocas +1
    errorVector[numberEpocas] <- errorAux/line
    
    errorEpocas[numberEpocas] <- errorVector[numberEpocas]
  }
  retList <- list(weights, errorVector[1:numberEpocas])
  return(retList)
}

#exemplo 1
# t <- matrix(seq(0, 2 * pi, 0.1 * pi), ncol = 1)
# X <- matrix(sin(t), ncol=1)

# y <- matrix(4*X+2, ncol=1)

# retlist <- trainAdaline(X, y, 1, 10, 10)
# X <- cbind(1, X)
# weight <- as.matrix(retlist[[1]])

# yHat <- X %*% weight
# str(yHat)
# plot(1:NROW(yHat), y, type = "p", col = "blue")
# lines(1:NROW(yHat), yHat, type = "l", col = "green")
#######################################################

#Gerando dados
N <- 10
x <- matrix(runif(N)*10, ncol = 1)
#x <- matrix(seq(0, 10, 0.5), ncol = 1)
#y <- 3 * x + 5
y <- (3 * x + 5) + rnorm(length(x)) * 0.8

retlist <- trainAdaline(x, y, 0.01, 0.01, 1000)
w<-retlist[[1]]

plot(retlist[[2]], type = "l")

#Gerando dados
xrange <- matrix(seq(0, 10, 0.1), ncol = 1)
xrange <- cbind(1, xrange)
yrange <- xrange %*% w

#Estimativa de sa??da

plot(xrange[ ,2], yrange, type='l', col='blue')
points(x, y, col='red')


#eta ?? tam o passo que eu vou dar na superf??cie de erro pra chegar na solu????o, learning rate