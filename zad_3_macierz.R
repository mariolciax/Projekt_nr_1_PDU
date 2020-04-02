repr_macierz <- function(x, eps = 1e-16){
  # umiesc tu swoje rozwiazanie
  stopifnot(
    is.matrix(x),
    dim(x) > 0:0
  )
  result <- which(x != 0*eps, arr.ind =TRUE)
  val <-x[which(x > eps*0 | x < eps*0)]
  wynikowa_macierz<-cbind(result,val)
  wynikowa_macierz
}

y <- matrix(0, ncol = 5, nrow = 7)
y[c(1,2,5,6),1] <- rnorm(4)
y
repr_macierz(y)

y <- matrix(0, ncol = 5, nrow = 7)

y[1, c(1,3,4)] <- rnorm(3)
y[6,2] <- runif(1)
y
?dim
repr_macierz(y)
?matrix
0*1e-16
1e-16
?lapply
