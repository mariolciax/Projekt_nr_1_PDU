### Praca projektowa nr 1 
### Rozwi¹zanie zadan
### Imie Nazwisko nr_indeksu kierunek (wybierz: MAT, MAD, IAD)

## ...---... Zadanie 01.01 ...---...

calka <- function(f, a, b, n = 100){
  stopifnot(
    is.numeric(a),
    is.numeric(b),
    is.numeric(n),
    is.function(f)
  )
  x <- seq(a, b, length.out = n+1)
  y <- f(x)
  (b-a) * (2 * sum(y[-c(1, n+1)]) + y[1]+ y[n+1]) / (2*n)
}



## ...---... Zadanie 01.02 ...---...

sklej <- function(x, sep = ""){
  stopifnot(
    is.list(x)
  )
  do.call(paste, x)
}


## ...---... Zadanie 01.03 ...---...

repr_macierz <- function(x, eps = 1e-16){
  stopifnot(
    is.matrix(x),
    dim(x) > 0:0
  )
  result <- which(x != 0*eps, arr.ind =TRUE)
  val <-x[which(x > eps*0 | x < eps*0)]
  wynikowa_macierz<-cbind(result,val)
  wynikowa_macierz
  
}


## ...---... Zadanie 01.04 ...---...

logiderle <- function(i, j, n){
  # umiesc tu swoje rozwiazanie
    all(i[2:length(i)] > j[1:(length(j) - 1)])
    stopifnot(
      is.double(i),
      is.double(j),
      is.numeric(n),
      length(i) == 1 |  all(i[2:length(i)] > j[1:(length(j) - 1)])
    )
    sapply(1:n, function(l) any(i <= l & l <= j))
} 
