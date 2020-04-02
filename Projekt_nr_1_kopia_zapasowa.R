### Praca projektowa nr 1 
### Rozwišzanie zadan
### Imie Nazwisko nr_indeksu kierunek (wybierz: MAT, MAD, IAD)

## ...---... Zadanie 01.01 ...---...

calka <- function(f, a, b, n = 100){
    x <- seq(a, b, length.out = n+1)
    y <- f(x)
    (b-a) * (2 * sum(y[-c(1, n+1)]) + y[1]+ y[n+1]) / (2*n)
  }
  
  calka(dnorm, -3, 3, 1000)
  calka(function(x) -x^2+2, -1, 1, 100)


## ...---... Zadanie 01.02 ...---...

sklej <- function(x, sep = ""){
  # umiesc tu swoje rozwiazanie
    is.list(x)
    do.call(paste, x)
  }
  sklej(list(letters[1:2], LETTERS[1:2], sep = "*"))
  w <- list(c("Ala", "Kasia", "Zosia"),
            c("ma", "lubi", "zawsze"),
            c("kota", "truskawki", "wygrywa"),
            c(".",";","!"))
  sklej(w, sep=" ")
  
  sklej(list(c("a","b","c"),
             as.character(1:3),
             rep("%",3)), 
        sep = "~~")
  sklej(c("a","b","c"), sep =" ")
  


## ...---... Zadanie 01.03 ...---...

repr_macierz <- function(x, eps = 1e-16){
  # umiesc tu swoje rozwiazanie
  is.matrix(x)
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
  


## ...---... Zadanie 01.04 ...---...

logiderle <- function(i, j, n){
  # umiesc tu swoje rozwiazanie
} 
