calkuj <- function(f, a, b, n=100) {
  stopifnot(
    is.numeric(a),
    is.numeric(b),
    is.numeric(n) & n >= 0
  )
  x <- seq(a, b, length.out = n+1)
  y <- f(x)
  (b-a) * (2 * sum(y[-c(1, n+1)]) + y[1]+ y[n+1]) / (2*n)
}

calkuj(dnorm, -3, 3, 1000)
calkuj(function(x) -x^2+2, -1, 1, 100)

?c
is.numeric(3)
