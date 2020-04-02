logiderle <- function(i, j, n) {
   #all(i[2:length(i)] > j[1:(length(j) - 1)])
   stopifnot(
      is.double(i),
      is.double(j),
      is.numeric(n),
      length(i) == 1 |  all(i[2:length(i)] > j[1:(length(j) - 1)])
   )
   sapply(1:n, function(l) any(i <= l & l <= j))
}

logiderle(i=c(1,4), j=c(1, 6), n=7)

logiderle(i=c(1,4,7), j = c(1,6,8), n=10)
typeof(c(1,0,34))
mode(c(1,4,5))

?all

