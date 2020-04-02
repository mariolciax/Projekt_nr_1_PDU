###FUNKCJA
sklej <- function(x, sep="") {
  stopifnot(
    is.list(x)
  )
  do.call(paste, x)
}


### PRZYKŁADY
sklej(list(letters[1:4], LETTERS[1:4], sep = "*"))
w <- list(c("Ala", "Kasia", "Zosia"),
          c("ma", "lubi", "zawsze"),
          c("kota", "truskawki", "wygrywa"),
          c(".",";","!"))
sklej(w, sep=" ")
przyklad <- list(c("a","b","c"),
                 as.character(1:3),
                 rep("%",3))
sklej(przyklad, sep ="*")

sklej(c("a","b","c"), sep =" ")
