
is.mids <- function(x) {
    inherits(x, "mids")
}




is.mira <- function(x) {
    inherits(x, "mira")
}



is.mipo <- function(x) {
    inherits(x, "mipo")
}


is.mitml.result <- function(x) {
  inherits(x, "mitml.result")
}



is.passive <- function(string) {
    return("~" == substring(string, 1, 1))
}


is.mads <- function(x) {
  inherits(x, "mads")
}


