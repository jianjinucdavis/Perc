#' recode a vector
#' 
#' \code{myrecode} recode values in a vector using new values
#' 
#' @param variable The vector to be recoded
#' @param oldvalue values to be recoded
#' @param newvalue values used to replace old values
#' @return A new vector of the same length but replaced the old values using the new values
#' 
#' @details old values in new values need to be corresponded to each other. 
#' If you want to replace several values using one value, you need to put it in a list.
#' 
#' @examples
#' 
#' x <- rep(c("A", "B"), 10)
#' y <- myrecode(x, c("A", "B"), c(0, 1))
#' 
#' x <- 1:100
#' y <- myrecode(x, list(x[x < 50], x[x >= 50]), c(0,1))
#' 
#' x <- matrix(1:100, nrow = 10, ncol = 10)
#' y <- myrecode(x, list(x[x < 50], x[x >= 50]), c(0,1))


myrecode <- function(variable, oldvalue, newvalue) {
  stopifnot(length(oldvalue) == length(newvalue))
  for (i in seq_along(oldvalue)){
    variable[variable %in% oldvalue[[i]]] <- newvalue[i]
  }
  return(variable)
}