#' "add_three" function adds three numbers together and returns the result.
#'
#' DESCRIPTION FUNCTION
#' @param a A number to be added.
#' @param b A number to be added.
#' @param c A number to be added.
#' @return The sum of a, b, and c.
#' @author Dolapo Salim Olatoye
#' @examples
#' add_three(1, 2, 3) # returns 6
#' @export


add_three <- function(a, b, c) {
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
    stop("Error: All inputs must be numbers.")
  }
  d <- a + b + c
  return(d)
}

