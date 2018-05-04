#' Double
#'
#' Compute the double value of a vector
#'
#' The double is calculated by multiplying each value by 2.
#'
#' @param x A vector
#' @param ... Unused
#'
#' @return A vector containing the double values.
#'
#' @examples
#' Double(runif(3))
#'
#' @name Double
NULL


#' @rdname Double
#' @export
Double <- function(x, ...) {
  UseMethod("Double")
}

#' @rdname Double
#' @method Double default
#' @export
Double.default <- function(x, ...) {
  # Input check
  if (!is.numeric(x)) stop("Double requires a numeric object")

  # Compute and return
  return(2*x)
}

#' @rdname Double
#' @method Double integer
#' @export
Double.integer <- function(x, ...) {
  return(2L*x)
}

