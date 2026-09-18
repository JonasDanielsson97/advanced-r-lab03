#' Greatest common divisor using the Euclidean algorithm
#'
#' Computes the greatest common divisor of two integers using
#' the Euclidean algorithm.
#'
#' @param a A numeric scalar representing an integer.
#' @param b A numeric scalar representing an integer.
#' @return The greatest common divisor of `a` and `b`.
#' @references
#' https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @export
#' @examples
#' euclidean(100, 1000)
#' euclidean(123612, 13892347912)

euclidean <- function(a, b) {
    stopifnot(
        is.numeric(a),
        is.numeric(b),
        length(a) == 1,
        length(b) == 1,
        a %% 1 == 0,
        b %% 1 == 0
    )

# Managing negative numbers by taking absolute values
    a <- abs(a)
    b <- abs(b)

# Implementing the Euclidean algorithm to find the greatest common divisor (GCD)
    while (b != 0) {
        temp <- b
        b <- a %% b
        a <- temp
    }
    return(a)
}