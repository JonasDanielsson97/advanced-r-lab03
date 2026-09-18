
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

    while (b != 0) {
        remainder <- a %% b
        a <- b
        b <- remainder
    }
    return(a)
}