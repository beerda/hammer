#' Rearrange the vector of values by the names in other vector
#'
#' Create a vector of length equal to `by` filled with `NA` values.
#' Then put a value of `value` to corresponding places such that
#' the value of `by` equals to the name of `value`.
#'
#' @param value the named vector whose values have to be rearranged
#' @param by the rearangment specification: a character vector of
#'      names of `value`
#' @returns a new vector of the same type as `value`
#' @export
#' @examples
#' by <- c("a", "b", "b", "c", "a")
#' values <- c(a = 1, b = 2)
#' rearrange(values, by)       # returns c(1, 2, 2, NA, 1)
rearrange <- function(value,
                      by) {
    .must_be_atomic_vector(value)
    .must_be_character_vector(by)

    res <- rep(NA, length(by))
    for (n in names(value)) {
        i <- which(by == n)
        res[i] <- value[n]
    }

    res
}
