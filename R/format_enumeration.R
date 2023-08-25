#' Convert the character vector to a string of enumerated values.
#'
#' @param x the character vector to be converted
#' @param sep the separator string that will be placed between the `x` vector values
#' @param max the maximum number of values from `x` to be placed into the string
#' @param dots the string appended to the result if the `x` vector is larger than `max`
#' @param ... further arguments that are ignored
#' @return The string with concatenated values from `x`
#'
#' @author Michal Burda
#' @export
format_enumeration <- function(x,
                               sep = ", ",
                               max = 5,
                               dots = "...",
                               ...) {
    .must_be_atomic_vector(x)
    .must_be_character_scalar(sep)
    .must_be_integerish_scalar(max)
    .must_be_character_scalar(dots)

    x <- as.character(x)
    if (length(x) > max) {
        length(x) <- max
        x <- c(x, dots)
    }

    paste(x, collapse = sep)
}
