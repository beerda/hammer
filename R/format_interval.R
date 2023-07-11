#' Convert two numeric vectors to strings formatted as intervals
#'
#' @param from the first vector of values. If `to` is `NULL`, the length of `from`
#'      must be 2.
#' @param to the second vector of values. If given, the length of `to` must be equal
#'      to the length of `from`.
#' @param sep the separator string
#' @param ... further arguments passed to [format_number()]
#' @return A string vector with elements formatted as: `"from -- to`
#'
#' @author Michal Burda
#' @export
format_interval <- function(from,
                            to = NULL,
                            sep = " -- ",
                            ...) {
    .must_be_numeric_vector(from)
    .must_be_character_scalar(sep)

    if (is.null(to)) {
        .must_have_length(from, 2)
        res <- format_interval(from[1], from[2], sep = sep, ...)

    } else {
        .must_be_numeric_vector(to)
        .must_have_equal_lengths(from, to)
        res <- paste0(format_number(from, ...),
                      sep,
                      format_number(to, ...))
    }

    res
}
