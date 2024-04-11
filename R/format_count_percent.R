#' Convert the data into strings reporting the absolute and relative frequencies
#'
#' `p` is the number of positive cases and `n` is the number of all cases. The result
#' is a character vector formatted as `"<p> (<p/n> %)"`.
#'
#' @param p a numeric vector of positive cases.
#' @param n a numeric vector of all cases.
#' @param ignore_na if `FALSE`, the `NA` values will result into something like `"NA (NA %)"`.
#' If `TRUE`, such results will be replaced with an empty string. Any other value
#' will be directly used as a replacement.
#' @param ... further arguments passed to [format_percent()]
#' @returns a character vector of size `length(p)` (if `p` and `n` is specified)
#'
#' @author Michal Burda
#' @export
format_count_percent <- function(p,
                                 n,
                                 ignore_na = FALSE,
                                 ...) {
    .must_be_atomic_scalar(ignore_na)
    .must_be_numeric_vector(p)
    .must_be_numeric_vector(n)

    na <- is.na(p + n + p / n)
    perc <- format_percent(p / n, ...)
    num <- format_number(p, digits = 0)
    res <- paste(num, " (", perc, ")", sep = "")

    if (isTRUE(ignore_na)) {
        ignore_na <- ""
    }
    if (!isFALSE(ignore_na)) {
        res[na] <- ignore_na
    }

    res
}
