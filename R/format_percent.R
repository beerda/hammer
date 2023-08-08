#' Convert the numeric vector to a character vector in a percentage formatting.
#'
#' The numeric vector is multiplied by 100, rounded, and the percent sign (`%`) is appended to it.
#'
#' @param x a numeric vector to be transformed
#' @param ... further arguments passed to [format_number()]
#' @return a character vector
#'
#' @author Michal Burda
#' @export
format_percent <- function(x, ...) {
    .must_be_numeric_vector(x)

    paste(format_number(100 * x, ...), '%')
}
