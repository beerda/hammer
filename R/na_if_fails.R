#' Return NA if expression fails
#'
#' This function evaluates an expression and returns `NA` if it fails, i.e.,
#' if it stops with error.
#'
#' @param expr an expression to evaluate
#' @param na value to return if the expression fails (default is `NA_real_`)
#' @return The result of the expression if it succeeds, or `na` if it fails
#' @author Michal Burda
#' @export
na_if_fails <- function(expr, na = NA_real_) {
    res <- try(expr)

    if (inherits(res, "try-error")) na else res
}
