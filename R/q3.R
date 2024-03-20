#' Compute the third quartile (75%)
#'
#' @param x the numeric vector
#' @param na_rm whether to ignore `NA`
#' @returns The third quartile
#'
#' @author Michal Burda
#' @export
q3 <- function(x,
               na_rm = FALSE) {
    .must_be_numeric_vector(x)
    .must_be_flag(na_rm)

    quantile(x,
             probs = 0.75,
             na.rm = na_rm,
             names = FALSE)
}
