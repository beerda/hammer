#' Compute the relative frequency (percentage) of the given level in the given vector
#'
#' The function returns the relative number (percentage) of occurences of `level` in vector `x`.
#' @param x the vector to compute the occurences of `level` in. It must be an atomic vector.
#' @param level the level to be computed. It could be either integerish value or a string or `NA`.
#' The integerish value is treated as the index of level to be counted, character value selects
#' the level to be counted by its name, and `NA` selects missing values to be counted.
#' @param use_na if `FALSE` then `NA` values are omitted prior computing the frequency.
#' @return The relative number of occurences of `level` in `x`, i.e., the fraction in [0,1].
#' @seealso [aggreg_count()]
#' @author Michal Burda
#' @export
#' @examples
#' aggreg_percent(c(F,F,T,T,T), level = 1)                          # 3/5
#' aggreg_percent(c(F,F,T,T,T), level = "FALSE")                    # 2/5
#' aggreg_percent(c("b", "a", "a"), level = "a")                    # 2/3
#' aggreg_percent(c(NA, "b", "a", "a"), level = NA, use_na = TRUE)  # 1/4
aggreg_percent <- function(x, level = 1, use_na = FALSE) {
    .must_be_atomic_vector(x)
    ..must_be_type(function(a) is_scalar_integerish(a) ||
                       is_scalar_character(a) ||
                       (length(a) == 1L && is.na(a)),
                   "integerish or character scalar")(level)
    .must_be_flag(use_na)

    if (!use_na) {
        x <- na.omit(x)
    }
    p <- aggreg_count(x, level)
    n <- length(x)

    p / n
}
