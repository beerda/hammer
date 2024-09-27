#' Compute the relative frequency (percentage) of the given level in the given vector
#'
#' The function returns the relative number (percentage) of occurences of `level` in
#' column `var` of data frame `data`.
#' @param x the vector to compute the occurences of `level` in. It must be an atomic vector.
#' @param level the level to be computed. It could be either integerish value or a string or `NA`.
#'      The integerish value is treated as the index of level to be counted, character value selects
#'      the level to be counted by its name, and `NA` selects missing values to be counted.
#' @param use_na if `FALSE` then `NA` values are omitted prior computing the frequency.
#' @param ... further arguments passed to [format_percent()]
#' @return The relative number of occurences of `level` in `x`, i.e., the fraction number
#'      in the interval from 0 to 1.
#' @seealso [aggreg_count()]
#' @author Michal Burda
#' @export
#' @examples
#' aggreg_percent(c(FALSE, FALSE, TRUE, TRUE, TRUE), level = 1)        # 3/5
#' aggreg_percent(c(FALSE, FALSE, TRUE, TRUE, TRUE), level = "FALSE")  # 2/5
#' aggreg_percent(c("b", "a", "a"), level = "a")                       # 2/3
#' aggreg_percent(c(NA, "b", "a", "a"), level = NA, use_na = TRUE)     # 1/4
aggreg_percent <- function(data,
                           var,
                           level = 1,
                           use_na = FALSE,
                           ...) {
    .must_be_data_frame(data)
    .must_be_string(var)

    x <- data[[var]]

    .must_be_atomic_vector(x)
    ..must_be_type(function(a) is_scalar_integerish(a) ||
                       is_scalar_character(a) ||
                       (length(a) == 1L && is.na(a)),
                   "integerish or character scalar")(level)
    .must_be_flag(use_na)

    if (!use_na) {
        x <- na.omit(x)
    }
    p <- aggreg_count(data.frame(x = x), "x", level)
    n <- length(x)

    format_percent(p / n, ...)
}
