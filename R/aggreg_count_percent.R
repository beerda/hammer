#' Compute absolute and relative frequency of the given level in the given vector.
#'
#' The function returns the absolute and relative number (percentage) of occurences
#' of `level` in column `var` in data frame `data`.
#'
#' @param data the data frame
#' @param var the name of the column to compute the occurences of `level` in. It must be an atomic vector.
#' @param level the level to be computed. It could be either integerish value or a string or `NA`.
#' The integerish value is treated as the index of level to be counted, character value selects
#' the level to be counted by its name, and `NA` selects missing values to be counted.
#' @param use_na if `FALSE` then `NA` values are omitted prior computing the frequency.
#' @param ... further arguments passed to [format_count_percent()]
#' @return The absolute and relative number of occurences of `level` in `x`.
#' @seealso [format_count_percent()], [aggreg_count()], [aggreg_percent()]
#' @author Michal Burda
#' @export
aggreg_count_percent <- function(data,
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

    format_count_percent(p = p, n = n, ...)
}
