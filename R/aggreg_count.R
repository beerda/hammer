#' Compute the count of the given level in the given vector
#'
#' The function returns the number of occurrences of `level` in vector `x`.
#'
#' @param x the vector to compute the occurences of `level` in. It must be an atomic vector.
#' @param level the level to be computed. It could be either integerish value or a string or `NA`.
#' The integerish value is treated as the index of level to be counted, character value selects
#' the level to be counted by its name, and `NA` selects missing values to be counted.
#' @return The number of occurences of `level` in `x`
#' @seealso [aggreg_percent()]
#' @author Michal Burda
#' @export
#' @examples
#' aggreg_count(c(FALSE, FALSE ,TRUE, TRUE, TRUE), level = 1)          # 3
#' aggreg_count(c(FALSE, FALSE, TRUE, TRUE, TRUE), level = "FALSE")    # 2
#' aggreg_count(c("b", "a", "a"), level = "a")                         # 2
#' aggreg_count(c(NA, "b", "a", "a"), level = NA)                      # 1
aggreg_count <- function(x, level = 1) {
    .must_be_atomic_vector(x)
    ..must_be_type(function(a) is_scalar_integerish(a) ||
                       is_scalar_character(a) ||
                       (length(a) == 1L && is.na(a)),
                   "integerish or character scalar")(level)

    if (is.logical(x)) {
        x <- factor(x, levels=c('TRUE', 'FALSE'))
    }
    tab <- table(x, useNA = "always")

    if (is_integerish(level)) {
        if (!isTRUE(level <= length(tab))) {
            cli_abort(c("If integerish, {.var level} must not be greater than the number of unique elements in {.var x}.",
                        "i" = "The number of unique elements in {.var x} is {length(tab)}.",
                        "x" = "{.var level} is {level}."))
        }
    } else if (is.character(level)) {
        if (!isTRUE(level %in% names(tab))) {
            cli_abort(c("If character, {.var level} must be a value contained in {.var x} after transforming to character.",
                        "x" = "{.var level} is \"{level}\"."))

        }
    } else {
        level <- length(tab)
    }

    as.vector(tab[level])
}
