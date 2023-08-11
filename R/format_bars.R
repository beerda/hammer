#' Convert the data into strings reporting relative frequencies in the form of
#' textual bar-chart
#'
#' `p` is the number of positive cases and `n` is the number of all cases. The result
#' is a character vector depicting bars created from the `symbol` (repeated at most
#' `width`-times if `p` = `n`).
#'
#' @param p an integerish vector of positive cases.
#' @param n an integerish vector of all cases.
#' @param width the maximum width of the textual bar-chart (i.e., the maximum number
#' of `symbol` repeats)
#' @param symbol the character string used to plot the bar
#' @param fill the character string used to fill the empty part of the chart
#'
#' @returns a character vector of bars created from `symbol` that depict the
#' relative frequency of `p`s in `n`s
#'
#' @author Michal Burda
#' @export
format_bars <- function(p,
                        n,
                        width = 20,
                        symbol = "I",
                        fill = "-") {
    .must_be_integerish_vector(p)
    .must_be_integerish_vector(n)
    .must_be_greater_eq(p, 0)
    .must_be_lower_eq(p, n)
    .must_be_integerish_scalar(width)
    .must_be_greater(width, 0)
    .must_be_character_scalar(symbol)

    rel <- width * p / n
    ticks <- round(rel)
    ticks <- ifelse(rel > 0 & ticks == 0, 1, ticks)
    ticks <- ifelse(rel < 1 & ticks == width, width - 1, ticks)
    res <- sapply(ticks, function(x) {
        if (is.na(x))
            return(NA)

        paste(c(rep(symbol, times = x),
                rep(fill, times = width - x)),
              collapse = "")
    })

    res
}
