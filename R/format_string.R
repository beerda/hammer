#' Shorten string and add `"..."`
#'
#' The elements of character vector `x` are trimmed to the maximum length
#' given by `max_width`. If shortening takes place, `ellipsis` is appended
#' to the end of the shortened string.
#'
#' @param x the character vector to be formatted
#' @param max_width the maximum width of strings in `x`
#' @param ellipsis string that will be appended to trimmed elements of `x`
#' @param ... unused arguments
#' @return a formatted character vector
#' @author Michal Burda
#' @export
format_string <- function(x,
                          max_width = Inf,
                          ellipsis = "...",
                          ...) {
    .must_be_character_vector(x)
    .must_be_integerish_scalar(max_width)

    i <- nchar(x) > max_width
    x[i] <- strtrim(x[i], width = max_width)
    x[i] <- paste0(x[i], ellipsis)

    x
}
