#'
#' @return
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
