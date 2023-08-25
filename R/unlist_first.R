#' Flatten list by taking only its first element.
#'
#' Given a list `x`, `unlist_first` simplifies it to produce a vector
#' which contains all first values of each element of `x`.
#'
#' @param x the list to be simplified
#' @return a vector
#' @author Michal Burda
#' @export
unlist_first <- function(x) {
    .must_be_list(x)

    res <- lapply(x, function(v) v[[1]])

    unlist(res)
}
