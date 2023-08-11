#' Get the most frequent value
#'
#' @param x the vector within which to search the modus
#' @param na_rm whether to ignore `NA`
#' @returns The modus (mode), i.e., the most frequent value
#'
#' @author Michal Burda
#' @export
modus <- function(x,
                  na_rm = FALSE) {
    .must_be_atomic_vector(x)
    .must_be_flag(na_rm)

  if (na_rm) {
    x <- na.omit(x)
  }
  uniqv <- unique(x)

  uniqv[which.max(tabulate(match(x, uniqv)))]
}
