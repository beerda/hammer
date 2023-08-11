#' Output the objects with default separator being empty string.
#'
#' @param sep The separator
#' @param ... the objects to output
#'
#' @seealso [cat()]
#' @export
#' @author Michal Burda
cat0 <- function(...,
                 sep = "") {
  cat(sep = sep, ...)
}
