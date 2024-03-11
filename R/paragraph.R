#'
#' @return
#' @author Michal Burda
#' @export
paragraph <- function(..., sep = "") {
    .must_be_character_scalar(sep)

    dots <- list(...)

    structure(dots,
              class = c("paragraph", "list"),
              sep = sep)
}
