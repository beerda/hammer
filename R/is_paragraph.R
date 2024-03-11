#'
#' @return
#' @author Michal Burda
#' @export
is_paragraph <- function(x) {
    inherits(x, "paragraph") && is.list(x)
}
