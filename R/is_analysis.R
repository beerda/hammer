#'
#' @return
#' @author Michal Burda
#' @export
is_analysis <- function(x) {
    inherits(x, "analysis") && is.list(x)
}
