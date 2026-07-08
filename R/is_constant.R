#'
#' @return
#' @author Michal Burda
#' @export
is_constant <- function(x) {
    .must_be_atomic_vector(x)

    if (length(x) == 0) {
        return(TRUE)
    }

    length(unique(x)) == 1
}
