#' @export
rearrange <- function(value,
                      by) {
    .must_be_atomic_vector(value)
    .must_be_character_vector(by)

    res <- rep(NA, length(by))
    for (n in names(value)) {
        i <- which(by == n)
        res[i] <- value[n]
    }

    res
}
