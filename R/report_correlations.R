#'
#' @return
#' @author Michal Burda
#' @export
report_correlations <- function(data,
                                xvars,
                                yvars,
                                digits = 2,
                                p_adjust = p.adjust.methods,
                                ...) {
    p_adjust <- match.arg(p_adjust)

    grid <- expand_grid(xvars=xvars, yvars=yvars)
    result <- apply(grid, 1, function(row) {
        d <- data.frame(x=data[[row[1]]],
                        y=data[[row[2]]])
        d <- na.omit(d)
        res <- cor.test(d$x, d$y, ...)
        return(list(estimate=res$estimate,
                    p.value=res$p.value))
    })
    result <- lapply(result, as.data.frame)
    result <- do.call(rbind, result)
    result <- cbind(grid, as.data.frame(result))

    rownames(result) <- NULL
    colnames(result) <- c("x", "y", "estimate", "p")

    if (p_adjust != "none") {
        result[["p adj."]] <- format_pvalue(p.adjust(result$p, method = p_adjust))
    }

    result$estimate <- format_number(result$estimate, digits = digits)
    result$p <- format_pvalue(result$p)

    result
}
