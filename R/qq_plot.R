#' Plot the quantile-quantile plot
#'
#' @param x the numeric data to plot
#' @return the plot
#' @author Michal Burda
#' @export
qq_plot <- function(x) {
    .must_be_numeric_vector(x)

    d <- data.frame(x = scale(na.omit(x)))

    ggplot(d) +
        aes(sample=x) +
        stat_qq() +
        geom_abline(slope = 1)
}
