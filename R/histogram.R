#' Plot a nice histogram
#'
#' @param x numeric vector
#' @param name the name to put on the X-axis
#' @param rugs whether to draw rugs
#' @param na whether to draw the NA bar
#' @export
#' @author Michal Burda
histogram <- function(x,
                      name = deparse(substitute(x)),
                      rugs = length(x) <= 1000,
                      na = any(is.na(x))) {
    .must_be_numeric_vector(x)
    .must_be_character_scalar(name)
    .must_be_flag(rugs)
    .must_be_flag(na)

    d <- data.frame(x = na.omit(x))
    bp <- boxplot(x, plot = FALSE)
    outs <- data.frame(o = bp$out)
    bins <- nclass.Sturges(x)
    binwidth <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / bins
    g <- ggplot(d) +
        aes(x = x) +
        geom_histogram(bins = bins, colour = "black", fill = "white") +
        geom_density(aes(y = binwidth * after_stat(.data$count)))

    if (nrow(d) > 0) {
        space <- 0.35
        built <- ggplot_build(g)
        xrange <- built$layout$panel_params[[1]]$x.range
        yrange <- built$layout$panel_params[[1]]$y.range
        yrange[1] <- yrange[1] * 2
        bcenter <- yrange[1] * ((1 - space) / 2 + space)
        g <- g +
            ylim(yrange[1], yrange[2]) +
            geom_errorbarh(aes(y = bcenter,
                               xmin = bp$stats[1],
                               xmax = bp$stats[5]),
                           color = "black",
                           height = abs(yrange[1] * (1 - space)),
                           size = space) +
            geom_rect(aes(xmin = bp$stats[2],
                          xmax = bp$stats[4],
                          ymin = yrange[1] + space,
                          ymax = yrange[1] * space),
                      color="black",
                      fill="grey70") +
            geom_linerange(aes(x = bp$stats[3],
                               ymin = yrange[1],
                               ymax = yrange[1] * space))

        if (nrow(outs) > 0) {
            g <- g +
                geom_point(data = outs,
                           mapping = aes(x = .data$o, y = bcenter))
        }
        if (rugs) {
            g <- g +
                geom_rug(data = d,
                         mapping = aes(x = x), color = "gray20")
        }
        if (na) {
            nas <- sum(is.na(x))
            minx <- min(x, na.rm = TRUE)
            maxx <- max(x, na.rm = TRUE)
            minx <- xrange[1]
            maxx <- xrange[2]
            diffx <- maxx - minx
            g <- g +
                geom_linerange(ymin = 0,
                               ymax = nas,
                               x = maxx + 0.005 * diffx,
                               size = 2) +
                scale_x_continuous(expand = c(0.1, 0))
        }
    }

    g + xlab(name) + ylab("count")
}
