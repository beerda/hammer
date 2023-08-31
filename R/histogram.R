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
        aes(y = x) +
        geom_histogram(bins = bins, colour = "black", fill = "gray") +
        geom_density(aes(x = binwidth * after_stat(.data$count)))

    if (nrow(d) > 0) {
        space <- 0.35
        built <- ggplot_build(g)
        xrange <- built$layout$panel_params[[1]]$x.range
        yrange <- built$layout$panel_params[[1]]$y.range
        xrange[1] <- xrange[1] * 2
        bcenter <- xrange[1] * ((1 - space) / 2 + space)
        g <- g +
            xlim(xrange[1], xrange[2]) +
            geom_errorbar(aes(x = bcenter,
                               ymin = bp$stats[1],
                               ymax = bp$stats[5]),
                           color = "black",
                           width = abs(xrange[1] * (1 - space)),
                           size = space) +
            geom_rect(aes(ymin = bp$stats[2],
                          ymax = bp$stats[4],
                          xmin = xrange[1] + space,
                          xmax = xrange[1] * space),
                      color="black",
                      fill="white") +
            geom_linerange(aes(y = bp$stats[3],
                               xmin = xrange[1],
                               xmax = xrange[1] * space))

        if (nrow(outs) > 0) {
            g <- g +
                geom_point(data = outs,
                           mapping = aes(y = .data$o, x = bcenter))
        }
        if (rugs) {
            g <- g +
                geom_rug(data = d,
                         mapping = aes(y = x), color = "gray20")
        }
        if (na) {
            nas <- sum(is.na(x))
            miny <- yrange[1]
            maxy <- yrange[2]
            diffy <- maxy - miny
            g <- g +
                geom_linerange(xmin = 0,
                               xmax = nas,
                               y = maxy + 0.005 * diffy,
                               size = 2) +
                scale_y_continuous(expand = c(0.1, 0))
        }
    }

    g + xlab("count") + ylab(name)
}
