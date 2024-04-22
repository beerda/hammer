#' Plot the frequency bar chart with absolute and relative count in text over each
#' bar.
#'
#' @param x The factor data to plot
#' @param name the name of the data axis
#' @export
#' @author Michal Burda
plot_frequency <- function(x,
                           weight = NULL,
                           name = deparse(substitute(x))) {
    .must_be_factor(x)
    .must_be_numeric_vector(weight, null = TRUE)
    .must_be_character_scalar(name)

    if (!is.null(weight)) {
        .must_have_equal_lengths(x, weight)
    }

    if (any(is.na(x))) {
        levels(x) <- c(levels(x), '(NA)')
        x[is.na(x)] <- '(NA)'
    }

    d <- data.frame(x = x)
    if (!is.null(weight)) {
        d$weight <- weight
    }

    lim <- max(table(d$x))

    g <- ggplot(d)

    if (is.null(weight)) {
        g <- g + aes(y = x,
                     x = after_stat(.data$count))
    } else {
        g <- g + aes(weight = weight,
                     y = x,
                     x = after_stat(.data$count))
    }

    g <- g + geom_bar(colour = "black", fill = "gray")
    if (is.null(weight)) {
        g <- g + geom_text(aes(label = after_stat(format_count_percent(.data$count, sum(.data$count)))),
                           hjust =  -0.2,
                           stat = 'count',
                           color = "black",
                           size = 3)
    } else {
        g <- g + geom_text(aes(label = after_stat(format_percent(.data$count / sum(.data$count)))),
                           hjust =  -0.2,
                           stat = 'count',
                           color = "black",
                           size = 3)
    }

    g <- g +
        #scale_x_continuous(expand = c(0, 0),
                           #limits = c(NA, lim * 1.15)) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
        scale_y_discrete(drop = FALSE) +
        ylab(name)

    g
}
