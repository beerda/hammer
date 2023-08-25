#' Plot the frequency bar chart with absolute and relative count in text over each
#' bar.
#'
#' @param x The factor data to plot
#' @param name the name of the data axis
#' @export
#' @author Michal Burda
freq_plot <- function(x,
                      name = deparse(substitute(x))) {
    .must_be_factor(x)
    .must_be_character_scalar(name)

  levels(x) <- c(levels(x), '(NA)')
  x[is.na(x)] <- '(NA)'

  d <- data.frame(x = x)
  lim <- max(table(d$x))

  g <- ggplot(d) +
      aes(y = x,
          x = after_stat(.data$count)) +
      geom_bar() +
      geom_text(aes(label = after_stat(paste0(.data$count,
                                              ' (',
                                              round(100 * .data$count / sum(.data$count)),
                                              ' %)'))),
                hjust =  -0.2,
                stat = 'count',
                color = "black",
                size = 3) +
      scale_x_continuous(expand = c(0, 0),
                         limits = c(NA, lim * 1.15)) +
      ylab(name)

  g
}
