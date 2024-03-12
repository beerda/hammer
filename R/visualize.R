.decorateValueForVisualization <- function(x) {
    if (is.factor(x) || is.logical(x)) {
        if (is.logical(x)) {
            x <- ifelse(x, 'T', 'F')
            x <- as.factor(x)
        }
        levels(x) <- c(levels(x), '(NA)')
        x[is.na(x)] <- '(NA)'
    }

    x
}


#'
#' @return
#' @author Michal Burda
#' @export
visualize <- function(data, var1, var2 = NULL, digits = 2, color = FALSE) {
    # TODO fix assertions
    #assert_that(is.data.frame(data))
    #assert_that(is.string(var1))
    #assert_that(is.null(var2) || is.string(var2))

    if (is.null(data[[var1]])) {
        stop(var1, ' does not exist in provided data frame')
    }
    if (!is.null(var2) && is.null(data[[var2]])) {
        stop(var2, ' does not exist in provided data frame')
    }

    val1 <- .decorateValueForVisualization(data[[var1]])
    g <- NULL

    if (is.null(var2)) {
        # univariate visualization
        if (is.numeric(val1) || is.double(val1) || is.integer(val1)) {
            g <- histogram(val1, name = var1)

        } else if (is.factor(val1) || is.character(val1) || is.logical(val1)) {
            g <- freqPlot(val1, name = var1)

        } else {
            g <- ggplot() +
                annotate('text',
                         x=mean(seq_along(cols)),
                         y=0,
                         label=paste0('class:\n', paste0(class(d$value), collapse=' '))) +
                scale_x_discrete(limits=names(cols)) +
                theme(axis.ticks.y=element_blank(),
                      axis.text.y=element_blank())
        }

    } else {
        # bivariate visualization
        val2 <- .decorateValueForVisualization(data[[var2]])
        firstNumeric <- FALSE
        secondNumeric <- FALSE

        if (is.numeric(val1) || is.double(val1) || is.integer(val1)) {
            firstNumeric <- TRUE

        } else if (is.numeric(val2) || is.double(val2) || is.integer(val2)) {
            tempval <- val1
            tempvar <- var1
            val1 <- val2
            var1 <- var2
            val2 <- tempval
            var2 <- tempvar
            firstNumeric <- TRUE
        }

        if (is.numeric(val2) || is.double(val2) || is.integer(val2)) {
            secondNumeric <- TRUE
        }

        var1q <- paste0('`', var1, '`')
        var2q <- paste0('`', var2, '`')

        if (firstNumeric && secondNumeric) {
            corr <- cor(val1, val2, use = 'pairwise')
            corr <- paste0('r = ', formatNumber(corr, digits = digits), '')
            g <- ggplot(data) +
                aes_string(x = var1q, y = var2q) +
                geom_point(na.rm = TRUE) +
                geom_smooth(method = 'lm', formula = y~x, fullrange = TRUE, na.rm = TRUE) +
                labs(subtitle = corr)
            #annotate('text', Inf, Inf, label = corr, color = 'blue', hjust = 1, vjust = 1)

        } else if (firstNumeric && !secondNumeric) {
            g <- ggplot(data)
            if (color) {
                g <- g + aes_string(x = var1q, y = var2q, fill = var2q)
            } else {
                g <- g + aes_string(x = var1q, y = var2q)
            }

            g <- g + geom_boxplot(na.rm = TRUE) +
                theme(legend.position='none')

        } else {
            #dd <- data.frame(x = val1, y = val2)
            #g <- ggplot(dd) +
                #geom_mosaic(aes(x = product(x, y), fill = x), offset = 0.02) +
                #theme(legend.position = 'none') +
                #labs(y = var1, x = var2)
#
            #if (!color) {
                #g <- g + scale_fill_grey()
            #}
            stop("not implemented")
        }
    }

    g
}
