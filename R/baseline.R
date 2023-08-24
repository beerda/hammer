.aggregator <- function(var,
                        subdata,
                        dots,
                        handlers) {
    if (var %in% colnames(subdata)) {
        x <- subdata[[var]]
        cl <- class(x)
        if (is.function(handlers[[cl]])) {
            return(do.call(handlers[[cl]], c(list(x), dots)))
        }
    }
    return('')
}


#' Create baseline table from data frame
#'
#' Function works mainly for data frames grouped by [dplyr::group_by()]. In the
#' resulting data frame, each column corresponds to a group and each row
#' corresponds to a non-grouping column of the original data frame.
#'
#' @param .data the data frame to be processed
#' @param .n if `TRUE`, the first row of the result has to contain number of rows
#' in groups
#' @param .all if `TRUE`, an additional `"all"` column is created containing
#' statistics of the whole data frame
#' @param .test if `TRUE`, an additional `"p"` column is created with p-values
#' of appropriate statistical tests of significance of differences in groups
#' @param .bullet a string used as a bullet in enumeration of factor levels
#' @param .numeric_aggreg a custom function used to aggregate numeric values.
#' @param .categ_aggreg a custom function used to aggregate categorical values.
#' @seealso [create_numeric_aggreg()]
#' @export
#' @author Michal Burda
baseline <- function(.data,
                     .n = TRUE,
                     .all = TRUE,
                     .test = n_groups(.data) > 0,
                     .bullet = " \u2022 ",
                     .numeric_aggreg = create_numeric_aggreg("robust"),
                     .categ_aggreg = aggreg_count_percent,
                     ...) {
    .must_be_data_frame(.data)
    .must_be_flag(.n)
    .must_be_flag(.all)
    .must_be_flag(.test)
    .must_be_character_scalar(.bullet)
    .must_be_function(.numeric_aggreg)
    .must_be_function(.categ_aggreg)

    groups <- NULL
    if (n_groups(.data) > 0) {
        groups <- group_rows(.data)
        names(groups) <- Reduce(function(...) paste(..., sep='/'),
                                group_keys(.data))
    } else {
        if (!isTRUE(.all)) {
            cli_abort(c("Can't create baseline table if data are not grouped and {.var {.all}} is FALSE."))
        }
    }

    origGroups <- groups
    origData <- .data
    dots <- list(...)
    vars <- colnames(.data)
    mastervars <- vars
    labs <- labels(.data)[[2]]
    labs[is.na(labs)] <- colnames(.data)[is.na(labs)]

    gi <- !(vars %in% colnames(group_keys(.data)))
    vars <- vars[gi]
    mastervars <- mastervars[gi]
    labs <- labs[gi]
    .data <- .data[, gi]

    if (.all) {
        groups <- c(groups, list_of(all = seq_len(nrow(.data))))
    }

    i <- 0
    while (i < ncol(.data)) {
        i <- i + 1
        l <- labs[i]
        v <- vars[i]
        x <- .data[[v]]
        if (is.factor(x)) {
            newdata <- list()
            for (lev in levels(x)) {
                newdata[[paste0('.', v, '=', lev)]] <- x == lev
            }
            labs[i] <- paste0(l, ':')
            labs <- append(labs, paste0(.bullet, levels(x)), i)
            vars[i] <- NA_character_
            vars <- append(vars, names(newdata), i)
            mastervars <- append(mastervars, rep(NA_character_, nlevels(x)), i)
            .data <- cbind(.data, newdata)
        }
    }

    res <- data.frame(name=labs, stringsAsFactors=FALSE)

    handlers <- list(numeric=.numeric_aggreg,
                     integer=.numeric_aggreg,
                     logical=.categ_aggreg,
                     factor=.categ_aggreg)

    for (g in names(groups)) {
        subdata <- .data[groups[[g]], ]
        res[[g]] <- vapply(vars, .aggregator, character(1),
                           subdata, dots, handlers,
                           USE.NAMES=FALSE)
    }

    if (.n) {
        counts <- sapply(groups, length)
        row <- res[NA, ][1, ]
        row[1, names(counts)] <- counts
        row[1, 'name'] <- 'N'
        res <- rbind(row, res)
        mastervars <- c(NA, mastervars)
    }

#    if (.test) {
#        if (length(origGroups) != 2) {
#            stop('groups of length 2 supported only (two-sample tests)')
#        }
#        pvals <- vapply(as.character(colnames(origData)), .test, numeric(1),
#                        origData, origGroups, dots)
#        pvals <- rearrange(pvals, by=mastervars)
#        res[['p-value']] <- formatPvalue(pvals, na='')
#    }

    rownames(res) <- NULL
    res
}
