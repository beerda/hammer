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


.create_test_based_aggreg <- function(test, thresh) {
    function(x, ...) {
        fit <- test(x)
        aggreg <- if (fit$p.value < thresh) aggreg_median_iqr else aggreg_mean_sd

        aggreg(x, ...)
    }
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
#' @param .numeric_stat selection of the statistic to compute for numeric
#' variables: `"robust"` (see [aggreg_median_iqr()]), `"parametric"` (see
#' [aggreg_mean_sd()]), or `"test"`, which performs `.normality_test` first
#' on each numeric variable to determine between `"robust"` and `"parametric"`
#' @param .normality_test a function that performs the normality test in case of
#' `.numeric_stat = "test"`. The function must return a list with the `"p.value"`
#' element containing the p-value of the rejection of normality.
#' @param .normality_thresh a p-value threshold below which will be the
#' `.normality_test` interpreted as rejection of normality.
#' @param .numeric_aggreg a custom function used to aggregate numeric values.
#' If non-NULL then `.numeric_stat` is ignored.
#' @param .categ_aggreg a custom function used to aggregate categorical values.
#' @export
#' @author Michal Burda
baseline <- function(.data,
                     .n = TRUE,
                     .all = TRUE,
                     .test = n_groups(.data) > 0,
                     .bullet = " \u2022 ",
                     .numeric_stat = c("robust", "parametric", "test"),
                     .normality_test = shapiro.test,
                     .normality_thresh = 0.05,
                     .numeric_aggreg = switch(.numeric_stat,
                                              robust = aggreg_median_iqr,
                                              parametric = aggreg_mean_sd,
                                              test = .create_test_based_aggreg(.normality_test, .normality_thresh)),
                     .categ_aggreg = aggreg_count_percent,
                     ...) {
    .numeric_stat <- match.arg(.numeric_stat)

    .must_be_data_frame(.data)
    .must_be_flag(.n)
    .must_be_flag(.all)
    .must_be_flag(.test)
    .must_be_character_scalar(.bullet)
    .must_be_character_scalar(.numeric_stat)
    .must_be_double_scalar(.normality_thresh)
    .must_be_function(.normality_test)
    .must_be_in_range(.normality_thresh, c(0, 0.1))
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
