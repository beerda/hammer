.aggregator <- function(var,
                        subdata,
                        dots,
                        handlers) {
    if (var %in% colnames(subdata)) {
        x <- subdata[[var]]
        cl <- class(x)[1]
        if (is.function(handlers[[cl]])) {
            return(do.call(handlers[[cl]], c(list(x), dots)))
        }
    }

    return("")
}


.tester <- function(var,
                    data,
                    g,
                    dots,
                    handlers) {
    if (var %in% colnames(data)) {
        x <- data[[var]]
        cl <- class(x)[1]
        if (is.function(handlers[[cl]])) {
            return(do.call(handlers[[cl]], c(list(x, g), dots)))
        }
    }

    return(NA_real_)
}


#' Create baseline table from data frame
#'
#' Function works mainly for data frames grouped by [dplyr::group_by()]. In the
#' resulting data frame, each column corresponds to a group and each row
#' corresponds to a non-grouping column of the original data frame.
#'
#' @param .data the data frame to be processed
#' @param .n if `TRUE`, the first row of the result has to contain number of rows
#'      in groups
#' @param .all if `TRUE`, an additional `"all"` column is created containing
#'      statistics of the whole data frame
#' @param .test if `TRUE`, an additional `"p"` column is created with p-values
#'      of appropriate statistical tests of significance of differences in groups
#' @param .type the type of statistic to use for numeric data is used to set
#'      the default for `.numeric_aggreg` or `.numeric_test` argument if these
#'      are set to `NULL`. See [create_numeric_aggreg()] or [create_numeric_test()]
#'      respectively.
#' @param .labels a character vector of column labels (defaults to column names
#'      of `.data`)
#' @param .bullet a string used as a bullet in enumeration of factor levels
#' @param .max_width a maximum width of variable names or factor levels provided
#'      in the first column of the result (formatted by [format_string()])
#' @param .numeric_aggreg a custom function used to aggregate numeric values.
#'      If `NULL`, `create_numeric_aggreg(.type)` is used to obtain the default.
#' @param .categ_aggreg a custom function used to aggregate categorical values.
#'      If `NULL`, [aggreg_count_percent()] is used.
#' @param .numeric_test a custom function used to provide p-value of the test on
#'      numeric columns. If `NULL`, `create_numeric_test(.type, k)` is used to
#'      obtain the default function where `k` is the number of groups of `.data`.
#' @param .categ_test a custom function used to provide p-value of the test on
#'      categorical columns. If `NULL`, [test_fisher()] is used as a default.
#' @param ... further arguments passed to aggregating or testing functions.
#' @seealso [create_numeric_aggreg()], [create_numeric_test()]
#' @export
#' @author Michal Burda
baseline <- function(.data,
                     .n = TRUE,
                     .all = TRUE,
                     .test = n_groups(.data) > 1,
                     .type = c("robust", "parametric", "test"),
                     .labels = labels(.data)[[2]],
                     .bullet = " \u2022 ",
                     .max_width = 30L,
                     .numeric_aggreg = NULL,
                     .categ_aggreg = NULL,
                     .numeric_test = NULL,
                     .categ_test = NULL,
                     ...) {
    .type <- match.arg(.type)

    .must_be_data_frame(.data)
    .must_be_flag(.n)
    .must_be_flag(.all)
    .must_be_flag(.test)
    .must_be_character_scalar(.type)
    .must_be_character_vector(.labels)
    .must_be_character_scalar(.bullet)
    .must_be_integerish_scalar(.max_width)
    .must_be_greater_eq(.max_width, 1)
    .must_be_function(.numeric_aggreg, null = TRUE)
    .must_be_function(.categ_aggreg, null = TRUE)
    .must_be_function(.numeric_test, null = TRUE)
    .must_be_function(.categ_test, null = TRUE)

    if (length(.labels) != ncol(.data)) {
        cli_abort(c("The length of {.var .labels} must be equal to the number of columns of {.var .data}.",
                    "x" = "{.var data} has {ncol(data)} columns.",
                    "x" = "{.var .labels} has {length(.labels)} elements."))
    }

    groups <- NULL
    if (n_groups(.data) > 1) {
        groups <- group_rows(.data)
        names(groups) <- Reduce(function(...) paste(..., sep='/'),
                                group_keys(.data))
    } else {
        if (!isTRUE(.all)) {
            cli_abort(c("Can't create baseline table if data are not grouped and {.var {.all}} is FALSE."))
        }
    }

    orig_groups <- groups
    orig_data <- .data
    dots <- list(...)
    vars <- colnames(.data)
    mastervars <- vars
    labs <- format_string(.labels, max_width = .max_width)

    gi <- !(vars %in% colnames(group_keys(.data)))
    vars <- vars[gi]
    mastervars <- mastervars[gi]
    labs <- labs[gi]
    .data <- .data[, gi, drop = FALSE]

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
            levs <- format_string(levels(x), max_width = .max_width)
            labs <- append(labs, paste0(.bullet, levs), i)
            vars[i] <- NA_character_
            vars <- append(vars, names(newdata), i)
            mastervars <- append(mastervars, rep(NA_character_, nlevels(x)), i)
            .data <- cbind(.data, newdata)
        }
    }

    res <- data.frame(name=labs, stringsAsFactors=FALSE)

    if (is.null(.numeric_aggreg)) {
        .numeric_aggreg <- create_numeric_aggreg(.type)
    }
    if (is.null(.categ_aggreg)) {
        .categ_aggreg <- aggreg_count_percent
    }

    handlers <- list(numeric = .numeric_aggreg,
                     integer = .numeric_aggreg,
                     logical = .categ_aggreg,
                     factor = .categ_aggreg)

    for (g in names(groups)) {
        subdata <- .data[groups[[g]], ]
        res[[g]] <- vapply(vars,
                           .aggregator,
                           character(1),
                           subdata, dots, handlers,
                           USE.NAMES=FALSE)
    }

    if (.n) {
        counts <- sapply(groups, length)
        row <- res[NA, ][1, ]
        row[1, names(counts)] <- counts
        row[1, "name"] <- "N"
        res <- rbind(row, res)
        mastervars <- c(NA, mastervars)
    }

    if (.test) {
        if (is.null(.numeric_test)) {
            .numeric_test <- create_numeric_test(.type, k = length(orig_groups))
        }
        if (is.null(.categ_test)) {
            .categ_test <- test_fisher
        }

        handlers <- list(numeric = .numeric_test,
                         integer = .numeric_test,
                         logical = .categ_test,
                         factor = .categ_test)

        pvals <- vapply(as.character(colnames(orig_data)),
                        .tester,
                        numeric(1),
                        orig_data, factor(group_indices(orig_data)), dots, handlers)
        pvals <- rearrange(pvals, by = mastervars)
        res[['p-value']] <- format_pvalue(pvals, na = "")
    }

    rownames(res) <- NULL
    res
}
