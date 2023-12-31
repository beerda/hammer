% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_t.R
\name{test_t}
\alias{test_t}
\title{Return p-value of the two-sample t test}
\usage{
test_t(x, g, ...)
}
\arguments{
\item{x}{numeric values}

\item{g}{grouping factor (must contain 2 levels)}

\item{...}{further parameters passed to the underlying \code{\link[=t.test]{t.test()}} function}
}
\value{
The p-value of the test
}
\description{
This function is a wrapper over the \code{\link[=t.test]{t.test()}} function.
It performs the two-sample t test and returns the resulting p-value.
}
\author{
Michal Burda
}
