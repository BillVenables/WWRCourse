% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_same.R
\name{all_same}
\alias{all_same}
\title{Compare multiple objects}
\usage{
all_same(x, ...)
}
\arguments{
\item{x}{an object}

\item{...}{additional objects to be compared to \code{x}}
}
\value{
if all objects are equal to \code{x} as defined by
        \code{all.equal(x, object)} yielding \code{TRUE} then
        the logical value \code{TRUE}.  If not, then a
        logical vector indicating which are, or are not,
        equal to \code{x} in this sense
}
\description{
Simple function to allow comparisons between multiple objects
}
\examples{
all_same(letters, LETTERS, sample(letters), 26)
all_same(letters, tolower(LETTERS), sort(sample(letters)))
}
