% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterators.R
\name{makeIwrapper}
\alias{makeIwrapper}
\alias{irunif}
\alias{irnorm}
\alias{irbinom}
\alias{irnbinom}
\alias{irpois}
\alias{isample}
\alias{irexp}
\title{Make Iterator for Random Numbers}
\usage{
makeIwrapper(FUN)

irunif(..., count)

irnorm(..., count)

irbinom(..., count)

irnbinom(..., count)

irpois(..., count)

isample(..., count)

irexp(..., count)
}
\arguments{
\item{FUN}{A character string nominating an R function to generate the random numbers}

\item{...}{extra arguments to be passed on to the generator function}

\item{count}{The number of times that the iterator will fire. If not specified, it will fire values forever.}
}
\value{
An iterator function
}
\description{
A funtion to make further iterators
}
\examples{
irexp <- makeIwrapper("rexp")
rexp_it <- irexp(n = 25)
rexp_it$nextElem()
}
