% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Self_starters.R
\name{simulate.nls}
\alias{simulate.nls}
\title{Simulation method for non-linear models}
\usage{
\method{simulate}{nls}(object, nsim = 1, seed = NULL, ...)
}
\arguments{
\item{object}{A fitted model object}

\item{nsim}{The number of simulated response variable simulations required}

\item{seed}{An optional seed for the random number generator}

\item{...}{additional arguments used by other methods.}
}
\value{
A data frame with of size n x nsim, with columns the simulated response variables
}
\description{
This method function is based on stats:::simulate.lm and has simular functionality
}
\examples{
fm <- nls(Time ~ SSstormer(Viscosity, Weight, beta, theta), data = Stormer)
Times <- simulate(fm, 10, seed = 2021)

}
