% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Self_starters.R
\name{SSstormer}
\alias{SSstormer}
\title{Stormer Viscometer Calibration Model}
\usage{
SSstormer(v, w, b, c)
}
\arguments{
\item{v, w}{numeric vectors, known viscosities and actuating weights respectively}

\item{b, c}{place-holders for the parameter estimates}
}
\value{
An evaluated model function, with a derivative matrix attribute
}
\description{
A self-starting model fitting function for the Stormer Viscometer calibration example
The model fitted is ~ b*v/(w - c)
}
\examples{
fm <- nls(Time ~ SSstormer(Viscosity, Weight, beta, theta), Stormer)
coef(fm)
}
