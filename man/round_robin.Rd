% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roundRobin.R
\name{round_robin}
\alias{round_robin}
\alias{print.round_robin}
\alias{summary.round_robin}
\title{Generate a round robin tournament schedule}
\usage{
round_robin(teams, alphabetical = TRUE, reorder = FALSE)

\method{print}{round_robin}(x, ...)

\method{summary}{round_robin}(object, type = c("venue", "travel"), ...)
}
\arguments{
\item{teams}{Either an interger specifing the number of teams,
or a character string vector giving their names}

\item{alphabetical}{logical: should the teams be alphabetically
ordered, if necessary?}

\item{reorder}{logical: within each round should the games be
listed in alphabetical order of the "Home" team?}
}
\value{
A 3-dimensional (n/2 x 2 x (n-1)) array giving the
       entire tournament
}
\description{
Given a list of n team names, generate a program of n-1 Rounds
where each team plays each other team precisely once.  If n is
odd the teams are augmented by a dummy team <Bye>.
}
\examples{
(Season2019 <- round_robin(NRL))
summary(Season2019)
summary(Season2019, "travel")
}
