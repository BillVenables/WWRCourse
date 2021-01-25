#' Compare multiple objects
#'
#' Simple function to allow comparisons between multiple objects
#'
#' @param x an object
#' @param ... additional objects to be compared to \code{x}
#'
#' @return if all objects are equal to \code{x} as defined by
#'         \code{all.equal(x, object)} yielding \code{TRUE} then
#'         the logical value \code{TRUE}.  If not, then a
#'         logical vector indicating which are, or are not,
#'         equal to \code{x} in this sense
#' @export
#'
#' @examples
#' all_same(letters, LETTERS, sample(letters), 26)
#' all_same(letters, tolower(LETTERS), sort(sample(letters)))
all_same <- function(x, ...) {
  same <- sapply(list(...), function(dot) isTRUE(all.equal(x, dot)))
  asame <- all(same)
  if(asame) {
    asame
  } else {
    structure(c(TRUE, same),
              names = sapply(match.call(), deparse)[-1])
  }
}
