#' @import iterators
NULL


#' Generate all subsets
#'
#' An iterator to generate all subset of a collection
#'
#' @param n The size of the set
#' @param ... currently ignred
#' @param chunks  number of chunks to be supplied by the iterator
#' @param chunkSize size of a chunk (an alternative to \code{chunks})
#'
#' @return An iterator function for the purpose
#' @export
#' @examples
#' get_subs <- isubset(5, chunks = 4)
#' get_subs$nextElem()
#' get_subs$nextElem()
#' get_subs$nextElem()
#' get_subs$nextElem()
#' get_subs$nextElem()
isubset <- function(n, ..., chunks, chunkSize) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0)
    stop("n must be a strictly positive numeric value")
  if (length(list(...)) > 0)
    stop("chunks or chunkSize may only be specified as a named argument")
  if ((missing(chunkSize) && missing(chunks)) ||
      (!missing(chunkSize) && !missing(chunks)))
    stop("either chunks or chunkSize must be specified, but not both")
  N <- 2^n
  if (missing(chunkSize)) {
    if (!is.numeric(chunks) || length(chunks) != 1 || chunks < 1)
      stop("chunks must be a numeric value >= 1")
    chunkSize <- ceiling(N/chunks)
  }
  n2 <- 0
  nextEl <- function() {
    if(n2 >= N) stop("StopIteration", call. = FALSE)
    n1 <- n2 + 1
    n2 <<- min(n2 + chunkSize, N)
    lapply(n1:n2, function(x) which(as.logical(intToBits(x-1))))
  }
  object <- list(nextElem = nextEl)
  class(object) <- c("abstractiter", "iter")
  object
}

#
# Copyright (c) 2008-2010 Revolution Analytics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This function makes iterator makers.  The resulting iterator makers all take
# an optional "count" argument which specifies the number of times the
# resulting iterator should fire.  The iterators are wrappers around functions
# that return different values each time they are called.  All this is done to
# avoid cutting and pasting the same code repeatedly.  We could make this
# function available to the user, but I'm not sure if we will immediately.


#' Make Iterator for Random Numbers
#'
#' A funtion to make further iterators
#'
#' @param FUN A character string nominating an R function to generate the random numbers
#' @param ... extra arguments to be passed on to the generator function
#' @param count The number of times that the iterator will fire. If not specified, it will fire values forever.
#'
#' @return  An iterator function
#' @export
#'
#' @examples
#' irexp <- makeIwrapper("rexp")
#' rexp_it <- irexp(n = 25)
#' rexp_it$nextElem()
makeIwrapper <- function(FUN) {
  function(..., count) {
    if (!missing(count) && (!is.numeric(count) || length(count) != 1))
      stop('count must be a numeric value')

    # construct the call object to put into the nextElem function
    m <- as.call(c(as.name(FUN), list(...)))

    # construct the body of the nextElem function
    fbody <- if (missing(count)) {
      m
    } else {
      substitute({
        if (count > 0) {
          count <<- count - 1L
          REPLACETHIS
        } else {
          stop('StopIteration', call.=FALSE)
        }
      }, list(REPLACETHIS=m))
    }

    # create the nextElem function using fbody
    nextEl <- function() NULL
    body(nextEl) <- fbody

    # create and return the iterator object
    it <- list(nextElem=nextEl)
    class(it) <- c('abstractiter', 'iter')
    it
  }
}

# define some iterator makers using makeIwrapper

#' @rdname makeIwrapper
#' @export
irunif <- makeIwrapper('runif')

#' @rdname makeIwrapper
#' @export
irnorm <- makeIwrapper('rnorm')

#' @rdname makeIwrapper
#' @export
irbinom <- makeIwrapper('rbinom')

#' @rdname makeIwrapper
#' @export
irnbinom <- makeIwrapper('rnbinom')

#' @rdname makeIwrapper
#' @export
irpois <- makeIwrapper('rpois')

#' @rdname makeIwrapper
#' @export
isample <- makeIwrapper('sample')

#' @rdname makeIwrapper
#' @export
irexp <- makeIwrapper("rexp")
