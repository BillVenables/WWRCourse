#' @import fractional
#' @export fractional
#' @export numerical
#' @export unfractional
NULL

#' @import english
#' @export english
#' @export as.english
#' @export indefinite
#' @export Indefinite
#' @export words
#' @export Words
#' @export ordinal
NULL


#' @import lazyData
#' @export requireData
NULL

#' @import SOAR
#' @export Attach
#' @export Store
#' @export Objects
#' @export Ls
#' @export Remove
#' @export Search
NULL


#' Turn off tible production
#'
#' Convenience function to turn a \code{"tibble"} back into an
#' honest data frame
#'
#' @param tibble Any object that can be coerced to a data frame, including tibbles
#'
#' @return An honest data frame
#' @export
untibble <- function(tibble) {
  data.frame(unclass(tibble),
             check.names = FALSE,
             stringsAsFactors = FALSE)
}

#' Attach multiple packages
#'
#' Attach a series of packages to the search path, optionally
#' installing them if needed
#'
#' @param ... package names, unquoted
#' @oaran quietly logical: suppress package startup messages?
#'
#' @return a list of packages attached to the search path
#' @export
#'
#' @examples
#' s0 <- search()
#' use_packages(mgcv, lme4, splines, quietly = FALSE)
#' setdiff(search(), s0)
use_packages <- function (..., quietly = FALSE) {
  pkgs <- lapply(sys.call(), deparse)[-1]
  pkgs$quietly <- NULL
  if(quietly) {
    suppressPackageStartupMessages({
      for (pkg in pkgs) {
        if (!require(pkg, character.only = TRUE)) {
          install.packages(pkg)
          library(pkg, character.only = TRUE)
        }
      }
    })
  } else {
    for (pkg in pkgs) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
      }

    }
  }
  invisible(pkgs)
}



#' Extended subset function
#'
#' Like \code{\link[base]{subset}} but allowing any form of indexing for
#' the \code{subset} argument, not just logical.
#'
#' @param x,subset,select,drop As for \code{\link[base]{subset}}
#' @param ... additional arguments passed on
#'
#' @return A data frame, as for \code{\link[base]{subset}}
#' @export
#'
#' @examples
#' subset(iris, 1:10)
subset.data.frame <- function (x, subset, select, drop = FALSE, ...) {
  if (missing(subset))
      r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    ## the following 4 lines are changed
    if (is.logical(r))
        r <- r & !is.na(r)
    else
        r <- r[!is.na(r)]
  }
  if (missing(select))
      vars <- TRUE
  else {
    nl <- as.list(1:ncol(x))
    names(nl) <- names(x)
    vars <- eval(substitute(select), nl, parent.frame())
  }
  x[r, vars, drop = drop]
}


#' Zero fill strings
#'
#' Pads out strings with leading zeros to a uniform character size.
#' \code{fill0} does the same job using a different algorighm
#'
#' @param x A vector which will be coerced to charater
#'
#' @return A zero-padded character string vector
#' @export
#'
#' @examples
#' zfill(1:10)
#' fill0(cs(Mary, had, a, little, lamb))
zfill <- function(x) {
  m <- max(n <- nchar(x <- as.character(x)))
  paste0(strrep(0, m-n), x)
}

#' @rdname zfill
#' @export
fill0 <- function(x) {
  gsub(" ", "0", format(x, justify = "right"))
}

#' Unquoted concatenation
#'
#' Convenience function for inputing strings without quote signs
#' (unless the strings themselves are not standard R names).
#'
#' Based on an original function \code{Cs} in the \code{Hmisc} package.
#'
#' @param ... strings, may be unquoted if standard R name format
#'
#' @return A string vector
#' @export
#'
#' @examples
#' (rhyme <- cs(Mary, had, a, little, lamb, "4 Christmas", 1:4))
cs <- function(...)  {## Hmisc package, (courtesy Frank Harrell)
  as.character(sys.call())[-1]
}

#' Extended union and intersect functions
#'
#' Like \code{\link[base]{intersect}} and \code{\link[base]{union}} but
#' allowing more than two arguments.
#'
#' @param ... One or more vectors
#'
#' @return A vector with the result of the operation
#' @export
#'
#' @examples
#' Union(letters[1:3], 1:3, LETTERS[1:3], NULL, letters[1:5])
#' Intersect(letters, letters[1:12], letters[7:15])
Intersect <- function(...) {
  args <- list(...)
  if(length(args) <= 2) {
    do.call(intersect, args)
  } else {
    Reduce(intersect, args)
  }
}

#' @rdname Intersect
#' @export
Union <- function(...) {
  args <- list(...)
  if(length(args) <= 2) {
    do.call(union, args)
  } else {
    Reduce(union, args)
  }
}

#' @import xtable
NULL

#' Formatting function for tables in Rnw files
#'
#' Convenience function for drafting the course materia.s
#'
#' @param x,align as for \code{\link[xtable]{xtable}}
#' @param ... additional arguments passed on to methods
#' @param floating,hline.after,add.to.row,include.rownames as for \code{\link[xtable]{print.xtable}}
#'
#' @return A data frame, giving a suitably formatted LaTeX table when printed
#' @export
#'
#' @examples
#' booktabs(head(iris))
booktabs <- function(x, ...) {
  UseMethod("booktabs")
}

#' @rdname booktabs
#' @export
booktabs.default <- function(x, align = c("@{}l",
                                rep("r", ncol(x)-1), "r@{}"), ...) {
  x <- xtable(x, align = align, ...)
  class(x) <- c("booktabs", class(x))
  x
}

#' @rdname booktabs
#' @export
booktabs.noquote <- function(x, align = c("@{}l", rep("r", ncol(x)-1), "r@{}"), ...) {
  x <- xtable(unclass(x), align = align, ...)
  class(x) <- c("booktabs", class(x))
  x
}


#' @rdname booktabs
#' @export
booktabs.xtable <- function(x, ...) {
  a <- attr(x, "align")
  if(substring(a[1], 1, 3) != "@{}") {
    a[1] <- paste0("@{}", a[1])
    a[length(a)] <- paste0(a[length(a)], "@{}")
    attr(x, "align") <- a
  }
  class(x) <- c("booktabs", class(x))
  x
}

#' @rdname booktabs
#' @export
print.booktabs <- function(x,
                           floating = FALSE,
                           hline.after = NULL,
                           add.to.row = list(pos = list(-1, 0, nrow(x)),
                                             command = c('\\toprule ',
                                                         '\\midrule ',
                                                         '\\bottomrule ')),
                           include.rownames = getOption("xtable.include.rownames", TRUE),
                           ...) {
  if(!include.rownames) {
    attr(x, "align")[2] <- paste0("@{}", attr(x, "align")[2])
  }
  xtable::print.xtable(x,
                       floating = floating,
                       hline.after = hline.after,
                       add.to.row = add.to.row,
                       include.rownames = include.rownames,
                       ...)
}

