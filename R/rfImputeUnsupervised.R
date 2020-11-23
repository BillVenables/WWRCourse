#' @import randomForest
NULL

#' Missing value imputation
#'
#' An unsupervised missing value imputation method
#' using \code{randomForest} methods.  Somewhat experimental
#'
#' @param object A one-sided formula, or a data frame containing missing values in some of the variables.  Missing values in \code{Date} or \code{POSIXt} objects are not accommodated
#' @param ... additional arguments passed on to \code{randomForest}
#' @param data A data frame used when \code{x} is a \code{formula}
#'
#' @return A data frame like \code{object} with missing values imputed, if possible
#' @export
rfImputeUnsupervised <- function(object, ...) {
  # stopifnot(requireData(randomForest))
  UseMethod("rfImputeUnsupervised")
}

.plug <- function(x, w)
  if(is.numeric(x)) {
    weighted.mean(x, w)
  } else {
    tab <- na.omit(tapply(w, x, sum))
    names(tab)[which.max(tab)]
  }


#' @rdname rfImputeUnsupervised
#' @export
rfImputeUnsupervised.default <- function(object, iter = 5, ...) {
  holes <- which(is.na(object), arr.ind = TRUE)
  if(nrow(holes) == 0)
    return(object)
  omits <- split(holes[, "row"], holes[, "col"])

  x <- .naRoughFix(object)

  for(it in seq(iter)) {
    prox <- randomForest::randomForest(x, proximity = TRUE, ...)$proximity
    for(nch in names(omits)) {
      c <- as.numeric(nch)
      om <- omits[[nch]]
      for(r in om)
        x[r, c] <- .plug(object[-om, c], prox[-om, r])
    }
  }
  x
}

#' @rdname rfImputeUnsupervised
#' @export
rfImputeUnsupervised.formula <- function(object,
                                         data = environment(object), ...) {
  data <- as.data.frame(data)
  vars <- attr(terms(object, data = data), "term.labels")

  rfImputeUnsupervised.default(data[, vars], ...)
}

.naRoughFix <- function(object, ...)
  UseMethod(".naRoughFix")

.naRoughFix.data.frame <- local({
  fixup <- function(x) {
    if(is.numeric(x)) {
      x[is.na(x)] <- median(x, na.rm = TRUE)
    } else {
      freq <- table(x)
      x[is.na(x)] <- names(freq)[which.max(freq)]
    }
    x
  }

  function(object, ...) {
    holes <- which(is.na(object), arr.ind =  TRUE)
    if(nrow(holes) == 0)
      return(object)
    colms <- unique(holes[, "col"])
    OK <- sapply(object[, colms, drop = FALSE],
                 function(x) is.numeric(x) || is.factor(x) ||
                   is.character(x))
    if(any(!OK))
      stop("cannot handle missing values other than factor or numeric")
    object[, colms] <- lapply(object[, colms, drop = FALSE], fixup)
    object
  }
})

.naRoughFix.default <- function(object, ...) {
  if(!is.atomic(object) || !anyNA(object))
    return(object)
  d <- dim(object)
  if(length(d) > 2)
    stop("cannot deal with dimensions greater than 2.")
  if(!is.numeric(object))
    stop("cannot deal with non-numeric objects.")
  if(length(d) == 2) {
    holes <- which(is.na(object), arr.ind = TRUE)
    colms <- unique(holes[, "col"])
    medns <- sapply(object[, colms], median, na.rm = TRUE)
    object[holes] <- medns[match(holes[, "col"], colms)]
  } else {
    object[is.na(object)] <- median(object, na.rm = TRUE)
  }
  object
}
