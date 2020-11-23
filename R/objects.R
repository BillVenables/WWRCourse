## Miscellaneous bits and pieces of general usefulness
##

#' Construct basis vectors for Harmonic terms in a model
#'
#' Convenience function for specifying harmonic terms in a model
#' The two functions \code{Annual} and \code{Hyear} do the same job
#' but are retained for compatibility purposes.
#'
#' @param day,x time variable, assumed to be in elapsed days from a fixed origin
#' @param dates time variable, as a Date object or can be coerced to Date
#' @param theta angle expressed in radians
#' @param k the number of sine and cosine terms
#'
#' @return A matrix of harmonc terms with \code{length(days)} rows and \code{2*k} columns
#' @export
#'
#' @examples
#' head(Annual(0:364))
Annual <- function (day, k = 4) {  ## day of the year, starting from 0
  theta <- Arg(complex(argument = 2*base::pi*day/365.25))
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(day), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}

is.ok <- Negate(is.na)

#' @rdname Annual
#' @export
Seasonal <- function(dates, k = 4) {
  dates <- as.Date(dates)
  stopifnot(all(is.ok(dates)), inherits(dates, "Date"))
  theta <- 2*base::pi*(as.numeric(difftime(dates, as.Date("2000-01-01"), units = "days")))/365.25
  theta <- Arg(complex(argument = theta))
  Harm(theta, k = k)
}

#' @rdname Annual
#' @export
Harm <- function (theta, k = 4) {
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(theta), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}

#' @rdname Annual
#' @export
Hyear <- function(x, k = 4) {
  theta <- Arg(complex(argument = 2*base::pi*x/365.25))
  Harm(theta, k = k)
}


#' Column product matrix
#'
#' Forms all pairwise products of the columns of X and Y suitable for interaction
#' terms in linear modeld and their allies
#'
#' @param X,Y  Numeric matrices with the same number of rows
#'
#' @return A matrix of products with \code{nrow(X)} rows and \code{ncol(X)*ncol(Y)} columns
#' @export
`%star%` <- function(X, Y) { ## all column-products
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  stopifnot(is.numeric(X), is.numeric(Y),
            nrow(X) == nrow(Y))
  XY <- matrix(NA, nrow(X), ncol(X)*ncol(Y))
  k2 <- 0
  for(j in 1:ncol(X)) {
    k1 <- k2+1
    k2 <- k2 + ncol(Y)
    XY[, k1:k2] <- X[, j] * Y
  }
  XY
}

## #' @import splines
## #' @export ns
## #' @export bs
## #' NULL

#' Special interaction function
#'
#' Convenience function, mainly as a demonstration.
#' Used in WWR examples
#'
#' @param day Day of year
#' @param sea Distance out to sea
#' @param k  integer: two components
#'
#' @return A matix for use in model specification
#' @export
twoWay <- function(day, sea, k = c(3,2)) {
  Hyear(day, k[1]) %star% splines::ns(sea, k[2])
}

#' Extract the most typical value from a vector
#'
#' Given a factor, character string vector, numeric vector or logical vector
#' extract a single "typical" value.  Either the median or the most frequently
#' occurring value
#'
#' @param x A suitable vector
#' @param ... currently ignored
#'
#' @return The "most typical" value as described
#' @export
#'
#' @examples
#' data.frame(lapply(quine, mostFreq))
mostFreq <- function(x, ...) {
  UseMethod("mostFreq")
}

#' @rdname mostFreq
#' @export
mostFreq.character <- function(x, ...) {
  tx <- table(x)
  names(tx)[which.max(tx)]
}

#' @rdname mostFreq
#' @export
mostFreq.factor <- function(x, ...) {
  mostFreq.character(as.character(x))
}


#' @rdname mostFreq
#' @export
mostFreq.logical <- function(x, ...) {
  tx <- as.vector(table(x))
  tx[2] > tx[1]
}

#' @rdname mostFreq
#' @export
mostFreq.numeric <- function (x, ...) {
  median(x, na.rm = TRUE)
}


# myCairo <- function(name, width, height, ...)
#   grDevices::cairo_pdf(filename = paste0(name, ".pdf"), height = height,
#                        width = width, family = "HersheySans",
#                        pointsize = 12)

#' @importFrom PBSmapping plotPolys
NULL

#'
#' Reasonably high quality map of the Northern Prawn Fishery
#'
#' Demonstration function
#'
#' @param col colour for the mainland
#' @param axes logical: do you want axes?
#' @param las graphics parameter
#'
#' @return as for \code{\link[graphics]{lines}}
#' @export
#'
#' @examples
#' NPF0()
#' text(Latitude ~ Longitude, roundTrip, Locality, col = "red", xpd = NA, pos = 2)
NPF0 <- function(col = alpha("#A67A5C", 0.5), axes = FALSE, las = 1) {
  # stopifnot(requireData(PBSmapping))
  plotPolys(worldLLhigh,
            xlim = c(126.85, 142.25), ylim = c(-17.85, -8.95), col = col,
            axes = axes, projection = "LL", xlab = "", ylab = "", las = las)
  lines(NPFBigCoastline)
}

#' @import rpart
NULL

#' One Standard Error Rule
#'
#' Extract the Cp value needed to implement the 1-standard-error rule in
#' tree prunng for tree objexts
#'
#' @param tree A tree object fitted with \code{\link[rpart]{rpart}}
#' @param f The number of standard errors (if not the default 1)
#' @param ... Presently ignored
#'
#' @return A value suitable for specifying as the cp argument of \code{\link[rpart]{prune}}
#' @export
oneSERule <- function (tree, f, ...) {
  # stopifnot(requireData(rpart))
  UseMethod("oneSERule")
}


#' @rdname oneSERule
#' @export
oneSERule.rpart <- function (tree, f = 1, ...) {
    cp <- data.frame(tree$cptable)    #$
    imin <- with(cp, which(xerror == min(xerror))[1])
    with(cp, CP[which(xerror <= xerror[imin] + f * xstd[imin])[1]])
}


#' Extract the out-of-bag error rate from a randomForest object
#'
#' Convenience function
#'
#' @param obj A classification object produced by \code{\link[randomForest]{randomForest}}
#' @param ... Presently ignored
#'
#' @return The overall out-of-bag error rate, if available
#' @export
OOB <- function(obj, ...) {
  # stopifnot(requireData(randomForest))
  UseMethod("OOB")
}

#' @rdname OOB
#' @export
OOB.default <- function(obj, ...)
  stop(sprintf("No OOB method for objects of class %s currently exists.",
               dQuote(class(obj))))


#' @rdname OOB
#' @export
OOB.randomForest <- function(obj, ...)
  with(obj, {
    here <- with(sys.status(), max(sys.parents))
    if(type == "classification" && exists("confusion", frame = here))
      err.rate[ntree, "OOB"] else NA
  })

#' Time stamping functions
#'
#' Convenience functions for time-stamping output or graphics
#'
#' @return a character string giving the date or the precise time
#' @export
#'
#' @examples
#' hist(~log(crim), Boston, binWidth(0.5), fill = pal_desert)
#' text("top right", right_now(), cex = 0.7, col = "steel blue")
today <- function() {
  as.character(Sys.Date())
}

#' @rdname today
#' @export
right_now <- function() {
  format(strptime(date(), "%a %b %d %H:%M:%S %Y"),
         format = "%Y-%m-%d %H:%M:%S (%a)")
}

