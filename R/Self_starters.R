#' Modify Self-starting Model Functions
#' 
#' This is a work-around function to modify the output functions from \code{deriv}
#' or \code{selfStart} to allow more flexibility when using them with \code{nlmer}
#' from the \code{lme4} package.
#'
#' @param func A function object, the output from \code{deriv} or \code{selfStart}
#'
#' @return A modified function with one extra line of code inserted
#' @export
#'
#' @examples
#' mod_fun <- deriv(~ b*v/(w - c), namevec = c("b", "c"), function.arg = function(v, w, b, c) {})
#' mod_fun_fix <- fix_deriv(mod_fun)
fix_deriv <- function(func) {
  if(mode(func) != "function") {
    return(func)
  }
  code <- as.list(body(func))
  if(any(grepl("(actualArgs|dimnames|colnames)", code)) ||
     (sum(k <- grepl('attr\\(\\.value, "gradient"\\) <- \\.grad', code)) != 1)) {
    warning("The function seems to have been fixed.  Check code.")
    return(func)
  }
  attribs <- attributes(func)
  k <- which(k) - 1
  code <- c(code[1:k],
            list(quote(colnames(.grad) <- as.list(match.call())[colnames(.grad)])),
            code[-(1:k)])
  body(func) <- as.call(code)
  attributes(func) <- attribs
  func
}

#' Initial Value Functions
#' 
#' Free-standing functions to arrive at suitable initial values for non-linear model
#' fitting functions
#' 
#' @name initial_value
#'
#' @param x numeric vector; the independent variable
#' @param y numeric vector; the response variable
#'
#' @return A vector of initial values for the parameters with appropriate names
#' @export
#'
#' @examples
#' library(WWRData)
#' with(muscle, gompertz2_init(Conc, Length))
gompertz2_init <- function(x, y) {
  r <- range(x)
  d <- (r[2] - r[1])/4
  yf <- predict(lm(log(y) ~ poly(x, 2)), data.frame(x = r[1] + (1:3)*d))
  rho <- ((yf[3] - yf[2])/(yf[2] - yf[1]))^(1/d)
  b <- coef(lm(log(y) ~ 1 + I(-rho^x)))
  setNames(c(b, rho), c("b0", "b1", "rho"))
}


#' @rdname initial_value
#' @export
gompertz3_init <- function(x, y) {
  b <- coef(nls(log(y) ~ SSnegexp(x, b0, b1, rho)))
  b["b1"] <- -b["b1"]
  b["rho"] <- exp(-1/b["rho"])
  b
}

#' @rdname initial_value
#' @export
negexp_init <- function(x, y) {
  r <- range(x)
  d <- diff(r)/4
  yf <- predict(lm(y ~ poly(x, 2)), data.frame(x = r[1] + (1:3)*d))
  ratio <- (yf[1] - yf[2])/(yf[2] - yf[3])
  stopifnot(ratio > 0)
  theta <- unname(d/log(ratio))
  b <- unname(coef(lm(y ~ 1 + exp(-x/theta))))
  list(b0 = b[1], b1 = b[2], theta = theta)
}

#' Self-starting Model Functions
#' 
#' Self-starting model fitting functions for non-linear Gompertz Growth Curve models.
#' The non-linear model is of the form ~ exp(b0 - b1*rho^x)
#' 
#' The functions are not used directly, but in non-linear model formulae
#'
#' @param x numeric vector, time variable or analogue
#' @param b0,b1.rho place-holders for the parameter estimates. 
#'
#' @return An evaluated model function, with a derivative matrix attribute
#' @export
#'
#' @examples
#' fm <- nls(Length ~ SSgompertz2(Conc, b0, b1, rho), data = muscle)
#' coef(fm)
SSgompertz2 <- structure(function (x, b0, b1, rho) {
  .expr1 <- rho^x
  .expr4 <- exp(b0 - b1 * .expr1)
  .value <- .expr4
  .grad <- array(0, c(length(.value), 3L), 
                 list(NULL, c("b0", "b1", "rho")))
  .grad[, "b0"] <- .expr4
  .grad[, "b1"] <- -(.expr4 * .expr1)
  .grad[, "rho"] <- -(.expr4 * (b1 * (rho^(x - 1) * x)))
  colnames(.grad) <- as.list(match.call())[colnames(.grad)]
  attr(.value, "gradient") <- .grad
  .value
}, initial = function(mCall, data, LHS) {
  x <- eval(mCall[["x"]], data)
  y <- eval(LHS, data)
  r <- range(x)
  d <- (r[2] - r[1])/4
  yf <- predict(lm(log(y) ~ poly(x, 2)), data.frame(x = r[1] + (1:3)*d))
  ratio <- ((yf[3] - yf[2])/(yf[2] - yf[1]))
  stopifnot(ratio > 0)
  rho <- ratio^(1/d)
  b <- coef(lm(log(y) ~ 1 + I(-rho^x)))
  setNames(c(b, rho), mCall[c("b0", "b1", "rho")])
}, pnames = c("b0", "b1", "rho"), class = "selfStart")


#' @rdname SSgompertz2
#' @export
SSgompertz3 <- structure(function (x, b0, b1, rho) {
  .expr1 <- rho^x
  .expr4 <- exp(b0 - b1 * .expr1)
  .value <- .expr4
  .grad <- array(0, c(length(.value), 3L), 
                 list(NULL, c("b0", "b1", "rho")))
  .grad[, "b0"] <- .expr4
  .grad[, "b1"] <- -(.expr4 * .expr1)
  .grad[, "rho"] <- -(.expr4 * (b1 * (rho^(x - 1) * x)))
  colnames(.grad) <- as.list(match.call())[colnames(.grad)]
  attr(.value, "gradient") <- .grad
  .value
}, initial = function(mCall, data, LHS) {
  x <- eval(mCall[["x"]], data)
  y <- eval(LHS, data)
  b <- coef(nls(log(y) ~ SSnegexp(x, b0, b1, rho)))
  b["b1"] <- -b["b1"]
  b["rho"] <- exp(-1/b["rho"])
  b
}, pnames = c("b0", "b1", "rho"), class = "selfStart")

#' Negative Exponential Model
#' 
#' A self-starting model fitting function for a negative exponential model.
#' The model formula is ~ b0 + b1*exp(-t/theta)
#'
#' @param t The time variable, or analogue
#' @param b0,b1,theta  place-holders for the parameter estimates
#'
#' @return An evaluated model function, with a derivative matrix attribute
#' @export
#'
#' @examples
#' fm <- nls(Weight ~ SSnegexp(Days, b0, b1, theta), wtloss)
#' coef(fm)
SSnegexp <- structure(function (t, b0, b1, theta) {
  .expr3 <- exp(-t/theta)
  .value <- b0 + b1 * .expr3
  .grad <- array(0, c(length(.value), 3L),
                 list(NULL, c("b0", "b1", "theta")))
  .grad[, "b0"] <- 1
  .grad[, "b1"] <- .expr3
  .grad[, "theta"] <- b1 * (.expr3 * (t/theta^2))
  colnames(.grad) <- as.list(match.call())[colnames(.grad)]
  attr(.value, "gradient") <- .grad
  .value
}, initial = function(mCall, data, LHS) {
  t <- eval(mCall[["t"]], data)
  y <- eval(LHS, data)
  r <- range(t)
  d <- (r[2] - r[1])/4
  yf <- predict(lm(y ~ poly(t, 2)), data.frame(t = r[1] + (1:3)*d))
  ratio <- (yf[1] - yf[2])/(yf[2] - yf[3])
  stopifnot(ratio > 0)
  theta <- d/log(ratio)
  b <- coef(lm(y ~ 1 + exp(-t/theta)))
  setNames(c(b, theta), mCall[c("b0", "b1", "theta")])
}, pnames = c("b0", "b1", "theta"), class = "selfStart")

#' Stormer Viscometer Calibration Model
#' 
#' A self-starting model fitting function for the Stormer Viscometer calibration example
#' The model fitted is ~ b*v/(w - c)
#'
#' @param v,w  numeric vectors, known viscosities and actuating weights respectively
#' @param b,c  place-holders for the parameter estimates
#'
#' @return An evaluated model function, with a derivative matrix attribute
#' @export
#'
#' @examples
#' fm <- nls(Time ~ SSstormer(Viscosity, Weight, beta, theta), Stormer)
#' coef(fm)
SSstormer <- structure(function (v, w, b, c) {
  .expr1 <- b * v
  .expr2 <- w - c
  .value <- .expr1/.expr2
  .grad <- array(0, c(length(.value), 2L), 
                 list(NULL, c("b", "c")))
  .grad[, "b"] <- v/.expr2
  .grad[, "c"] <- .expr1/.expr2^2
  colnames(.grad) <- as.list(match.call())[colnames(.grad)]
  attr(.value, "gradient") <- .grad
  .value
}, initial = function(mCall, data, LHS) {
  t <- eval(LHS, data)
  v <- eval(mCall[["v"]], data)
  w <- eval(mCall[["w"]], data)
  b <- coef(lm(I(w*t) ~ 0 + v + t))
  setNames(b, mCall[c("b", "c")])
}, pnames = c("b", "c"), class = "selfStart")

#' Simulation method for non-linear models
#' 
#' This method function is based on stats:::simulate.lm and has simular functionality
#'
#' @param object A fitted model object
#' @param nsim The number of simulated response variable simulations required
#' @param seed An optional seed for the random number generator
#' @param ... additional arguments used by other methods.
#'
#' @return  A data frame with of size n x nsim, with columns the simulated response variables
#' @export
#'
#' @examples
#' fm <- nls(Time ~ SSstormer(Viscosity, Weight, beta, theta), data = Stormer)
#' Times <- simulate(fm, 10, seed = 2021)
#' 
simulate.nls <- function (object, nsim = 1, seed = NULL, ...) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  if (is.null(seed)) 
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  ftd <- fitted(object)
  n <- length(ftd)
  nm <- names(ftd)
  ntot <- n * nsim
  vars <- deviance(object)/df.residual(object)
  if (!(is.null(w <- object$weights) || (length(w) == 1L && w == 1))) vars <- vars/w
  val <- ftd + rnorm(ntot, sd = sqrt(vars))
  dim(val) <- c(n, nsim)
  val <- as.data.frame(val)
  names(val) <- paste0("sim_", seq_len(nsim))
  row.names(val) <- nm
  attr(val, "seed") <- RNGstate
  val
}