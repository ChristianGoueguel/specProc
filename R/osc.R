
osc <- function(x, y, method = "wold", center = TRUE, osc.ncomp = 4, pls.ncomp = 10, tol = 1e-3, iter = 20, ...) {

  #' arguments validity checking
  if (missing(x) || missing(y)) {
    stop("data set or class are missing")
  }
  if (nrow(x) != length(y)) stop("x and y don't match.")
  if (length(unique(y)) < 2) {
    stop("Classification needs at least two classes.")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("NA is not permitted in data set or class labels.")
  }

  method <- match.arg(method, c("wold", "sjoblom", "wise"))

  #' initialization
  x <- as.matrix(x)
  y <- as.factor(y)
  n <- nrow(x)
  p <- ncol(x)

  if (pls.ncomp < 1 || pls.ncomp > min(n - 1, p)) {
    pls.ncomp <- min(n - 1, p)
    }

  #' Select OSC algorithm:
  osc_method <- switch(method, wold = osc_wold, sjoblom = osc_sjoblom, wise = osc_wise)

  #' call OSC algorithm
  res <- osc_method(x, y, center = center, osc.ncomp = osc.ncomp, pls.ncomp = pls.ncomp, tol = tol, iter = iter, ...)

  #' Build and return the object:
  res$call <- match.call()
  res$call[[1]] <- as.name("osc")
  res$center <- center
  res$osc.ncomp <- osc.ncomp
  res$pls.ncomp <- pls.ncomp
  res$method <- method

  class(res) <- "osc"
  return(res)

  }
