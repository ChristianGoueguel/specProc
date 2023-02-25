multiPeakFit2 <- function(x, y, peaks, profiles) {
  # x: vector of x values
  # y: vector of y values
  # peaks: vector of peak center positions
  # profiles: list of functions that describe the peak shapes

  # Set up initial parameter values and bounds
  nPeaks <- length(peaks)
  initPars <- c(rep(1, nPeaks), peaks, rep(1e-2, nPeaks))
  lowerBounds <- c(rep(0, nPeaks), peaks - diff(range(x))/20, rep(0, nPeaks))
  upperBounds <- c(rep(max(y), nPeaks), peaks + diff(range(x))/20, rep(max(y), nPeaks))

  # Define the fitting function
  peakFit <- function(pars) {
    # Evaluate the peak model for the given parameters
    model <- rep(0, length(x))
    for (i in 1:nPeaks) {
      model <- model + profiles[[i]](x, pars[i], pars[i + nPeaks], pars[i + 2*nPeaks])
    }
    # Return the difference between the model and the data
    return(model - y)
  }

  # Perform the fit
  fit <- minpack.lm::nlsLM(peakFit, start = initPars, lower = lowerBounds, upper = upperBounds)

  # Extract the fitted peak parameters
  peakParams <- cbind(peaks, coef(fit)[1:nPeaks], coef(fit)[(nPeaks + 1):(2*nPeaks)], coef(fit)[(2*nPeaks + 1):(3*nPeaks)])
  colnames(peakParams) <- c("center", "amplitude", "width", "offset")

  # Return the fitted peak parameters and the residuals
  return(list(peakParams = peakParams, residuals = peakFit(coef(fit))))
}


# Generate a simulated dataset with two Gaussian peaks
set.seed(123)
x <- seq(0, 10, length.out = 100)
y <- rnorm(100, mean = 2*dnorm(x, mean = 3, sd = 0.5) + dnorm(x, mean = 7, sd = 1), sd = 0.2)

# Define the peak shapes
gaussProfile <- function(x, center, amp, width) {
  return(amp*dnorm(x, mean = center, sd = width))
}
profiles <- list(gaussProfile, gaussProfile)

# Fit the peaks
peaks <- c(3, 7)
fit <- multiPeakFit2(x, y, peaks, profiles)
print(fit$peakParams)
plot(x, y)
lines(x, y - fit$residuals, col = "red")
