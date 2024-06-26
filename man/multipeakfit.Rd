% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multipeakfit.R
\name{multipeakfit}
\alias{multipeakfit}
\title{Multiple Peaks Fitting}
\usage{
multipeakfit(
  x,
  peaks,
  profiles,
  wL = NULL,
  wG = NULL,
  A = NULL,
  wlgth.min = NULL,
  wlgth.max = NULL,
  id = NULL,
  max.iter = 200
)
}
\arguments{
\item{x}{A data frame or tibble}

\item{peaks}{A vector of the selected peaks center wavelengths}

\item{profiles}{A vector of the lineshape functions for fitting}

\item{wL}{A numeric of the Lorentzian full width at half maximum (overall initial guess)}

\item{wG}{A numeric of the Gaussian full width at half maximum (overall initial guess)}

\item{A}{A numeric of the peak area (overall initial guess)}

\item{wlgth.min}{A numeric of the lower bound of the wavelength subset}

\item{wlgth.max}{A numeric of the upper bound of the wavelength subset}

\item{id}{A character specifying the spectra id (optional)}

\item{max.iter}{A numeric specifying the maximum number of iteration (200 by default)}
}
\value{
Fitted value for each peak and the estimated parameters along with the
corresponding errors
}
\description{
Fitting of multiple spectral lines by the same or different lineshape
functions with variable parameters.
}
\details{
The function uses \code{minpack.lm::nlsLM}, which is based on the Levenberg-Marquardt
algorithm for searching the minimum value of the square of the sum of the residuals.
}
\author{
Christian L. Goueguel
}
