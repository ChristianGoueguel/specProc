% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/umad.R
\name{umad}
\alias{umad}
\title{Unbiased Median Absolute Deviation}
\usage{
umad(x, method = "hayes", drop.na = TRUE)
}
\arguments{
\item{x}{A numeric vector.}

\item{method}{A character string specifying the method to use for calculating the correction factor when the number of sample is more than 100. The available options are "hayes" (default) and "williams".}

\item{drop.na}{A logical value indicating whether to remove missing values (NA) from the calculations. If \code{TRUE} (the default), missing values will be removed. If \code{FALSE}, missing values will be included.}
}
\value{
The MAD scale estimate for the input vector \code{x}.
}
\description{
This function calculates the Median Absolute Deviation (MAD) scale
estimator for a numeric vector, using the Park-Kim-Wang approach. The method
adds a small sample correction factor to make MAD unbiased at the normal distribution.
}
\details{
The correction factor, \eqn{C}, is calculated differently based on the sample size \eqn{n}:
\itemize{
\item For \eqn{n > 100}, \eqn{C} is calculated using an analytical approximation
proposed by either Hayes (2014) or Williams (2011).
\item For \eqn{n \leq 100}, \eqn{C} is obtained from a pre-computed table of
values proposed by Park \emph{et al.} (2020).
}
}
\references{
\itemize{
\item Park, C., Kim, H., Wang, M., (2020).
Investigation of finite-sample properties of robust location and scale
estimators.
Communications in Statistics - Simulation and Computation, 51(5):2619–2645.
\item Hayes, K., (2014).
Finite-Sample Bias-Correction Factors for the Median
Absolute Deviation.Communications in Statistics - Simulation and Computation, 43(10):2205–2212.
\item Williams, D.C., (2011).
Finite sample correction factors for several simple robust estimators of
normal standard deviation.
Journal of Statistical Computation and Simulation, 81(11):1697–1702.
}
}
\author{
Christian L. Goueguel
}
