% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yGradientglsw.R
\name{yGradientglsw}
\alias{yGradientglsw}
\title{y-Gradient Generalized Least Squares Weighting}
\usage{
yGradientglsw(x, y, alpha = 0.01)
}
\arguments{
\item{x}{A numeric matrix, data frame or tibble, representing the predictors data.}

\item{y}{A numeric vector representing the response vector.}

\item{alpha}{A numeric value specifying the weighting parameter. Typical values range from 1 to 0.0001. Default is 0.01.}
}
\value{
A tibble containing the filtering matrix.
}
\description{
The y-gradient generalized least squares weighting algorithm (GLSW) removes
variance from the data (spectra), which is orthogonal to the response.
}
\details{
The y-Gradient GLSW is an alternative method to GLSW, where a continuous \eqn{\textbf{y}}-variable
is used to develop pseudo-groupings of samples in \eqn{\textbf{X}} by comparing
the differences in the corresponding \eqn{\textbf{y}} values. This is referred to as the \emph{"gradient method"}
because it utilizes a gradient of the sorted \eqn{\textbf{X}}- and
\eqn{\textbf{y}}-blocks to calculate a covariance matrix.
}
\references{
\itemize{
\item Zorzetti, B.M., Shaver, J.M., Harynuk, J.J., (2011).
Estimation of the age of a weathered mixture of volatile organic compounds.
Analytica Chimica Acta, 694(1-2):31–37.
}
}
