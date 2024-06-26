% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snv.R
\name{snv}
\alias{snv}
\title{Standard Normal Variate}
\usage{
snv(x, drop.na = TRUE)
}
\arguments{
\item{x}{A numeric matrix or data frame.}

\item{drop.na}{A logical value indicating whether to remove missing values
(NA) from the calculations. If \code{TRUE} (the default), missing values will be
removed. If \code{FALSE}, missing values will be included.}
}
\value{
A list with the following components:
\item{\code{correction}}{The SNV-scaled data.}
\item{\code{means}}{A vector of row means.}
\item{\code{stds}}{A vector of row standard deviations.}
}
\description{
This function performs Standard Normal Variate (SNV) scaling on the input
spectral data. SNV scaling scales each row of the input data to have zero
mean and unit standard deviation. This is equivalent to autoscaling the
transpose of the input data.
}
\author{
Christian L. Goueguel
}
