% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotSpec.R
\name{plotSpec}
\alias{plotSpec}
\title{Plotting of Spectra}
\usage{
plotSpec(x, id = NULL, colvar = NULL, .interactive = FALSE, drop_na = FALSE)
}
\arguments{
\item{x}{data frame or tibble of the spectra.}

\item{id}{optional (\code{NULL} by default). Column name of a factor variable
that identified each spectrum.}

\item{colvar}{optional (\code{NULL} by default). Column name of a numeric
variable to be display in color scale.}

\item{.interactive}{optional (\code{FALSE} by default). When set to \code{TRUE}
enables interactive plot.}

\item{drop_na}{Optional (\code{FALSE} by default). Remove rows with NA intensity
if drop_na is \code{TRUE}.}
}
\value{
Object of class ggplot or of class plotly if \code{.interactive = TRUE}.
}
\description{
Spectrum plots are commonly x–y plots in which the x-axis represents the
wavelength and the y-axis represents intensity of a spectrum's signal.
The function allows to plot a spectrum or several spectra in a single plot,
identified either by an id (for example, the samples or spectra id) or by a
target variable (for example, the concentration of a chemical element).
}
\details{
This function is based on the ggplot2 package, thus allowing users to easily
add or modify different components of the plot.
}
\author{
Christian L. Goueguel
}
