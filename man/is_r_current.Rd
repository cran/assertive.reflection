% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert-is-current.R, R/is-current.R
\name{assert_is_r_current}
\alias{assert_is_r_current}
\alias{assert_is_current_r}
\alias{is_r_current}
\title{Is this version of R up to date?}
\usage{
assert_is_r_current(severity = getOption("assertive.severity", "stop"))

assert_is_current_r(severity = getOption("assertive.severity", "stop"))

is_r_current(
  cran = getOption("repos", c(CRAN = "http://cran.r-project.org"))["CRAN"]
)
}
\arguments{
\item{severity}{How severe should the consequences of the assertion be?
Either \code{"stop"}, \code{"warning"}, \code{"message"}, or \code{"none"}.}

\item{cran}{A string giving the URL of the CRAN repository to check.}
}
\value{
An object of class \code{R_system_version} giving the current release
version of R.
}
\description{
Check if this version of R is as new as the current release version of R.
}
\note{
Development versions of R can have versions higher than the current
release version of R.  For convenience, these will return \code{TRUE}.
}
\examples{
\donttest{
# This example is marked "don't test" since it requires an 
# internet connection and is potentially long running
is_r_current()
}
}
