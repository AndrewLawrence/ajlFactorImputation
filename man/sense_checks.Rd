% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_imputation_sensechecks.R
\name{sense_checks}
\alias{sense_checks}
\alias{sensecheck_dryrunmice}
\alias{sensecheck_faobspervar}
\alias{sensecheck_fatotalobs}
\title{Sense check}
\usage{
sensecheck_dryrunmice(status, options, data)

sensecheck_faobspervar(status, nvar, nobs)

sensecheck_fatotalobs(status, nobs)
}
\description{
Internal functions to check for common problems in the input to
\code{\link{factor_imputation}}.
Turn off these checks with \code{run_checks = FALSE}.
}
\keyword{internal}
