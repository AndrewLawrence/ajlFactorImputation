
#' Extract the \code{\link{mifa}} pooled covariance matrix,
#'     usually from a \code{\link{ajlfacimp-class}}
#'     object, the output of \code{\link{factor_imputation}}.
#'
#' @param object An object.
#' @param ... Additional arguments.
#' @export
get_pooledcov <- function(object, ...) {
  UseMethod("get_pooledcov")
}

#' @export
get_pooledcov.default <- function(object, ...) {
  cli::cli_abort(
    c("get_pooledcov is not implemented for objects of class { class(object) }",
      "i" = "see the ajlFactorImputation::ajlfacimp-class")
  )
}

#' @export
get_pooledcov.ajlfacimp <- function(object, ...) {
  object[["mifa"]][["cov_combined"]]
}

#' @export
get_pooledcov.mifa <- function(object, ...) {
  object[["cov_combined"]]
}
