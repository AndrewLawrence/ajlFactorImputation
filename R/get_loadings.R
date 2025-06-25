
#' Extract the loadings of the factor analysis,
#'     usually from a \code{\link{ajlfacimp-class}}
#'     object, the output of \code{\link{factor_imputation}}.
#'
#' @param object An object.
#' @param ... Additional arguments.
#' @export
get_loadings <- function(object, ...) {
  UseMethod("get_loadings")
}

#' @export
get_loadings.default <- function(object, ...) {
  cli::cli_abort(
    c("get_loadings is not implemented for objects of class { class(object) }",
      "i" = "see the ajlFactorImputation::ajlfacimp-class")
  )
}

#' @export
get_loadings.ajlfacimp <- function(object, ...) {
  get_loadings(object[["fa"]])
}

#' @export
get_loadings.fa <- function(object, ...) {
  object[["loadings"]]
}

#' @export
get_loadings.principal <- function(object, ...) {
  object[["loadings"]]
}
