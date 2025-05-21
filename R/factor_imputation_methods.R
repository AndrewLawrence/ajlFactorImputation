
#' Extract the mice "mids" object, usually from a \code{\link{ajlfacimp-class}}
#'     object, the output of \code{\link{factor_imputation}}.
#'
#' @param object An object.
#' @param ... Additional arguments.
#' @export
get_mids <- function(object, ...) {
  UseMethod("get_mids")
}

#' @export
get_mids.default <- function(object, ...) {
  cli::cli_abort(
    c("get_mids is not implemented for objects of class { class(object) }",
      "i" = "see the ajlFactorImputation::ajlfacimp-class")
  )
}


#' @export
get_mids.ajlfacimp <- function(object, ...) {
  object[["mifa"]][["mids"]]
}
