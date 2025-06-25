

#' Sense check
#'
#' Internal functions to check for common problems in the input to
#' \code{\link{factor_imputation}}.
#' Turn off these checks with `run_checks = FALSE`.
#'
#' @name sense_checks
#' @rdname sense_checks
#' @keywords internal
sensecheck_dryrunmice <- function(status, options, data) {
  test_mice_call <- append(options$mice,
                           values = list(data = data),
                           after = 0L)
  test_mice_call$maxit <- 0L

  test_mice <- do.call(mice::mice, test_mice_call)
  test_mice_log <- test_mice$loggedEvents

  if ( !is.null(test_mice_log) ) {
    cli::cli({
      cli::cli_inform("run_checks: mice issues with the imputation data:")
      cli::cat_print(test_mice_log)
      cli::cli_warn(c(
        "A mice 'dry run' identified basic problems with the variables.",
        "i" = "See {.code meth} and {.code out} columns above",
        "i" = "common problems are constant and colinear variables",
        "i" = "the 'meth' column is the reason for exclusion",
        "i" = "the 'out' column is the variable excluded",
        "i" = "Do not use constant/colinear variables in the imputation",
        "i" = "they can be included in the {.code other_vars} list."
      ))
    })
    status <- TRUE
  }
  status
}

#' @rdname sense_checks
#' @keywords internal
sensecheck_faobspervar <- function(status, nvar, nobs) {
  if ( (nobs / nvar) < 10 ) {
    cli::cli_warn(c(
      "run_checks: too few observations / variable for 'fa'",
      "i" = "You have requested common factor analysis ('fa')",
      "i" = "n variables: {nvar}",
      "i" = "n obs: {nobs}",
      "x" = "obs / variable: { round(nobs / nvar, 3) } < 10",
      "i" = "Threshold for likely 'fa' problems: <10 obs / variable"
    ))
    status <- TRUE
  }
  status
}


#' @rdname sense_checks
#' @keywords internal
sensecheck_fatotalobs <- function(status, nobs) {
  if (nobs < 100) {
    cli::cli_warn(
      c(
        "run_checks: too few observations for 'fa'",
        "i" = "You have requested common factor analysis ('fa')",
        "x" = "n obs: {nobs} < 100",
        "i" = "'fa' factor analysis unlikely to work well with n<100"
      )
    )
    status <- TRUE
  }
  status
}
