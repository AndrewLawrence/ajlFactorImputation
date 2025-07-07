

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

  # for sense checks we will always run the default mice
  #   variable checks.
  test_mice_call$eps <- NULL
  test_mice_call$maxcor <- NULL
  test_mice_call$threshold <- NULL
  test_mice_call$remove.constant <- NULL
  test_mice_call$remove.collinear <- NULL

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


#' @rdname sense_checks
#' @keywords internal
sensecheck_fahasfactors <- function(status, data) {
  chk <- vapply(data, \(x) !(is.numeric(x) | is.ordered(x)), FUN.VALUE = TRUE)
  if ( any(chk) ) {
    status <- TRUE
    report <- names(which(chk))
    cli::cli_warn(
      c(
        "run_checks: non-numeric variables subjected to factor analysis",
        "x" = "problem vars: {report}",
        "i" = "Non-numeric variables ususally don't make sense.",
        "i" = "input to factor analysis must be explicitly converted to numeric"
      )
    )
  }
  status
}


#' @rdname sense_checks
#' @keywords internal
sensecheck_characterfmt <- function(status, data) {
  chk <- vapply(data, \(x) is.character(x), FUN.VALUE = TRUE)
  if ( any(chk) ) {
    status <- TRUE
    report <- names(which(chk))
    cli::cli_warn(
      c(
        "run_checks: auxiliary variables have character format",
        "x" = "problem vars: {report}",
        "i" = "Better for reproducibility to convert to factor"
      )
    )
  }
  status
}

#' @rdname sense_checks
#' @keywords internal
sensecheck_facnlevels <- function(status, data) {
  fac <- vapply(data, \(x) (is.character(x) | is.factor(x)), FUN.VALUE = TRUE)
  fac_data <- data[, names(which(fac))]
  nlevels <- vapply(data, \(x) length(unique(x)), FUN.VALUE = 1L)
  thr <- round(nrow(data) / 10)
  chk <- nlevels > thr
  if ( any(chk) ) {
    status <- TRUE
    report <- names(which(chk))
    cli::cli_warn(
      c(
        "run_checks: auxiliary factors have too many levels",
        "x" = "problem vars: {report}",
        "i" = "Check for errors, consider collapsing rare categories."
      )
    )
  }
  status
}


#' @rdname sense_checks
#' @keywords internal
sensecheck_facminfreq <- function(status, data) {
  fac <- vapply(data, \(x) (is.character(x) | is.factor(x)), FUN.VALUE = TRUE)
  fac_data <- data[, names(which(fac))]
  nlevels <- vapply(data, \(x) min(table(x)), FUN.VALUE = 1L)
  thr <- 5L
  chk <- nlevels <= thr
  if ( any(chk) ) {
    status <- TRUE
    report <- names(which(chk))
    cli::cli_warn(
      c(
        "run_checks: auxiliary factors have level with too few observations",
        "x" = "problem vars: {report}",
        "i" = "Check for errors, consider collapsing rare categories."
      )
    )
  }
  status
}

#' @rdname sense_checks
#' @keywords internal
sensecheck_missingvaluecodes <- function(status, data) {
  mv_codes <- c(-9999, -8888, -7777, -999, -888, -777, -99, -88, -77)
  res <- vector(mode = "list", length = length(mv_codes))
  res <- setNames(res, mv_codes)

  for ( i in seq_along(mv_codes) ) {
    loc <- which(data == mv_codes[i], arr.ind = TRUE)
    if ( nrow(loc) > 0 ) {
      res[[i]] <- colnames(data)[loc[, "col"]]
    }
  }

  chk <- !vapply(res, is.null, FUN.VALUE = TRUE)
  if ( any(chk) ) {
    status <- TRUE
    report <- paste0(names(res)[which(chk)], ": ", res[[which(chk)]])
    cli::cli_warn(
      c(
        "run_checks: unusual values typically used for missing data found",
        "x" = "problem vars: {report}",
        "i" = "Check for errors or turn off checks if valid."
      )
    )
  }
  status
}
