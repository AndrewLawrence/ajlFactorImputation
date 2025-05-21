
#' report_data
#' @keywords internal
#' @importFrom cli cli_h1 cli_bullets
report_data <- function(data) {
  cli::cli_h1("Dataset")

  cli::cli_bullets(c(
    "type: {class(data)}",
    "# variables: {ncol(data)}",
    "# observatations: {nrow(data)}"
  ))
  NULL
}

#' report_parsed_varlist
#' @keywords internal
report_parsed_varlist <- function(varlist) {
  cli::cli_h1("Variable roles")

  cli::cli_bullets(
    c(
      "* { length(varlist$fa) } var{?s}
      will be used in the {.emph factor analysis}.",
      "{varlist$fa}",
      "* { length(varlist$av) } auxiliary var{?s}
      will be used in the imputation.",
      "{varlist$av}",
      "* { length(varlist$other) } var{?s}
      will be appended to the results.",
      "{varlist$other}",
      "* { length(varlist$excluded) } var{?s}
      will be ignored.",
      "{varlist$excluded}"
    )
  )
  NULL
}

report_runningmifa <- function(x) {
  # x is an options object:
  cli::cli_h1("Running mifa: imputation & covariance matrix pooling")

  cli::cli_bullets(c("i" = "# imputations (m): { x$mice$m }",
                     "i" = "# iterations (maxit): { x$mice$maxit }"))
  NULL
}

report_nfactors <- function(x) {
  if ( x == "parallel" ) {
    cli::cli_h1("Determining number of Components/Factors by Parallel analysis")
    cat("\n")
    cli::cli_inform(c("Note:",
                      "Script suppresses all warning messages from fa.parallel
                      and assumes it works. This may not be the case."))
    return(NULL)
  }
  cli::cli_h1("Number of Components/Factors set to { x }")
  cat("\n")
  NULL
}

report_runningfa <- function(x, type) {
  # x is options$nfactors
  cli::cli_h1("Running { type } analysis with { x } factors")
  cat("\n")
}

report_missingness <- function(x) {
  # x is a data.frame
  cli::cli_h1("Imputation data missingness")
  cat("\n")

  md_n <- apply(x, 2, \(.x) sum(is.na(.x)))
  md_pc <- md_n / NROW(x)

  cat("N missing\n")
  print(md_n)
  cat("% missing\n")
  print(round(md_pc * 100))
  NULL
}

report_varexp <- function(x) {
  # x is the result of running mifa
  cli::cli_h1("Initial mifa variance explained")
  cat("\n")
  print(x$var_explained)
  NULL
}
