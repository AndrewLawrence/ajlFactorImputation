# Input checks and parsing functions:

#' @importFrom checkmate assertDataFrame
check_data <- function(df) {
  checkmate::assertDataFrame(df, all.missing = FALSE,
                             min.rows = 2, min.cols = 2)
}

check_impute_data <- function(df) {
  # Check rows:
  chk <- apply(df, 1, \(.x) sum(is.na(.x)) == length(.x) )
  if ( any(chk) ) {
    cat("\n")
    cli::cli_abort(c(
      "Some rows have no non-missing data.
      These are not suitable for imputation.",
      "i" = "Empty row id{?s}: { which(chk) }"
    ))
  }
  # check cols:
  chk <- apply(df, 2, \(.x) sum(is.na(.x)) == length(.x) )
  if ( any(chk) ) {
    cat("\n")
    cli::cli_abort(c(
      "Some variables have no non-missing data.
      These are not suitable for imputation.",
      "i" = "Empty col id{?s}: { which(chk) }"
    ))
  }
}

#' @importFrom checkmate qassert assertSubset assertDisjunct
check_varlists <- function(fa, av, other, cnames) {

  # string vectors of length 1+ (or NULL)
  checkmate::qassert(fa, c("s+", "0"))
  checkmate::qassert(av, c("s+", "0"))
  checkmate::qassert(other, c("s+", "0"))

  # check in cnames:
  checkmate::assertSubset(fa, cnames)
  checkmate::assertSubset(av, cnames)
  checkmate::assertSubset(other, cnames)

  # names assigned to roles without collisions:
  checkmate::assertDisjunct(fa, av)
  checkmate::assertDisjunct(fa, other)
  checkmate::assertDisjunct(av, other)

}

parse_varlists <- function(fa, av, other, cnames) {
  if ( is.null(fa) ) {
    fa <- cnames[! cnames %in% c(av, other)]
  }
  if ( is.null(av) ) {
    av <- cnames[! cnames %in% c(fa, other)]
  }
  if ( is.null(other) ) {
    other <- cnames[! cnames %in% c(fa, av)]
  }
  excluded <- cnames[! cnames %in% c(fa, av, other) ]
  list(fa = fa,
       av = av,
       other = other,
       excluded = excluded)
}
