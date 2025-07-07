
#' fami_options
#'
#' Initialise options for \link{factor_imputation}. Detailed settings for
#' imputation and factor analysis are initialised, but the important overall
#' setting parameters are:
#' * fa_type = do you want a PCA or a common factor model?
#' * nfactors = how many factors to extract (or a method to calculate this)
#' * quickrun = tries to set several parameters to run quickly
#'      (if inaccurately) for testing purposes.
#'
#' @inheritParams mice::mice
#' @inheritParams mice::mice.impute.rf
#' @param fa_type Either Factor Analysis or PCA
#' @param nfactors Either, 1) the integer number of factors (or components)
#'     to extract, -OR- 2) the string "parallel" indicating the parallel method
#'     (\link[psych]{fa.parallel}) should be applied to estimate the number of
#'     factors/components. The fa_type argument will determine whether factors
#'     or components are used.
#' @param quickrun If TRUE, options will skimp on iterations to test basics
#' @param mice_var_checks If \code{FALSE} (default) will set arguments so mice
#'     does not run particular variable checks (constants,
#'     multicollinearity, high correlation) that are not relevant
#'     when using random forests as the imputation model. For non-rf multiple
#'     imputation methods, disabling checks may produce errors/poor
#'     performance. The default imputation method in
#'     \code{\link{ajlFactorImputation}} is random forest, so these checks are
#'     disabled by default.
#' @param n_imputations Number of mice imputations (called `m` by mice).
#'     If NULL, will be 10 (quickrun: 5)
#' @param ntree The number of trees to grow in the random forest imputation
#'     method, default of 100 is used (quickrun: 10)
#' @param defaultMethod How should mice impute values? This is a length 4
#'     character vector (see \code{\link[mice]{mice}}) giving defaults for four
#'     variable types. If unspecified (`NULL`) then random forest ("rf")
#'     will be used for all types.
#' @export
fami_options <- function(
  fa_type = c("fa", "pca"),
  nfactors = "parallel",
  quickrun = FALSE,
  mice_var_checks = FALSE,
  n_imputations = NULL,
  ntree = NULL,
  maxit = NULL,
  defaultMethod = NULL #nolint
) {

  # The quickrun flag will yield faster running (but less robust) parameters:
  checkmate::assertFlag(quickrun)

  # argument checking:
  checkmate::assertFlag(mice_var_checks)
  checkmate::qassert(n_imputations, c("n1", "0"))
  checkmate::qassert(ntree, c("n1", "0"))
  checkmate::qassert(maxit, c("n1", "0"))

  checkmate::assert(
    checkmate::checkChoice(nfactors, choices = "parallel"),
    checkmate::checkInt(nfactors, lower = 1),
    combine = "or"
  )

  fa_type <- checkmate::matchArg(fa_type, choices = c("fa", "pca"))

  # Argument handling:
  if ( missing(n_imputations) ) {
    n_imputations <- 10
    if ( quickrun ) {
      n_imputations <- 3
    }
  }

  if ( missing(maxit) ) {
    maxit <- 10
    if ( quickrun ) {
      maxit <- 5
    }
  }

  if ( missing(ntree) ) {
    ntree <- 100
    if ( quickrun ) {
      ntree <- 10
    }
  }

  if ( missing(defaultMethod) ) { #nolint
    defaultMethod <- c("rf", "rf", "rf", "rf") #nolint
  }

  # PCA/Factor analysis setup:
  #   see: https://lhbikos.github.io/ReC_Psychometrics/PAF.html

  # Factor analysis (fa_type = "fa")
  #   will be passed to the psych::fa function
  #   note: nfactors, n.obs handled separately.
  fa <- list(
    # factoring method:
    fm = "pa",       # principal axes factoring
    max.iter = 100,  # increase from the psych default of 50
    rotate = "oblimin",
    scores = "regression"
  )

  # Principal Component Analysis (fa_type = "pca")
  #   will be passed to the psych::principal function
  #   note: nfactors, n.obs handled separately.
  pca <- list(
    rotate = "varimax",
    method = "regression"
  )

  mice_opts <- list(
    m = n_imputations,
    maxit = maxit,
    defaultMethod = defaultMethod,
    ntree = ntree
  )

  if ( ! mice_var_checks ) {
    mice_opts$eps <- 0
    mice_opts$maxcor <- 1.0
    mice_opts$threshold <- 1.0
    mice_opts$remove.constant <- FALSE
    mice_opts$remove.collinear <- FALSE
  }

  # Return the options object:
  list(
    type = fa_type,
    mice = mice_opts,
    fa = fa,
    pca = pca,
    nfactors = nfactors
  )
}
