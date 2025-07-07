

#' factor_imputation
#'
#' Run imputation to produce a pooled covariance matrix, run FA/PCA on that
#'  matrix and then return multiply imputed factor scores within a mice
#'  \code{\link[mice]{mids}} object.
#'  Variables in the `data` data.frame can have different roles, either:
#'  1) Subjected to factor analysis with imputation (`fa_vars`),
#'  2) Used as an auxiliary variable in the imputation (`av_vars`), but not the
#'     factor analysis.
#'  3) or passed (without being used in imputation or factor analysis)
#'      to the return object (`other_vars`).
#'
#' This function will check the input data. Ensure you do not attempt to
#'     include either rows (observations) or columns (variables) which have
#'     completely missing data in the factor-imputation.
#'
#' Variables in the input data.frame should be set to their desired roles
#'  in the analysis using `fa_vars`, `av_vars` and `other_vars`.
#'  In general the function assumes that if you included a variable in
#'  `data` then it should do something. Specifically:
#' * If you don't set `fa_vars` then all variables not in another role list
#'   are subjected to factor analysis.
#' * If you don't set `av_vars` all remaining variables not in `fa_vars` or
#'   `other_vars` are used as auxiliary variables.
#' * If you set `fa_vars` and `av_vars` but not `other_vars` then all remaining
#'   variables will be treated as "other" variables.
#' * If you want to include variables in the input `data`, but then not include
#'   them in the output then you must specify `fa_vars`, `av_vars`,
#'   _AND_ `other_vars`. If this is done then variables not listed in any of
#'   those roles will be excluded and not appear in output objects.
#'
#'    note that the imputation will apply to a data.frame constructed as:
#'    `data[,c(fa_vars, av_vars)]`.
#'
#' @param data a data.frame of suitable format for \code{\link[mifa]{mifa}}.
#'     Note that any rows (observations) or columns (variables) which
#'     are "empty" i.e. contain only missing data (NA values) are unsuitable
#'     for imputation and will throw errors if in the `fa_vars` or `av_vars`
#'     variable sets.
#' @param fa_vars
#' Factor analysis variable names
#' (derive a pooled covariance matrix for these).
#' Format:
#' * A character vector of column names in `data`
#' * NULL: use everything not in `av_vars` or `other_vars`.
#' @param av_vars
#' Auxiliary variable names
#' (use these when imputing, but not in the factor analysis).
#' To be useful auxiliary variables should potentially provide
#' information about missing values. i.e. they should be observed
#' when variables with missing data were not, and related to the variables
#' with missing values.
#' Format:
#' * A character vector of column names in `data`.
#' * NULL: use everything not in `fa_vars` or `other_vars`.
#' @param other_vars
#' Other variables. Variables to append in output, but neither use in
#' the factor analysis or in imputation. E.g. ID variables.
#' Format:
#' * A character vector of column names in `data`.
#' * NULL: use everything not in `fa_vars` or `other_vars`.
#' @param options an options object produced by \link{fami_options}.
#'    See \link{fami_options} for more information.
#' @param run_checks If TRUE then sense checks will be run and
#'    the function will halt if any sense checks fail. Otherwise sense
#'    checks are not run.
#' @return An object of class \code{\link{ajlfacimp}}.
#' @importFrom mifa mifa
#' @importFrom mice md.pattern
#' @importFrom purrr map2_dfr
#' @importFrom utils capture.output
#' @importFrom stats setNames predict
#' @importFrom psych predict.psych
#' @importFrom ranger ranger
#' @example inst/examples/factor_imputation_example.R
#' @export
factor_imputation <- function(data,
                              fa_vars = NULL,
                              av_vars = NULL,
                              other_vars = NULL,
                              options = fami_options(fa_type = "fa"),
                              run_checks = TRUE) {

  checkmate::assert_flag(run_checks)

  # ~ fi initial check ------------------------------------------------------

  # check data and print some info to screen:
  report_data(data)

  # Check variable lists and parse
  check_varlists(
    fa = fa_vars,
    av = av_vars,
    other = other_vars,
    cnames = colnames(data)
  )
  vars <- parse_varlists(
    fa = fa_vars,
    av = av_vars,
    other = other_vars,
    cnames = colnames(data)
  )

  # Print some info about the variable roles:
  report_parsed_varlist(vars, type = options$type)

  # Assemble data for factor imputation:
  fa_data <- data[, c(vars$fa, vars$av)]

  # Check there are no completely empty rows:
  check_impute_data(fa_data)

  # report missingness
  report_missingness(fa_data)

  # ~ fi sense checks -------------------------------------------------------


  if (run_checks) {
    sense_problem <- FALSE
    # Will mice complain about imputation?:
    sense_problem <- sensecheck_dryrunmice(status = sense_problem,
                                           options = options,
                                           data = fa_data)
    # Are there too few obs/var for cfa?:
    if ((options$type == "fa")) {
      sense_problem <- sensecheck_fatotalobs(status = sense_problem,
                                             nobs = nrow(fa_data))
      sense_problem <- sensecheck_faobspervar(
        status = sense_problem,
        nvar = length(vars$fa),
        nobs = nrow(fa_data)
      )
    }
    if (sense_problem) {
      cli::cli_abort(
        c("one or more run checks failed.",
          "i" = "Fix data and rerun,",
          "i" = "or (if you're sure) set argument: {.code run_checks = FALSE}")
      )
    }
  }

  # ~ fi imputation ------------------------------------------------------
  # setup imputation call
  mifa_call <- vector(mode = "list", length = 2L + length(options$mice))

  names(mifa_call) <- c("data", "cov_vars", names(options$mice))

  mifa_call$data <- fa_data
  mifa_call$cov_vars <- vars$fa

  for ( i in 3:(2 + length(options$mice)) ) {
    mifa_call[[i]] <- options$mice[[i - 2]]
  }

  # Run the mifa:
  report_runningmifa(options)
  mifa_res <- do.call(mifa::mifa, mifa_call)

  report_varexp(mifa_res)


  # ~ fi number of factors --------------------------------------------------

  # Determine the number of factors:
  report_nfactors(options$nfactors)

  par_res <- NA # if parallel analysis not run.
  if ( options$nfactors == "parallel" ) {
    # run parallel analysis:
    utils::capture.output({
      par_res <- suppressWarnings(
        psych::fa.parallel(
          mifa_res$cov_combined,
          n.obs = NROW(fa_data),
          fm = options$fa$fm,
          fa = ifelse(options$type == "fa", yes = "fa", no = "pc"),
          n.iter = 50,
          sim = TRUE,
          plot = FALSE
        )
      )
    }, file = nullfile())
    # update nfactors:
    if ( options$type == "fa" ) {
      options$nfactors <- par_res$nfact
    } else {
      options$nfactors <- par_res$ncomp
    }
    report_nfactors(options$nfactors)
  }


  # ~ fi factor analysis -------------------------------------------------

  if ( options$type == "fa") {
    fa_function <- psych::"fa"
    fa_opts <- options$fa
  } else {
    fa_function <- psych::"principal"
    fa_opts <- options$pca
  }

  # assemble fa call:
  # common arguments:
  #   r, n.obs, nfactors
  fa_call <- vector(mode = "list", length = 3 + length(fa_opts))

  names(fa_call) <- c("r", "n.obs", "nfactors", names(fa_opts))
  fa_call[["r"]] <- mifa_res$cov_combined
  fa_call[["n.obs"]] <- nrow(fa_data)
  fa_call[["nfactors"]] <- options$nfactors

  for ( i in 4:(3 + length(fa_opts)) ) {
    fa_call[[i]] <- fa_opts[[i - 3]]
  }

  # run the factor analysis:
  report_runningfa(options$nfactors, options$type)

  capture.output({
    fa_res <- suppressWarnings(do.call(fa_function, fa_call))
  }, file = nullfile())

  # ~ fi extract scores -----------------------------------------------------
  # note that 0 is the unimputed data, 1:m the imputed datasets.
  imp_scores <- lapply(
    (0:options$mice$m), #nolint
    \(.x) {
      vars_in_fa <- dimnames(fa_res$Structure)[[1]]
      predict(
        object = fa_res,
        data = mice::complete(mifa_res$mids, .x)[, vars_in_fa]
      )
    })


  # First log methods used by mice:
  meth_log <- mifa_res$mids$method
  meth_add1 <- setNames(rep("", times = options$nfactors),
                        colnames(get_loadings(fa_res)))

  if (length(other_vars) == 0L) {
    # if we don't need to add any "other" variables:
    mifa_res$mids <- mice::as.mids(purrr::map2_dfr(
      0:options$mice$m,
      imp_scores,
      ~ cbind(.imp = .x, mice::complete(mifa_res$mids, .x), .y)
    ))

    mifa_res$mids$method <- c(meth_log, meth_add1)

  } else {
    other_add <- data[, other_vars, drop = FALSE]
    meth_add2 <- setNames(rep("", times = length(other_vars)),
                          other_vars)
    mifa_res$mids <- mice::as.mids(purrr::map2_dfr(
      0:options$mice$m,
      imp_scores,
      ~ cbind(.imp = .x, mice::complete(mifa_res$mids, .x), .y),
      other_add
    ))

    mifa_res$mids$method <- c(meth_log, meth_add1, meth_add2)
  }

  # Return:
  result <- structure(list(
    input = list(
      data = data,
      vars = vars,
      options = options
    ),
    mifa = mifa_res,
    par = par_res,
    fa = fa_res
  ),
  class = "ajlfacimp")
  result
}

# generic methods for the "ajlfacimp" class

#' @exportS3Method
print.ajlfacimp <- function(x, ...) {

  # convoluted, but accounts for missing data passed though via
  #   "other" variables.
  n_missing_orig <- sum(is.na(mice::complete(x$mifa$mids, 0L)))
  n_missing_after <- sum(is.na(mice::complete(x$mifa$mids, 1L)))
  n_imputed <- (n_missing_orig - n_missing_after) /
    (1 + x$input$options$nfactors)
  force(n_imputed)

  cli::cli_inform(c("An object of class ajlfacimp.",
                    "i" = "type: { x$input$options$type }",
                    "i" = "# factors/comps: { x$input$options$nfactors }",
                    "i" = "# factor vars: { length(x$input$vars$fa) }",
                    "i" = "# auxiliary vars: { length(x$input$vars$av) }",
                    "i" = "# other vars: { length(x$input$vars$other) }",
                    "i" = "# observations: { nrow(x$input$data) }",
                    "i" = "# imputed values: { n_imputed }"))
}

#' @exportS3Method
plot.ajlfacimp <- function(x, y, ...) {
  cli::cli_warn("plot method not implemented for ajlfacimp-class.")
}

#' @exportS3Method
summary.ajlfacimp <- function(object, ...) {
  print(object)
}
