#' ajlfacimp object
#'
#' Objects of class \code{"ajlfacimp"} are returned by [factor_imputation()].
#' They contain the results of the analysis along with metadata.
#' The key element is `mifa$mids` a mice "mids" object containing imputed data
#' with factor scores merged back in for downstream analysis.
#'
#' @section Components:
#' \describe{
#'   \item{input}{The original input data, variable roles, and options}
#'   \item{mifa}{The multiple imputation factor analysis object
#'       with pooled covariance matrix}
#'   \item{par}{Optional. If requested the fa.parallel analysis
#'       used for determining nfactors.}
#'   \item{fa}{The factor analysis run on pooled covariance.}
#' }
#'
#' @name ajlfacimp
#' @aliases ajlfacimp-class
#' @docType class
NULL
