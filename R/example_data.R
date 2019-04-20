#' An simulated example dataset
#'
#' The simulated \code{example} data contains 100 observations, 200 predictors, and an continuous outcome. Z contains 3 columns, each column is
#' indicator variable (can be viewed as the grouping of predictors).
#'
#' @docType data
#'
#' @usage data(example)
#'
#' @keywords datasets
#'
#' @format The \code{example} object is a list containing three elements:
#' \itemize{
#' \item X: A simulated 100 by 200 matrix
#' \item Y: Continuous response vector of length 100
#' \item Z: A 200 by 3 matrix. Z_jk indicates whether predictor X_j has external variable Z_k or not.
#' }
#'
#' @examples
#' data(example)
#' X <- example$X
#' Y <- example$Y
#' Z <- example$Z
#' \donttest{ipreg(X,Y,Z)}
"example"
