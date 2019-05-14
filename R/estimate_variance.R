#' Estimate noise variance given predictor X and response Y.
#'
#' \code{estimateVariance} estimate noise variance.
#' @param X predictor matrix of dimension \eqn{n} by \eqn{p}.
#' @param Y continuous outcome vector of length \eqn{n}.
#' @param n_rep number of repeated estimation. Default is 10.
#' @details The \code{estimateSigma} function from \link{selectiveInference} is used repeatedly to estimate noise variance.
#' @references Stephen Reid, Jerome Friedman, and Rob Tibshirani (2014). A study of error variance estimation in lasso regression. arXiv:1311.5274.
#' @seealso \link{selectiveInference}
#' @examples
#' ## simulate some data
#' set.seed(9)
#' n = 30
#' p = 10
#' sigma.square = 1
#' X = matrix(rnorm(n*p),n,p)
#' beta = c(2,-2,1,-1,rep(0,p-4))
#' Y = X%*%beta + rnorm(n,0,sqrt(sigma.square))
#'
#' ## estimate sigma square
#' sigma.square.est = estimateVariance(X,Y)
#' sigma.square.est
#' @importFrom selectiveInference estimateSigma
#' @export

estimateVariance <- function(X, Y, n_rep = 5) {
        Y <- as.double(drop(Y))
        dimY = dim(Y)
        nrowY = ifelse(is.null(dimY), length(Y), dimY[1])
        if (nrowY < 10) {
        stop("Need at least 10 observations to estimate variance")
        }

        temp = array(NA, n_rep)
        for (i in 1:n_rep) {
        c = suppressWarnings(estimateSigma(X, Y)$sigmahat^2)
        temp[i] = ifelse(is.infinite(c), NA, c)
        }
        return(mean(temp, na.rm = T))
}
