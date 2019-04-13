#' Estimate Variance given predictor X and response Y.
#'
#' \code{estimateVariance} estimate noise variance
#' @param X predictor matrix of dimension \eqn{n*q} used for previous regression
#' @param Y continuous outcome vector of dimension \eqn{n} used for previous regression
#' @param num number of repeated estimation
#' @importFrom selectiveInference estimateSigma
#' @export

estimateVariance<-function(X,Y,num = 10) {
        Y <- as.double(drop(Y))
        dimY = dim(Y)
        nrowY = ifelse(is.null(dimY), length(Y), dimY[1])
        if (nrowY < 10){
                stop("Need at least 10 observations to estimate variance")
        }

        temp = array(NA,num)
        for (i in 1:num){
                c = suppressWarnings(estimateSigma(X,Y)$sigmahat^2)
                temp[i] = ifelse(is.infinite(c),NA,c)
        }
        return(mean(temp,na.rm =T))
}
