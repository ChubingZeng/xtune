#' Estimate Variance given predictor X and response Y.
#'
#' \code{estimateVariance} estimate noise variance
#' @param X predictor matrix of dimension \eqn{n*q} used for previous regression
#' @param Y continuous outcome vector of dimension \eqn{n} used for previous regression
#' @param num number of repeated estimation
#' @importFrom selectiveInference estimateSigma
#' @export

estimateVariance<-function(X,Y,num = 10) {
        #options(warn = -1)
        temp = array(NA,num)
        for (i in 1:num){
                c = estimateSigma(X,Y)$sigmahat^2
                temp[i] = ifelse(is.infinite(c),NA,c)
        }
        return(mean(temp,na.rm =T))
}
