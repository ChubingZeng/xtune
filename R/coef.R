#' Extract model coefficients from fitted \code{ipreg} object
#'
#' \code{coef.ipreg} extracts model coefficients from objects returned by \code{ipreg} object.
#' @param object Fitted 'ipreg' model object.
#' @param ... Not used
#' @details \code{coef} and \code{predict} methods are provided as a convenience to extract coefficients and make prediction. \code{coef.ipreg} simply extracts the estimated coefficients returned by \code{ipreg}.
#' @return Coefficients extracted from the fitted model.
#' @seealso \code{ipreg}, \code{predict.ipreg}
#' @examples
#' ## see examples in predict.ipreg()
#' @export

coef.ipreg <- function(object,...) {
        beta.est <- object$beta.est
        return(drop(beta.est))
}
