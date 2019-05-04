#' Extract model coefficients from fitted \code{xtune} object
#'
#' \code{coef.xtune} extracts model coefficients from objects returned by \code{xtune} object.
#' @param object Fitted 'xtune' model object.
#' @param ... Not used
#' @details \code{coef} and \code{predict} methods are provided as a convenience to extract coefficients and make prediction. \code{coef.xtune} simply extracts the estimated coefficients returned by \code{xtune}.
#' @return Coefficients extracted from the fitted model.
#' @seealso \code{xtune}, \code{predict.xtune}
#' @examples
#' ## see examples in predict.xtune()
#' @export

coef.xtune <- function(object,...) {
        beta.est <- object$beta.est
        return(drop(beta.est))
}
