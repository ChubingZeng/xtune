#' Model predictions based on fitted \code{xtune} object
#'
#' \code{predict.xtune} produces predicted values fitting an xtune model to a new dataset
#' @param object Fitted 'xtune' model object.
#' @param newX Matrix of values at which predictions are to be made.
#' @param ... Not used
#' @details \code{coef} and \code{predict} methods are provided as a convenience to extract coefficients and make prediction. \code{predict.xtune} simply calculate the predicted value using the estimated coefficients returned by \code{xtune}.
#' @return A vector of predictions
#' @seealso \code{xtune}, \code{coef.xtune}
#' @examples
#' ## simulate data
#' set.seed(9)
#' data(example)
#' X <- example$X
#' Y <- example$Y
#' Z <- example$Z
#'
#' ## If no Z provided, perform Empirical Bayes tuning
#' fit.eb <- xtune(X,Y)
#' ## Coef and predict methods
#' coef(fit.eb)
#' predict(fit.eb,X)
#'
#' ## Differential shrinkage based on external information Z:
#' fit.diff <- xtune(X,Y,Z)
#' ## Coef and predict methods
#' coef(fit.diff)
#' predict(fit.diff,X)

#' @export

predict.xtune <- function(object, newX, ...) {
    if (!(typeof(newX) %in% c("double", "integer"))) {
        stop("New X contains non-numeric values")
    } else if (!is.matrix(newX)) {
            stop("New X is not a matrix")
    } else if (length(object$beta.est[-1]) != ncol(newX)) {
        stop("New X does not have the same number of columns as X train")
    }
    predicted <- object$beta.est[1] + newX %*% object$beta.est[-1]
    return(drop(predicted))
}
