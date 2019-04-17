#' Predict model for integrated penalized regression model fit
#'
#' \code{predict.ipreg} produces predicted values fitting an ipreg model to a new dataset
#' @param object Fitted 'ipreg' model object.
#' @param newX Matrix of values at which predictions are to be made.
#' @param ... Not used
#' @export

predict.ipreg <- function(object, newX, ...) {
    if (!(typeof(newX) %in% c("double", "integer"))) {
        stop("New X contains non-numeric values")
    } else if (length(object$beta.est[-1]) != ncol(newX)) {
        stop("New X does not have the same number of columns as X train")
    }
    predicted <- object$beta.est[1] + newX %*% object$beta.est[-1]
    return(drop(predicted))
}
