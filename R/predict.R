#' Model predictions based on fitted \code{ipreg} object
#'
#' \code{predict.ipreg} produces predicted values fitting an ipreg model to a new dataset
#' @param object Fitted 'ipreg' model object.
#' @param newX Matrix of values at which predictions are to be made.
#' @param ... Not used
#' @details \code{coef} and \code{predict} methods are provided as a convenience to extract coefficients and make prediction. \code{predict.ipreg} simply calculate the predicted value using the estimated coefficients returned by \code{ipreg}.
#' @return A vector of predictions
#' @seealso \code{ipreg}, \code{coef.ipreg}
#' @examples
#' ## simulate data
#' set.seed(9)
#' n = 50
#' p = 200
#' sigma.square = 1
#' X = matrix(rnorm(n*p),n,p)
#' beta = c(2,-2,1,-1,0.5,-0.5,rep(0,p-6))
#' Z = matrix(0,ncol= 3,nrow = p); Z[1:2,1] <- 1; Z[3:4,2] <- 1; Z[5:6,3] <- 1
#' Y = X%*%beta + rnorm(n,0,sqrt(sigma.square))
#'
#' ## If no Z provided, perform Empirical Bayes tuning
#' fit.eb <- ipreg(X,Y)
#' ## Coef and predict methods
#' coef(fit.eb)
#' predict(fit.eb,X)
#'
#' ## Differential shrinkage based on external information Z:
#' fit.diff <- ipreg(X,Y,Z)
#' ## Coef and predict methods
#' coef(fit.diff)
#' predict(fit.diff,X)

#' @export

predict.ipreg <- function(object, newX, ...) {
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
