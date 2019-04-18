#' Calculate mean square error
#'
#' \code{mse} calculate mean square error (MSE) between prediction values and true values
#' @param pred Prediction values vector
#' @param true Actual values vector
#' @return mean square error
#' @examples
#' Y1 <- rnorm(10,0,1)
#' Y2 <- rnorm(10,0,1)
#' mse(Y1,Y2)
#' @export

mse <- function(pred, true) {
    n_pred = ifelse(is.null(dim(pred)), length(pred), dim(pred)[1])
    n_true = ifelse(is.null(dim(true)), length(true), dim(true)[1])

    if (length(pred) != length(true)) {
        stop("The length of prediction values and actual values does not match")
    } else if (sum(is.na(pred)) > 0) {
        warning("Prediction values contain missing value")
    } else if (sum(is.na(true)) > 0) {
        warning("Actual values contain missing value")
    }

    return(mean((as.vector(pred) - as.vector(true))^2, na.rm = T))
}
