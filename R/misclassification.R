#' Calculate misclassification error
#'
#' \code{misclassification} calculate misclassification error between predicted class and true class
#' @param pred Predicted class
#' @param true Actual class
#' @return misclassification error
#' @examples
#' Y1 <- rbinom(10,1,0.5)
#' Y2 <- rnorm(10,1,0.5)
#' misclassification(Y1,Y2)
#' @export

misclassification <- function(pred, true) {
        n_pred = ifelse(is.null(dim(pred)), length(pred), dim(pred)[1])
        n_true = ifelse(is.null(dim(true)), length(true), dim(true)[1])

        if (length(pred) != length(true)) {
                stop("The length of prediction class and actual class does not match")
        } else if (sum(is.na(pred)) > 0) {
                warning("Prediction class contain missing value")
        } else if (sum(is.na(true)) > 0) {
                warning("Actual class contain missing value")
        }

        return(mean(pred != true, na.rm = T))
}
