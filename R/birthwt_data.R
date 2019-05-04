#' Risk Factors Associated with Low Infant Birth Weight
#'
#' The \code{databirthwt} data contains 189 observations, 9 predictors, and an continuous outcome, birthweight.
#' It is extracted from the birthwt data in the MASS package. The age and lwt variables are rescaled.
#'
#' @docType data
#'
#' @usage data(databirthwt)
#'
#' @keywords datasets
#'
#' @format The \code{databirthwt} object is a list containing three elements:
#' \itemize{
#' \item X: Matrix of predictors.
#' \item bwt: Birth weight in kilograms
#' \item Z: A non-informative external data matrix with each column representing a predictor. A seperate tuning parameter will be estimated for each regression coefficient using this Z.
#' }
#' @source MASS. R package. \url{https://cran.r-project.org/web/packages/MASS/index.html}
#' @references Hosmer, D.W. and Lemeshow, S. (1989) Applied Logistic Regression. New York: Wiley
#' @seealso \code{\link{birthwt}}
#' @examples
#' data(databirthwt)
#' X <- databirthwt$X
#' Y <- databirthwt$bwt
#' Z <- databirthwt$Z
#' \donttest{fit <- xtune(X,Y,Z)}
#' \donttest{fit$penalty.vector}
"databirthwt"
