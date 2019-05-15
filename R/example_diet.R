#' Simulated diet data to predict weight loss
#'
#' The simulated \code{diet} data contains 100 observations, 14 predictors,
#' and an binary outcome, weightloss. The external information Z is the nutrition fact about the dietary items.
#' Z contains three external information variables: Calories, protein and carbohydrates.
#'
#' @docType data
#'
#' @usage data(diet)
#'
#' @keywords datasets
#'
#' @format The \code{diet} object is a list containing three elements:
#' \itemize{
#' \item DietItems: Matrix of predictors.
#' \item weightloss: 0: no weight loss; 1: weight loss
#' \item nutritionFact: External information of the predictors
#' }
#' @references S. Witte, John & Greenland, Sander & W. Haile, Robert & L. Bird, Cristy. (1994). Hierarchical Regression Analysis Applied to a Study of Multiple Dietary Exposures and Breast Cancer. Epidemiology (Cambridge, Mass.). 5. 612-21. 10.1097/00001648-199411000-00009.
#' @seealso \code{\link{example}}
#' @examples
#' data(diet)
#' X <- diet$DietItems
#' Y <- diet$weightloss
#' Z <- diet$nutritionFact
#' \donttest{fit <- xtune(X,Y,Z)}
#' \donttest{fit$penalty.vector}
"diet"
