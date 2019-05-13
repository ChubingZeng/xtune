#' Simulated gene data to predict weight loss
#'
#' The simulated \code{gene} data contains 50 observations, 200 predictors,
#' and an continuous outcome, bone mineral density. The external information Z is four previous study results that identifies the biological importance of genes.
#'
#' @docType data
#'
#' @usage data(gene)
#'
#' @keywords datasets
#'
#' @format The \code{gene} object is a list containing three elements:
#' \itemize{
#' \item GeneExpression: Matrix of gene expression predictors.
#' \item bonedensity: Continuous outcome variable
#' \item PreviousStudy: Whether each gene is identified by previous study results.
#' }
#' @seealso \code{\link{diet}}
#' @examples
#' data(gene)
#' X <- gene$geneItems
#' Y <- gene$weightloss
#' Z <- gene$NuitritionFact
#' \donttest{fit <- xtune(X,Y,Z)}
#' \donttest{fit$penalty.vector}
"gene"
