#' Tuning differential shrinkage parameters in penalized regression based on external information.
#'
#' \code{xtune} uses an Empirical Bayes approach to integrate external information into penalized linear regression models. It fits models with differential amount of shrinkages for each regression coefficient based on external information.
#' @param X Numeric design matrix of explanatory variables (\eqn{n} observations in rows, \eqn{p} predictors in columns), without an intercept. \code{xtune} includes an intercept by default.
#' @param Y Outcome vector of dimension \eqn{n}. Quantitative for family="linear", or family="binary" for a 0/1 binary outcome variable.
#' @param Z Numeric information matrix about the predictors (\eqn{p} rows, each corresponding to a predictor in X; \eqn{q} columns of external information about the predictors, such as prior biological importance). If Z is the grouping of predictors, it is best if user codes it as a dummy variable (i.e. each column indicating whether predictors belong to a specific group)
#' @param family Response type. "linear" for continuous outcome, "binary" for 0/1 binary outcome.
#' @param sigma.square A user-supplied noise variance estimate. Typically, this is left unspecified, and the function automatically computes an estimated sigma square values using R package \code{selectiveinference}.
#' @param method The type of regularization applied in the model. method = 'lasso' for Lasso regression, method = 'ridge' for Ridge regression
#' @param message Generates diagnostic message in model fitting. Default is TRUE.
#' @param control Specifies \code{xtune} control object. See \code{\link{xtune.control}} for more details.
#' @details \code{xtune} has two main usages:
#' \itemize{
#' \item The basic usage of it is to choose the tuning parameter \eqn{\lambda} in Lasso and Ridge regression using an
#' Empirical Bayes approach, as an alternative to the widely-used cross-validation. This is done by calling \code{xtune} without specifying external information matrix Z.
#'
#' \item More importantly, if an external information Z about the predictors X is provided, \code{xtune} can allow differential shrinkage
#' parameters for regression coefficients in penalized regression models. The idea is that Z might be informative for the effect-size of regression coefficients, therefore we can guide the penalized regression model using Z.
#' }
#'
#' Please note that the number of rows in Z should match with the number of columns in X. Since each column in Z is a feature about X. \href{https://github.com/ChubingZeng/xtune}{See here for more details on how to specify Z}.
#'
#' A majorization-minimization procedure is employed to fit \code{xtune}.
#' @return An object with S3 class \code{xtune} containing:
#' \item{beta.est}{The fitted vector of coefficients.}
#' \item{penalty.vector}{The estimated penalty vector applied to each regression coefficient. Similar to the \code{penalty.factor} argument in \link{glmnet}.}
#' \item{lambda}{The estimated \eqn{\lambda} value. Note that the lambda value is calculated to reflect that the fact that penalty factors are internally rescaled to sum to nvars in \link{glmnet}. Similar to the \code{lambda} argument in \link{glmnet}.}
#' \item{n_iter}{Number of iterations used until convergence.}
#' \item{method}{Same as in argument above}
#' \item{sigma.square}{The estimated sigma square value using \code{\link{estimateVariance}}, if \code{sigma.square} is left unspecified.}
#' \item{family}{same as above}
#' \item{likelihood}{A vector containing the marginal likelihood value of the fitted model at each iteration.}
#' @author Chubing Zeng
#' @seealso \link{predict.xtune}, as well as \link{glmnet}.
#' @examples
#' ## use simulated example data
#' set.seed(9)
#' data(example)
#' X <- example$X
#' Y <- example$Y
#' Z <- example$Z
#'
#' ## Empirical Bayes tuning to estimate tuning parameter, as an alternative to cross-validation:
#' fit.eb <- xtune(X,Y)
#' fit.eb$lambda
#'
#' ### compare with tuning parameter choosen by cross-validation, using glmnet
#' \dontrun{
#' fit.cv <- cv.glmnet(X,Y,alpha = 1)
#' fit.cv$lambda.min
#'}
#' ## Differential shrinkage based on external information Z:
#' fit.diff <- xtune(X,Y,Z)
#' fit.diff$penalty.vector
#'
#' @import glmnet
#' @importFrom stats optim
#' @export

xtune <- function(X, Y, Z = NULL,family=c("linear","binary"), sigma.square = NULL, method = c("lasso", "ridge"), message = TRUE,
                  control = list()) {

        # function call
        this.call <- match.call()

        method = match.arg(method)
        family = match.arg(family)

        # check user inputs Check X, X need to be a matrix or data.frame
        np = dim(X)
        if (is.null(np) | (np[2] <= 1))
                stop("X must be a matrix with 2 or more columns")

        nobs = as.integer(np[1])
        nvar = as.integer(np[2])

        ## Check Y
        if (!is.numeric(Y) & !is.factor(Y)){
                stop("Y must be a quantitive vector or a 0/1 binary factor variable")
        } else if (is.factor(Y)){
                if(sum(!(levels(Y) %in% c("0","1")))!=0){
                        stop("Y is not 0/1 binary variable")
                }
                family = "binary"
        } else if (length(unique(Y)) == 2){
                if (sum(unique(Y))!=1){
                        stop("Y only has two unique values, but they are not 0/1")
                }
                family = "binary"
        }

        ## Change family to binary if Y only has two levels
        Y <- as.double(drop(Y))
        dimY = dim(Y)
        nrowY = ifelse(is.null(dimY), length(Y), dimY[1])
        if (nrowY != nobs)
                stop(paste("number of observations in Y (", nrowY, ") not equal to the number of rows of X (",
                           nobs, ")", sep = ""))

        # Check Z If no Z provided, then provide Z of a single column of 1
        if (is.null(Z)) {
                if (message == TRUE){
                        cat("No Z matrix provided, only a single tuning parameter will be estimated using empirical Bayes tuning","\n")
                }
                dat_ext = matrix(rep(1, nvar))
        } else {
                #### If Z is provided:
                dimZ = dim(Z)
                nrowZ = ifelse(is.null(dimZ), length(Z), dimZ[1])
                ncolZ = ifelse(is.null(dimZ), 1, dimZ[2])

                if (nrowZ != nvar) {
                        ## check the dimension of Z
                        stop(paste("number of rows in Z (", nrow(Z), ") not equal to the number of columns in X (",
                                   nvar, ")", sep = ""))
                } else if (!is.matrix(Z)) {
                        ## check is Z is a matrix
                        Z = as.matrix(Z)
                        dat_ext <- Z
                } else if (!(typeof(Z) %in% c("double", "integer"))) {
                        stop("Z contains non-numeric values")
                } else if (all(apply(Z, 2, function(x) length(unique(x)) == 1) == TRUE)) {
                        ## check if all rows in Z are the same
                        warning(paste("All rows in Z are the same, this Z matrix is not useful, EB tuning will be performed to estimate
                                      a single tuning parameter","\n"))
                        dat_ext = matrix(rep(1, nvar))
                } else {
                        dat_ext <- Z
                }
        }

        if (!identical(dat_ext[, 1], rep(1, nvar)) & ncolZ != nvar) {
                ## if no column of one is appended then append a column of 1s
                dat_ext = cbind(1, dat_ext)
        }

        drop(Z)
        nex = ncol(dat_ext)

        ## Check sigma.square
        if (is.null(sigma.square)) {
                sigma.square = estimateVariance(X, Y)
        } else if (!is.double(sigma.square) | is.infinite(sigma.square) | sigma.square <=
                   0) {
                stop("sigma square should be a positive finite number")
        } else {
                sigma.square = sigma.square
        }

        # Check method
        if (!method %in% c("lasso", "ridge")) {
                warning("Method not lasso or ridge; set to lasso")
                method = "lasso"
        }

        # check control object
        control <- do.call("xtune.control", control)
        control <- initialize_control(control, dat_ext)

        if (nex > 1) {
                if (message == TRUE){
                        cat(paste("Z provided, start estimating individual tuning parameters","\n"))
                }
        }

        # core function
        fit <- xtune.fit(X = X, Y = Y, Z = dat_ext, method = method, sigma.square = sigma.square,
                         alpha.init = control$alpha.init, maxstep = control$maxstep, tolerance = control$tolerance,
                         maxstep_inner = control$maxstep_inner, tolerance_inner = control$tolerance_inner,
                         compute.likelihood = control$compute.likelihood, verbosity = control$verbosity,
                         standardize = control$standardize, intercept = control$intercept)

        # Check status of model fit
        if (length(unique(fit$penalty.vector)) == 1) {
                fit$penalty.vector = rep(1,nvar)
        }

        fit$family = family
        return(structure(fit, class = "xtune"))
}

#' Control function for xtune fitting
#'
#' @description Control function for \code{\link{xtune}} fitting.
#' @param alpha.init initial values of alpha vector supplied to the algorithm.
#' alpha values are the hyper-parameters for the double exponential prior of regression coefficients,
#'  and it controls the prior variance of regression coefficients. Default is a vector of 0 with length p.
#' @param maxstep Maximum number of iterations. Default is 100.
#' @param tolerance Convergence threshhold. Default is 1e-4.
#' @param maxstep_inner Maximum number of iterations for the inner loop of the majorization-minimization algorithm.
#' @param tolerance_inner Convergence threshhold for the inner loop of the majorization-minimization algorithm.
#' @param compute.likelihood Should the function compute the marginal likelihood for hyper-parameters at each step of update? Default is TRUE.
#' @param verbosity Track algorithm update process? Default is FALSE.
#' @param standardize Standardize X or not, same as the standardize option in glmnet
#' @param intercept Should intercept(s) be fitted (default=TRUE) or set to zero (FALSE), same as the intercept option in glmnet
#' @export


xtune.control <- function(alpha.init = NULL, maxstep = 100, tolerance = 0.001,
                          maxstep_inner = 50, tolerance_inner = 0.1, compute.likelihood = FALSE, verbosity = FALSE,
                          standardize = TRUE, intercept = TRUE) {

        if (maxstep < 0) {
                stop("Error: max out loop step must be a postive integer")
        }

        if (tolerance < 0) {
                stop("Error: outer loop tolerance must be greater than 0")
        }

        if (maxstep_inner < 0) {
                stop("Error: max out loop step must be a postive integer")
        }

        if (tolerance_inner < 0) {
                stop("Error: outer loop tolerance must be greater than 0")
        }

        if (!is.logical(compute.likelihood)) {
                stop("Error: compute.likelihood should be either TRUE or FALSE")
        }

        if (!is.logical(verbosity)) {
                stop("Error: verbosity should be either TRUE or FALSE")
        }

        control_obj <- list(alpha.init = alpha.init, maxstep = maxstep, tolerance = tolerance,
                            maxstep_inner = maxstep_inner, tolerance_inner = tolerance_inner, compute.likelihood = compute.likelihood,
                            verbosity = verbosity, standardize = standardize, intercept = intercept)
}

initialize_control <- function(control_obj, ext) {
        if (is.null(control_obj$alpha.init) | all(apply(ext, 2, function(x) length(unique(x)) ==
                                                        1) == TRUE)) {
                alpha.init = rep(0, ncol(ext))
        } else if (length(control_obj$alpha.init) != ncol(ext)) {
                warning(cat(paste("number of elements in alpha initial values (", length(alpha.init),
                                  ") not equal to the number of columns of ext (", q, ")", ", alpha initial set to be all 0",
                                  sep = "")))
        } else {
                alpha.init = control_obj$alpha.init
        }
        control_obj$alpha.init <- alpha.init
        return(control_obj)
}

