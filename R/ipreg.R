#' L1 and L2 penalized regression with individual amount of shrinkage for each regression coefficient based on external information.
#'
#' \code{ipreg} estimated beta coefficients and pseudo probablity
#' @param X predictor matrix of dimension \eqn{n*q} used for previous regression
#' @param Y continuous outcome vector of dimension \eqn{n} used for previous regression
#' @param Z external information data matrix of dimension \eqn{p*q}
#' @param method method = "Lasso" for Lasso regression, method = "Ridge" for Ridge regression
#' @param sigma.square variance estimation, default is the estimated variance using R package "selectiveinference"
#' @param alpha.init initial value for alpha, default is rep(1,p)
#' @param maxstep maximal step of iterations
#' @param margin stoping creteria for converge
#' @param maxstep_inner maximum inner loop step
#' @param tol.inner stopping creteria for inner loop
#' @param compute.likelihood compute likelihood or not
#' @param verbosity track update process or not
#' @import glmnet
#' @importFrom stats optim
#' @export

ipreg <- function(X,Y,Z = NULL,sigma.square = NULL,method = c("lasso","ridge"),
                  control = list()){

        # function call
        this.call <- match.call()

        # check error distribution for y
        method = match.arg(method)

        # check user inputs
        ## Check X, X need to be a matrix or data.frame
        np = dim(X)
        if (is.null(np) | (np[2] <= 1))
                stop("X must be a matrix with 2 or more columns")

        nobs = as.integer(np[1]); nvar = as.integer(np[2])

        ## Check Y
        Y <- as.double(drop(Y))
        dimY = dim(Y)
        nrowY = ifelse(is.null(dimY), length(Y), dimY[1])
        if (nrowY != nobs)
                stop(paste("number of observations in Y (", nrowY, ") not equal to the number of rows of X (",
                           nobs, ")", sep = ""))

        ## Check sigma.square
        if (is.null(sigma.square)){
                cat("Estimating sigma square")
                sigma.square = estimateVariance(X,Y)
        } else if (! is.double(sigma.square) | is.infinite(sigma.square) | sigma.square <= 0){
                stop("sigma square should be a positive finite number")
        }
        else{
                sigma.square = sigma.square
        }

        # Check method
        if (! method %in% c("lasso","ridge")) {
                warning("Method not lasso or ridge; set to lasso")
                method = "lasso"
        }

        # Check Z
        #### If no Z provided, then provide Z of a single column of 1
        if (is.null(Z)){
                cat("No Z matrix provided, only a single tuning parameter will be estimated using empirical Bayes tuning")
                dat_ext = as.matrix(rep(1,nvar))
        } else {
                #### If Z is provided:
                dimZ = dim(Z)
                nrowZ = ifelse(is.null(dimZ), length(Z), dimZ[1])
                ncolZ = ifelse(is.null(dimZ), 1, dimZ[2])

                if (nrowZ != nvar){ ## check the dimension of Z
                        stop(paste("number of rows in Z (", nrow(Z),
                                   ") not equal to the number of columns in X (", nvar,
                                   ")", sep = ""))
                } else if (!is.matrix(Z)) { ## check is Z is a matrix
                        Z = as.matrix(Z)
                } else if (!(typeof(Z) %in% c("double", "integer"))) {
                        stop("Error: external contains non-numeric values")
                } else if (all(apply(Z, 2, function(x) length(unique(x)) == 1) == TRUE)){ ## check if all rows in Z are the same
                        warning("All rows in Z are the same, this Z matrix is not useful, EB tuning will be performed to estimate
                                a single tuning parameter")
                        dat_ext = as.matrix(rep(1,nvar))
                } else if (! identical(Z[,1],rep(1,nvar))) { ## if no column of one is appended then append a column of 1s
                        dat_ext = cbind(1,Z)
                } else{
                        dat_ext = Z
                }
        }

        nex = ncol(dat_ext)

        # check control object
        control <- do.call("ipreg.control", control)
        control <- initialize_control(control,dat_ext)

        if (nex > 1){
                cat("Z provided, start estimating individual tuning parameters")
        }

        # core function
        fit <- ipreg.fit(X = X,
                         Y = Y,
                         Z = dat_ext,
                         method = method,
                         sigma.square = sigma.square,
                         alpha.init = control$alpha.init,
                         maxstep = control$maxstep,
                         tolerance = control$tolerance,
                         maxstep_inner = control$maxstep_inner,
                         tolerance_inner = control$tolerance_inner,
                         compute.likelihood = control$compute.likelihood,
                         verbosity = control$verbosity)

        # Check status of model fit
        if (length(fit$tuningvector) == 1){
                fit$tuningvector = unique(fit$tuningvector)
        }

        fit$call <- this.call
        return(fit)
}

ipreg.control <- function(alpha.init = NULL,
                          maxstep = 100,
                          tolerance = 0.001,
                          maxstep_inner = 50,
                          tolerance_inner = 0.1,
                          compute.likelihood = TRUE,
                          verbosity = FALSE) {

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

        control_obj <- list(alpha.init = alpha.init,
                            maxstep = maxstep,
                            tolerance = tolerance,
                            maxstep_inner = maxstep_inner,
                            tolerance_inner = tolerance_inner,
                            compute.likelihood = compute.likelihood,
                            verbosity = verbosity)
}

initialize_control <- function(control_obj,Z){
        if (is.null(alpha.init) | all(apply(Z, 2, function(x) length(unique(x)) == 1) == TRUE)) {
                alpha.init = rep(0, nex)
        } else if (length(alpha.init) != ncol(Z)){
                warning(cat(paste("number of elements in alpha initial values (", length(alpha.init),
                                  ") not equal to the number of columns of Z (", q,
                                  ")",", alpha initial set to be all 0", sep = "")))
        } else{
                alpha.init = alpha.init
        }
        control_obj$alpha.init <- alpha.init
        return(control_obj)
}

