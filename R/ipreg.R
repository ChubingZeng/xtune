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

ipreg <- function(X,Y,Z,method = c("lasso","ridge"),sigma.square,
                   alpha.init,
                   maxstep = 100,
                   margin = 0.001,
                   maxstep_inner = 50,
                   tol.inner = 0.01,
                   compute.likelihood = FALSE,
                   verbosity = 0){

        #method = match.arg(method)

        #this.call = match.call()

        ## checking user inputs
        ### Check X
        np = dim(X)
        if (is.null(np) | (np[2] <= 1))
                stop("x should be a matrix with 2 or more columns")
        nobs = as.integer(np[1]); nvar = as.integer(np[2])

        ### Check Y
        dimY = dim(Y)
        nrowY = ifelse(is.null(dimY), length(Y), dimY[1])
        if (nrowY != nobs)
                stop(paste("number of observations in Y (", nrowY, ") not equal to the number of rows of X (",
                           nobs, ")", sep = ""))

        ### Check sigma.square
        if (missing(sigma.square)){
                cat("Estimating sigma square")
                sigma.square = estimateVariance(X,Y)
        } else if (! is.double(sigma.square)){
                stop("sigma square is not a number")
        } else if (is.infinite(sigma.square)){
                message("sigma square is infinite, estimated sigma square will be used")
                sigma.square = estimateVariance(X,Y)
        } else if (sigma.square <= 0){
                message("sigma square is not positive, estimated sigma square will be used")
                sigma.square = estimateVariance(X,Y)
        }
        else{
                sigma.square = sigma.square
        }

        ### Check method
        if (! method %in% c("lasso","ridge")) {
                warning("Method not lasso or ridge; set to lasso")
                method = "lasso"
        }

        ### Check Z
        #### If no Z provided, then provide Z of a single column of 1
        if (missing(Z)){
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

        ### Check alpha initial values
        if (missing(alpha.init) | all(apply(dat_ext, 2, function(x) length(unique(x)) == 1) == TRUE)) {
                alpha.init = rep(0, nex)
        } else if (length(alpha.init) != ncol(dat_ext)){
                warning(cat(paste("number of elements in alpha initial values (", length(alpha.init),
                           ") not equal to the number of columns of Z (", q,
                           ")",", alpha initial set to be all 0",sep = "")))
        } else{
                alpha.init = alpha.init
        }

        ### core function
        if (nex > 1){
                cat("Z provided, start estimating individual tuning parameters")
        }

        fit <- ipreg.fit(X,Y,dat_ext,method = method,
                            sigma.square = sigma.square,
                            alpha.init = alpha.init,
                            maxstep = maxstep,
                            margin = margin,
                            maxstep_inner = maxstep_inner,
                            tol.inner = tol.inner,
                            compute.likelihood = compute.likelihood,
                            verbosity = verbosity)

        return(list(coefest = fit$coefest, tuningvector = fit$tuningvector,
                    alpha.hat = fit$alpha.hat,n_iter = fit$n_iter,
                    sigma.square = sigma.square,likelihood.score = fit$likelihood.score))
}
