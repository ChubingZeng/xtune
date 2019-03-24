#' Customized LASSO Regression
#'
#' \code{cus_lasso} estimated beta coefficients and pseudo probablity
#' @param X predictor matrix of dimension \eqn{n*q} used for previous regression
#' @param Y continuous outcome vector of dimension \eqn{n} used for previous regression
#' @param Z external information data matrix of dimension \eqn{p*q}
#' @param sigma.square variance estimation, default is the estimated variance using R package "selectiveinference"
#' @param alpha.init initial value for alpha, default is rep(1,p)
#' @param maxstep maximal step of iterations
#' @param margin stoping creteria for converge
#' @param maxstep_inner maximum inner loop step
#' @param tol.delta stopping creteria for inner loop
#' @param compute.likelihood compute likelihood or not
#' @param verbosity track update process or not
#' @import glmnet
#' @import crayon
#' @importFrom stats optim
#' @export

cus_lasso <- function(X,Y,Z,sigma.square = estimateVariance(X,Y),
                      alpha.init = rep(0,ncol(Z)),
                      maxstep = 100,
                      margin = 0.001,
                      maxstep_inner = 50,
                      tol.inner = 0.1,
                      compute.likelihood = FALSE,
                      verbosity = 1){
        n = nrow(X);p=ncol(X);q = ncol(Z)
        ## Initialize
        alpha.old = alpha.init
        k = 1
        likelihood.score = c()
        while(k < maxstep){
                # Given alpha, update theta
                gamma = 2*exp(-2*Z%*%alpha.old)
                Sigma_y=sigma.square * diag(n) +(t(t(X)*c(gamma)))%*%t(X)
                theta = colSums(X*solve(Sigma_y,X))

                # Compute likelihood
                if (compute.likelihood == TRUE){
                        likelihood.score = c(likelihood.score,approx_likelihood.lasso(alpha.old,X,Y,Z,sigma.square))
                }

                # Given theta, update alpha
                update.result <-update_alpha.lasso(X,Y,Z,alpha.old = alpha.old,sigma.square = sigma.square,theta = theta,maxstep_inner = maxstep_inner,tol.inner = tol.inner)
                alpha.new <- update.result$alpha.est

                # Check convergence
                if(sum(abs(alpha.new - alpha.old)) < margin ){
                        cat(red$bold("Done!\n"))
                        break
                }
                alpha.old <- alpha.new

                # Track iteration progress
                if (verbosity == 1){
                        cat(blue$italic("#-----------------"),magenta("Iteration",k,"Done"),blue$italic("-----------------#\n"),sep = "")
                }
                k <- k+1
        }
        tauEst = exp(Z%*%alpha.old)
        pen_vec= tauEst*sigma.square/n
        C= sum(pen_vec)/p
        coef.cusLasso = coef(glmnet(X,Y,alpha = 1, lambda = C, penalty.factor = pen_vec))

        return(list(coef.cusLasso = coef.cusLasso,alpha.hat = alpha.old,n_iter = k-1,sigma.square = sigma.square,likelihood.score = likelihood.score))
}
