#' Customized LASSO using iterative reweighted l2
#'
#' \code{cus_ridge} estimated beta coefficients and pseudo probablity
#' @param X predictor matrix of dimension \eqn{n*q} used for previous regression
#' @param y continuous outcome vector of dimension \eqn{n} used for previous regression
#' @param beta_hat estimated beta coefficients from regression (no intercept term)
#' @param x_predict data for prediction (no column of 1 needed)
#' @return Returns the estimated beta coefficients for classification and pseudo probablity
#' @param Y continuous outcome vector of dimension \eqn{p}.
#' @param Z external information data matrix of dimension \eqn{p*q}
#' @param sigma.square variance estimation, default is the estimated variance using R package "selectiveinference"
#' @param alpha.init initial value for alpha, default is rep(1,p)
#' @param delta.init initial value for delta, the auxilary function for updating alpha
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

cus_ridge<- function(X,Y,Z,sigma.square = estimateVariance(X,Y),
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
                gamma = exp(-Z%*%alpha.old) ## gamma is the variance of beta in this vase
                Sigma_y=sigma.square * diag(n) +(t(t(X)*c(gamma)))%*%t(X)
                theta = colSums(X*solve(Sigma_y,X))

                # Compute likelihood
                if (compute.likelihood == TRUE){
                        likelihood.score = c(likelihood.score,approx_likelihood.ridge(alpha.old,X,Y,Z,sigma.square))
                }

                # Given theta, update alpha
                update.result <-update_alpha.ridge(X,Y,Z,alpha.old = alpha.old,sigma.square = sigma.square,theta = theta,maxstep_inner = maxstep_inner,tol.inner = tol.inner)
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
        gamma = exp(-Z%*%alpha.old)
        C=sum(1/gamma)/p*sigma.square/n
        coef.cusRidge <- coef(glmnet(X,Y,alpha=0, penalty.factor = 1/gamma,lambda = C))
        return(list(coef.cusRidge = coef.cusRidge,alpha.hat = alpha.old,n_iter = k-1,sigma.square = sigma.square,likelihood.score = likelihood.score))
}

