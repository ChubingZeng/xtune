#' Customized LASSO using iterative reweighted l2
#'
#' \code{CusLasso.l2} estimated beta coefficients and pseudo probablity
#' @param X predictor matrix of dimension \eqn{n*q} used for previous regression 
#' @param y continuous outcome vector of dimension \eqn{n} used for previous regression
#' @param beta_hat estimated beta coefficients from regression (no intercept term)
#' @param x_predict data for prediction (no column of 1 needed)
#' @return Returns the estimated beta coefficients for classification and pseudo probablity
#' @param X predictor matrix of dimension \eqn{n*q}.
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
#' @examples
#' CusLassol2.result = CusLasso.l2(x_train,y_train,Z,sigma.square = sig.est,compute.likelihood = T)

CusLasso.l2 <- function(X,Y,Z,sigma.square = estimateVariance(X,Y),
                        alpha.init = rep(1,ncol(Z)),
                        delta.init = rep(1,ncol(X)),
                        maxstep = 100,
                        margin = 0.001,
                        maxstep_inner = 100,
                        tol.delta = 0.001,
                        compute.likelihood = FALSE){
        n = nrow(X);p=ncol(X);q = ncol(Z)
        ## Initialize
        alpha.old = alpha.init
        k = 1
        likelihood.score = c()
        while(k < maxstep){
                gamma = 2*exp(-2*Z%*%alpha.old)
                Sigma_y = sigma.square * diag(n) + X %*% diag(c(gamma)) %*% t(X)
                theta = diag(t(X) %*% solve(Sigma_y,X))
                
                # Compute likelihood
                if (compute.likelihood == TRUE){
                        likelihood.score = c(likelihood.score,compute_likelihood(X,Y,1/gamma,sigma.square))
                }
                
                ## update alpha given theta
                alpha.new <- update_alpha(delta.init = delta.init,theta=theta,maxstep_inner = maxstep_inner,tol.delta = tol.delta)$alpha.est
                
                if(sum(abs(alpha.new - alpha.old)) < margin ){
                        break
                }
                alpha.old <- alpha.new
                k <- k+1
                print(k)
        }
        coef.ard = diag(c(gamma))%*%t(X)%*%solve(Sigma_y,Y)

        tauEst = exp(Z%*%alpha.old)
        pen_vec= tauEst*sigma.square/n
        C= sum(pen_vec)/p
        coef.cusLasso = coef(glmnet(X,Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
        
        return(list(coef.ard = coef.ard,coef.cusLasso = coef.cusLasso,alpha.hat = alpha.old,n_iter = k-1,sigma.square = sigma.square,likelihood.score = likelihood.score))
}
