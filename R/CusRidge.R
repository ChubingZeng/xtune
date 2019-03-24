#' Customized LASSO using iterative reweighted l2
#'
#' \code{CusLasso.l2} estimated beta coefficients and pseudo probablity
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
#' @examples
#' CusLassol2.result = CusLasso.l2(x_train,y_train,Z,sigma.square = sig.est,compute.likelihood = T)

Cus.Ridge<- function(X,Y,Z,sigma.square = estimateVariance(X,Y),
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

update_alpha.ridge<-function(X,Y,Z,alpha.old,sigma.square,theta,maxstep_inner,tol.inner){
        ## initial
        alpha.iner.old = alpha.old
        k_inner = 1
        n=nrow(X)
        p=ncol(X)
        while (k_inner < maxstep_inner){
                # given alpha update delta
                gamma = exp(-Z%*%alpha.iner.old)
                sd_y <- sqrt(var(Y)*(n-1)/n)
                C=sum(1/gamma)/p* sd_y*sigma.square/n
                delta.est=coef(glmnet(X,Y,alpha=0, penalty.factor = 1/gamma,lambda = C, standardize = F, intercept = FALSE))[-1]

                ## given delta update alpha
                alpha.iner.new <- stats::optim(alpha.old,likelihood.alpha.theta.ridge,likelihood.alpha.theta.gradient.ridge,Z=Z,theta = theta,delta=delta.est,method = "BFGS")$par
                if (sum(abs(alpha.iner.new - alpha.iner.old)) < tol.inner){
                        break
                }
                k_inner = k_inner + 1
                alpha.iner.old <- alpha.iner.new
        }
        return(list(alpha.est=alpha.iner.old,inner_iter = k_inner))
}

likelihood.alpha.theta.ridge<-function(Z,alpha,theta,delta){
        gamma = exp(-Z%*%alpha)
        return(as.numeric(t(theta)%*%gamma + delta^2%*%(1/gamma)))
}

likelihood.alpha.theta.gradient.ridge<-function(Z,alpha,theta,delta){
        gamma = exp(-Z%*%alpha)
        dev_gamma = (theta - delta^2/(gamma^2))
        return(-crossprod(dev_gamma,as.vector(gamma) * Z))
}

approx_likelihood.ridge <- function(to_estimate,X,Y,Z,sigma.square.est) {
        n = nrow(X)
        gamma = exp(-Z %*% to_estimate)  ## to_estimate:alpha estimates
        K = sigma.square.est * diag(n) + X %*% diag(c(gamma)) %*% t(X)
        logdetK = determinant(K)$modulus[1]
        part1 = t(Y) %*% solve(K,Y)
        normapprox = 1/2 * (part1 + logdetK)
        return(as.numeric(normapprox))
}

estimateVariance<-function(X,Y,num = 10) {
        options(warn = -1)
        temp = array(NA,num)
        for (i in 1:num){
                c = selectiveInference::estimateSigma(X,Y)$sigmahat^2
                temp[i] = ifelse(is.infinite(c),NA,c)
        }
        return(mean(temp,na.rm =T))
}
