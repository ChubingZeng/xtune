#' @importFrom stats optim var coef
update_alpha.lasso<-function(X,Y,Z,alpha.old,sigma.square,theta,maxstep_inner,tolerance_inner){
        ## initial
        alpha.inner.old = alpha.old
        k_inner = 1
        n=nrow(X)
        p=ncol(X)
        while (k_inner < maxstep_inner){
                # given alpha update delta
                gamma = 2*exp(-2*Z%*%alpha.inner.old)
                sd_y <- sqrt(var(Y)*(n-1)/n)
                C=sum(1/gamma)/p* sd_y*sigma.square/n
                delta.est=coef(glmnet(X,Y,alpha=0, penalty.factor = 1/gamma,lambda = C, standardize = F, intercept = FALSE))[-1]

                ## given delta update alpha
                alpha.inner.new <- optim(alpha.old,likelihood.alpha.theta.lasso,likelihood.alpha.theta.gradient.lasso,Z=Z,theta = theta,delta=delta.est,method = "BFGS")$par
                if (sum(abs(alpha.inner.new - alpha.inner.old)) < tolerance_inner){
                        break
                }
                k_inner = k_inner + 1
                alpha.inner.old <- alpha.inner.new
        }
        return(list(alpha.est=alpha.inner.old,inner_iter = k_inner))
}



likelihood.alpha.theta.lasso<-function(Z,alpha,theta,delta){
        gamma = 2*exp(-2*Z%*%alpha)
        return(as.numeric(t(theta)%*%gamma + delta^2%*%(1/gamma)))
}

likelihood.alpha.theta.gradient.lasso<-function(Z,alpha,theta,delta){
        gamma = 2*exp(-2*Z%*%alpha)
        dev_gamma = (theta - delta^2/(gamma^2))
        return(crossprod(dev_gamma,as.vector(gamma) * Z)*(-2))
}

approx_likelihood.lasso <- function(to_estimate,X,Y,Z,sigma.square.est) {
        n = nrow(X)
        gamma = 2/exp(2 * Z %*% to_estimate)  ## to_estimate:alpha estimates
        K = sigma.square.est * diag(n) + X %*% diag(c(gamma)) %*% t(X)
        logdetK = determinant(K)$modulus[1]
        part1 = t(Y) %*% solve(K,Y)
        normapprox = 1/2 * (part1 + logdetK)
        return(-as.numeric(normapprox))
}

update_alpha.ridge<-function(X,Y,Z,alpha.old,sigma.square,theta,maxstep_inner,tolerance_inner){
        ## initial
        alpha.inner.old = alpha.old
        k_inner = 1
        n=nrow(X)
        p=ncol(X)
        while (k_inner < maxstep_inner){
                # given alpha update delta
                gamma = exp(-Z%*%alpha.inner.old)
                sd_y <- sqrt(var(Y)*(n-1)/n)
                C=sum(1/gamma)/p* sd_y*sigma.square/n
                delta.est=coef(glmnet(X,Y,alpha=0, penalty.factor = 1/gamma,lambda = C, standardize = F, intercept = FALSE))[-1]

                ## given delta update alpha
                alpha.inner.new <- optim(alpha.old,likelihood.alpha.theta.ridge,likelihood.alpha.theta.gradient.ridge,Z=Z,theta = theta,delta=delta.est,method = "BFGS")$par
                if (sum(abs(alpha.inner.new - alpha.inner.old)) < tolerance_inner){
                        break
                }
                k_inner = k_inner + 1
                alpha.inner.old <- alpha.inner.new
        }
        return(list(alpha.est=alpha.inner.old,inner_iter = k_inner))
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
        return(-as.numeric(normapprox))
}

get_mse <- function(estimation, true) {
        return(mean((as.vector(estimation) - as.vector(true))^2))
}

sup_algos <- function(input_X, input_Y, input_Z,initial_value = rep(0,ncol(input_Z)),inSigmaSquare = estimateVariance(input_X,input_Y), method = "L-BFGS-B",compute.likelihood=F){
        N = nrow(input_X)
        P = ncol(input_X)
        likelihood.score <-c()
        if(method == "L-BFGS-B"){
                alphaEst1 = lbfgs(approx_likelihood, score_function, input_X = input_X, input_Y = input_Y, input_Z = input_Z,
                                  sigma2_est = inSigmaSquare, initial_value, invisible = 1)$par
                tauEst1 = exp(input_Z%*%alphaEst1)
                pen_vec= tauEst1*inSigmaSquare/N
                C= sum(pen_vec)/P
                coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
                varEst1 = inSigmaSquare
        }
        if(method == "Nelder-Mead"){
                alphaEst1 = optim(initial_value, fn = approx_likelihood, input_X = input_X, input_Y = input_Y, input_Z = input_Z, sigma2_est = inSigmaSquare)$par
                tauEst1 = exp(input_Z%*%alphaEst1)
                pen_vec= tauEst1*inSigmaSquare/N
                C= sum(pen_vec)/P
                coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
                varEst1 = inSigmaSquare
        }
        if(method == "sgd"){
                result = sgd_momentum(input_X,input_Y,input_Z,sigma_square = inSigmaSquare,compute.likelihood=compute.likelihood)
                alphaEst1 = result$alpha.est
                likelihood.score = result$likelihood.score
                tauEst1 = exp(input_Z%*%alphaEst1)
                pen_vec= tauEst1*inSigmaSquare/N
                C= sum(pen_vec)/P
                coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
                varEst1 = inSigmaSquare
        }
        return(list(tau_est = tauEst1, var_est = varEst1, alpha_est = alphaEst1 ,coefficients = coef1,likelihood.score = likelihood.score))
}

