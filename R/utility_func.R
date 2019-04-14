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

score_function <- function(to_estimate, input_X, input_Y, input_Z, sigma2_est) {
        X = input_X
        Y = input_Y
        Z = input_Z
        sigma_square = sigma2_est
        n = nrow(X)
        p = ncol(X)
        gamma = 2/exp(2 * Z %*% to_estimate)
        Rinv <- backsolve(chol(crossprod(X)/sigma_square + diag(c(1/gamma))), diag(1, p))
        diagSigma <- rowSums(Rinv^2)
        mu_vec <- (Rinv %*% (crossprod(Rinv, crossprod(X,Y))))/sigma_square
        dev_gamma = (gamma - diagSigma - mu_vec^2)/(gamma^2)
        return(-crossprod(dev_gamma,as.vector(gamma) * Z))
}

approx_likelihood <- function(to_estimate, input_X, input_Y, input_Z, sigma2_est) {
        X = input_X
        Y = input_Y
        Z = input_Z
        sigma_s = sigma2_est
        n = nrow(X)
        gamma = 2/exp(2 * Z %*% to_estimate)  ## to_estimate:alpha estimates
        K = sigma_s * diag(n) + X %*% diag(c(gamma)) %*% t(X)
        logdetK = determinant(K)$modulus[1]
        part1 = t(Y) %*% solve(K,Y)
        normapprox = 1/2 * (part1 + logdetK)
        return(as.numeric(normapprox))
}
