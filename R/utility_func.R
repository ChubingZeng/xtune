#' @importFrom stats optim var coef
update_alpha.lasso<-function(X,Y,Z,alpha.old,sigma.square,theta,maxstep_inner,tol.inner){
        ## initial
        alpha.iner.old = alpha.old
        k_inner = 1
        n=nrow(X)
        p=ncol(X)
        while (k_inner < maxstep_inner){
                # given alpha update delta
                gamma = 2*exp(-2*Z%*%alpha.iner.old)
                sd_y <- sqrt(var(Y)*(n-1)/n)
                C=sum(1/gamma)/p* sd_y*sigma.square/n
                delta.est=coef(glmnet(X,Y,alpha=0, penalty.factor = 1/gamma,lambda = C, standardize = F, intercept = FALSE))[-1]

                ## given delta update alpha
                alpha.iner.new <- optim(alpha.old,likelihood.alpha.theta.lasso,likelihood.alpha.theta.gradient.lasso,Z=Z,theta = theta,delta=delta.est,method = "BFGS")$par
                if (sum(abs(alpha.iner.new - alpha.iner.old)) < tol.inner){
                        break
                }
                k_inner = k_inner + 1
                alpha.iner.old <- alpha.iner.new
        }
        return(list(alpha.est=alpha.iner.old,inner_iter = k_inner))
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
                alpha.iner.new <- optim(alpha.old,likelihood.alpha.theta.ridge,likelihood.alpha.theta.gradient.ridge,Z=Z,theta = theta,delta=delta.est,method = "BFGS")$par
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
