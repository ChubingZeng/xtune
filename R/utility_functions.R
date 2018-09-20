# estimateVar_SI <- function(input_X, input_Y) {
#         options(warn = -1)
#         while(TRUE){
#                 temp = estimateSigma(input_X, input_Y)$sigmahat
#                 if(!is.nan(temp)){
#                         break
#                 }
#         }
#         return(temp^2)
# }

estimateVariance <- function(input_X, input_Y,num = 10) {
        options(warn = -1)
        temp = array(NA,num)
        for (i in 1:num){
                temp[i] = estimateSigma(input_X, input_Y)$sigmahat^2
        }
        return(mean(temp,na.rm =T))
}

##-------------------- MSE, MSE, MSE! --------------------------##
get_mse <- function(estimation, true) {
    return(mean((as.vector(estimation) - as.vector(true))^2))
}

consistent <- function(true, estimate, tolerrance) {
    bi_true = rep(0, p)
    bi_true[which(abs(true) > tolerrance)] = 1
    bi_est = rep(0, p)
    bi_est[which(abs(estimate) > tolerrance)] = 1
    tab = table(bi_true, bi_est)
    sen = tab[2, 2]/sum(tab[2, ])
    spec = tab[1, 1]/sum(tab[1, ])
    return(list(sensitivity = sen, specificity = spec))
}

relative_change <- function(x, x_ref) {
    return((x - x_ref)/x_ref)
}



# score_function <- function(to_estimate, input_X, input_Y, input_Z, sigma2_est) {
#         X = input_X
#         Y = input_Y
#         Z = input_Z
#         sigma_s = sigma2_est
#         n = nrow(X)
#         gamma = 2/exp(2 * Z %*% to_estimate)  ## to_estimate:alpha estimates
#         big_sigma = ginv(1/sigma_s * t(X) %*% X + diag(c(1/gamma)))
#         big_mu = 1/sigma_s * big_sigma %*% t(X) %*% Y
#         dev_gamma = (gamma - diag(big_sigma) - big_mu^2)/(gamma^2)
#         return(-t(dev_gamma) %*% (as.vector(gamma) * Z))
# }

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

sgd_momentum <- function(input_X,input_Y,input_Z,initial_val = rep(0,ncol(input_Z)),
                         momentum = 0.9,step_size = 0.1,margin = 10e-4,
                         max_iters = 1000, sigma_square = estimateVar_SI(input_X,input_Y)){
        alpha = initial_val
        iter = 0
        velocity = 0
        while (iter < max_iters) {
                gradient = score_function(alpha,input_X,input_Y,input_Z,sigma_square)/nrow(input_X)
                velocity = momentum*velocity - step_size * gradient
                alpha = as.vector(alpha + velocity)
                if(sqrt(sum(gradient^2)) <= margin){
                        break
                }
                iter = iter + 1
        }
        return(alpha)
}

# approx_likelihood_single <- function(tau,sigma_square, input_X, input_Y) {
#         X = input_X
#         Y = input_Y
#         n = nrow(X)
#         gamma = 2/exp(2 * tau)  ## to_estimate:alpha estimates
#         K = sigma_square * diag(n) + X %*% diag(rep(gamma,ncol(X))) %*% t(X)
#         logdetK = determinant(K)$modulus[1]
#         part1=t(Y)%*%solve(K,Y)
#         normapprox = 1/2 * (part1 + logdetK)
#         return(as.numeric(normapprox))
# }

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

# approx_likelihood <- function(to_estimate, input_X, input_Y, input_Z, sigma2_est) {
#         X = input_X
#         Y = input_Y
#         Z = input_Z
#         sigma_s = sigma2_est
#         n = nrow(X)
#         gamma = 2/exp(2 * Z %*% to_estimate)  ## to_estimate:alpha estimates
#         K = sigma_s * diag(n) + X %*% diag(c(gamma)) %*% t(X)
#         logdetK = determinant(K)$modulus[1]
#         part1 = t(Y) %*% ginv(K) %*% Y
#         normapprox = 1/2 * (part1 + logdetK)
#         return(as.numeric(normapprox))
# }
update_alpha<-function(delta.init,theta,maxstep_inner,tol.delta){
        ## initial
        delta.old = delta.init
        k_inner = 1
        while (k_inner < maxstep_inner){
                ## given delta update alpha
                alpha.est <- optim(alpha,likelihood.alpha.theta,likelihood.alpha.theta.gradient,theta = theta,delta=delta.old,method = "BFGS")$par
                ## given alpha update delta
                gamma = 2*exp(-2*Z%*%alpha.est)
                delta.new = coef(glmnet(X,Y,alpha = 0, lambda = sigma.square/n*sum(1/gamma)/p), penalty.factor = 1/gamma,intercept = F)[-1]
                if (sum(abs(delta.new - delta.old)) < tol.delta){
                        break
                }
                k_inner = k_inner + 1
                delta.old <- delta.new
        }
        return(list(alpha.est=alpha.est,inner_iter = k_inner))
}

likelihood.alpha.theta<-function(alpha,theta,delta){
        gamma = 2*exp(-2*Z%*%alpha)
        return(as.numeric(t(theta)%*%gamma + delta^2%*%(1/gamma)))
}

likelihood.alpha.theta.gradient<-function(alpha,theta,delta){
        gamma = 2*exp(-2*Z%*%alpha)
        dev_gamma = (theta - delta^2/(gamma^2))
        return(as.vector(crossprod(dev_gamma,as.vector(gamma) * Z)*(-2)))
}

accuracy <- function(predicted,true){
        return(sum(predicted == true)/length(true))
}

compute_likelihood <- function(X,Y,gamma,sigma.square) {
        n = nrow(X)
        K = sigma.square * diag(n) + X %*% diag(c(gamma)) %*% t(X)
        logdetK = determinant(K)$modulus[1]
        part1 = t(Y) %*% solve(K,Y)
        normapprox = 1/2 * (part1 + logdetK)
        return(as.numeric(normapprox))
}
