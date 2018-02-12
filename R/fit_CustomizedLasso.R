###------------------------------ Customized LASSO ------------------------------###
approx_likelihood <- function(to_estimate, input_X, input_Y, input_Z, sigma2_est) {
    X = input_X
    Y = input_Y
    Z = input_Z
    sigma_s = sigma2_est
    n = nrow(X)
    gamma = 2/exp(2 * Z %*% to_estimate)  ## to_estimate:alpha estimates
    K = sigma_s * diag(n) + X %*% diag(c(gamma)) %*% t(X)
    logdetK = determinant(K)$modulus[1]
    part1 = t(Y) %*% ginv(K) %*% Y
    normapprox = 1/2 * (part1 + logdetK)
    return(as.numeric(normapprox))
}


score_function <- function(to_estimate, input_X, input_Y, input_Z, sigma2_est) {
    X = input_X
    Y = input_Y
    Z = input_Z
    sigma_s = sigma2_est
    n = nrow(X)
    gamma = 2/exp(2 * Z %*% to_estimate)  ## to_estimate:alpha estimates
    big_sigma = ginv(1/sigma_s * t(X) %*% X + diag(c(1/gamma)))
    big_mu = 1/sigma_s * big_sigma %*% t(X) %*% Y
    dev_gamma = (gamma - diag(big_sigma) - big_mu^2)/(gamma^2)
    return(-t(dev_gamma) %*% (as.vector(gamma) * Z))
}



customized_lasso <- function(initial_value, inX, inY, inZ, inSigmaSquare) {
    alphaEst = tryCatch(lbfgs(approx_likelihood, score_function, input_X = inX, input_Y = inY, input_Z = inZ, 
        sigma2_est = inSigmaSquare, initial_value, invisible = 1)$par, error = function(c) {
        optim(initial_value, fn = approx_likelihood, input_X = inX, input_Y = inY, input_Z = inZ, sigma2_est = inSigmaSquare)$par
    })
    return(alphaEst)
}

