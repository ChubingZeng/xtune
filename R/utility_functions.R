##------------------- Esitmating error variance ------------------##
checkargs.xy <- function(x, y) {
    if (missing(x))
        stop("x is missing")
    if (is.null(x) || !is.matrix(x))
        stop("x must be a matrix")
    if (missing(y))
        stop("y is missing")
    if (is.null(y) || !is.numeric(y))
        stop("y must be numeric")
    if (ncol(x) == 0)
        stop("There must be at least one predictor [must have ncol(x) > 0]")
    if (checkcols(x))
        stop("x cannot have duplicate columns")
    if (length(y) == 0)
        stop("There must be at least one data point [must have length(y) > 0]")
    if (length(y) != nrow(x))
        stop("Dimensions don't match [length(y) != nrow(x)]")
}
checkcols <- function(A) {
    b = rnorm(nrow(A))
    a = sort(t(A) %*% b)
    if (any(diff(a) == 0))
        return(TRUE)
    return(FALSE)
}

estimateSigma <- function(x, y, intercept = TRUE, standardize = TRUE) {
    checkargs.xy(x, rep(0, nrow(x)))
    if (nrow(x) < 10)
        stop("Number of observations must be at least 10 to run estimateSigma")
    cvfit = cv.glmnet(x, y, intercept = intercept, standardize = standardize)
    lamhat = cvfit$lambda.min
    fit = glmnet(x, y, standardize = standardize)
    yhat = predict(fit, x, s = lamhat)
    nz = sum(predict(fit, s = lamhat, type = "coef") != 0)
    sigma = sqrt(sum((y - yhat)^2)/(length(y) - nz - 1))
    return(list(sigmahat = sigma, df = nz))
}

estimateVar_SI <- function(input_X, input_Y) {
    sd = array(NA, 10)
    options(warn = -1)
    for (m in 1:10) {
        temp = estimateSigma(input_X, input_Y)$sigmahat
        sd[m] = ifelse(temp %in% c(Inf, NaN), NA, temp)
    }
    sigma_sq_est_SI = mean(sd, na.rm = T)^2
    return(sigma_sq_est_SI)
}

##-------------------- MSE, MSE, MSE! --------------------------##
get_mse <- function(estimation, true) {
    return(mean((estimation - true)^2))
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

sgd_momentum <- function(input_X,input_Y,input_Z,initial_val = rep(0,ncol(input_Z)),
                         momentum = 0.9,step_size = 0.1,margin = 10e-4,
                         max_iters = 1000, sigma_square = estimateVar_SI(input_X,input_Y)){
        alpha = initial_val
        iter = 0
        velocity = 0
        while (iter < max_iters) {
                gradient = score_function(alpha,input_X,input_Y,input_Z,sigma_square)/nrow(X)
                velocity = momentum*velocity - eta * gradient
                alpha = as.vector(alpha + velocity)
                if(sqrt(sum(gradient^2)) <= epsilon){
                        break
                }
                iter = iter + 1
        }
        return(alpha)
}
