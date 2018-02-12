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

