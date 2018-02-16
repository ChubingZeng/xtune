#' Empirical Bayes estimation of Lasso tuning parameter.
#'
#' \code{eb_tuning} returns the tuning parameter estimated by empirical bayes method.
#' @param input_X predictor matrix of dimension \eqn{n*q}.
#' @param input_Y continuous outcome vector of dimension \eqn{p}.
#' @param initial_val initial value for \eqn{\tau}. Default value is 0.1.
#' @param max_step max step for iterations. Default value is 100.
#' @param margin convergence criteria. Default value is 0.01.
#' @return Returns the estimated tuning parameter and estimated coefficients
#' @examples
#' set.seed(999)
#' n = 100
#' p = 200
#' x <- matrix(rnorm(2*n*p,0,1),nrow=2*n,ncol=p)
#' y <- x%*%c(rlaplace(p,0,1)) + rnorm(2*n,0,1)
#' x_train = x[1:n,]
#' y_train = y[1:n]
#' x_test = x[(n+1):(2*n),]
#' y_test = y[(n+1):(2*n)]
#' eb.fit = eb_tuning(x_train,y_train)
#' print(cv.glmnet(x,y)$lambda.min)
#' print(eb.fit$tuningPar)
#' mean((cbind(rep(1,n),x_test)%*%(eb.fit$coef) - y_test)^2)
#' mean((cbind(rep(1,n),x_test)%*%coef(glmnet(x_train,y_train,alpha = 1,lambda=cv.glmnet(x_train,y_train)$lambda.min)) - y_test)^2)


eb_tuning <- function(input_X, input_Y, initial_val = 0.1, maxstep = 100, margin = 0.01) {
    tuningParameter = tryCatch({
        # estimate sigma square from SI
        var_SI = estimateVar_SI(input_X, input_Y)
        # estiamtes from EB
        X = input_X
        Y = input_Y
        gamma = initial_val
        sigma2 = var_SI
        n = nrow(input_X)
        p = ncol(input_X)

        gamma_sample = matrix(NA, ncol = 1, nrow = maxstep)
        sigma2_sample = matrix(NA, ncol = 1, nrow = maxstep)
        k = 1
        while (k < maxstep) {

            big_sigma = ginv((1/sigma2) * t(X) %*% X + diag(rep(gamma, p)))
            big_mu = (1/sigma2) * big_sigma %*% t(X) %*% Y

            if (k > 3) {
                distance = sum(gamma_sample[k - 1] - gamma_sample[k - 2])
                if (distance < margin) {
                  break
                }
            }

            eta = p - gamma * sum(diag(big_sigma))
            gamma = eta/(t(big_mu) %*% big_mu)
            yminus = Y - X %*% big_mu
            sigma2 = as.numeric(t(yminus) %*% yminus/(n - eta))

            gamma_sample[k] = gamma
            sigma2_sample[k] = sigma2
            k = k + 1
        }
        estimated_tau = sqrt(2 * gamma)
        var_EB = sigma2
        estimated_variance = ifelse(var_SI < var_EB + 10, var_SI, var_EB)
        # return parameters
        tuningParameter = estimated_tau * estimated_variance/(nrow(input_X))

    }, error = function(c) {
        tuningParameter = cv.glmnet(X, Y)$lambda.min
        estimated_tau = tuningParameter*nrow(input_X)/estimated_variance
    })
    coef = coef(glmnet(X,Y,alpha = 1,lambda = tuningParameter))
    return(list(tuningPar = tuningParameter, coef = coef, var_est = estimated_variance, tau_est = estimated_tau ))
}
