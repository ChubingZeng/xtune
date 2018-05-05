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


eb_tuning <- function(input_X, input_Y, initial_val = 0.1, maxstep = 100, margin = 0.01,verbosity = 0,var.cal = FALSE,var.fix = FALSE) {
        ## calculate var_si for initial value
        if (var.cal) {
                var_est_SI = estimateVar_SI(input_X, input_Y)
                var_est = var_est_SI
        } else{
                var_est = 1
        }
        # estimate sigma square from SI
        # estiamtes from EB

        gamma = initial_val
        gamma_old = gamma

        n = nrow(input_X)
        p = ncol(input_X)

        k = 1
        while (k < maxstep) {
                ## Big sigma related
                Rinv <- backsolve(chol(crossprod(input_X)/var_est + diag(rep(gamma, p))),diag(1,p))
                diagSigma <- rowSums(Rinv^2)
                ## compute mu vector
                mu_vec <- (Rinv %*% (crossprod(Rinv, crossprod(input_X, input_Y))))/var_est
                ## error
                err <- sum ((input_Y - input_X %*% mu_vec)^2)

                if(var_est < 2e-9)
                {
                        warning("Model might be overfitted")
                        break
                }

                ## Compute log likelihood
                if (verbosity > 0) {
                        log.det.Sigma.inv <- - 2 * sum (log (diag (Rinv)))

                        ## compute the marginal likelihood to monitor convergence
                        mlike <- -1/2 * (log.det.Sigma.inv - p*log(gamma)+
                                                 n * log (var_est) + 1/var_est * err +
                                                 as.vector(mu_vec^2) %*% (rep(gamma, p)))

                        cat ("Iteration =",k, " Marg. Likelihood =", formatC (mlike),"\tvar=", var_est, "\tgamma=",gamma,"\n")
                }

                ## compute eta
                eta <- p - gamma*(sum(diagSigma))
                ## update gamma
                gamma <- eta/(t(mu_vec) %*% mu_vec)

                ## stop if gamma update is too small
                if(gamma-gamma_old < margin){
                        break
                }

                ## update var_est (if var.cal is TRUE, var_est_SI is calculated, set var.fix = TRUE to use var_est_SI estimation; if var.cal = FALSE,var.fix must be set to FALSE)
                if (!var.fix) {
                        var_est <- as.numeric(err / (n - eta))
                }

                k = k + 1
                gamma_old = gamma
        }

        estimated_tau = sqrt(2 * gamma)
        if (!var.cal){
                estimated_variance = ifelse(var_est_SI < var_est + 10, var_est_SI, var_est)
        } else{
                estimated_variance = var_est
        }

        # return parameters
        tuningParameter = estimated_tau * estimated_variance/(nrow(input_X))
        coef = coef(glmnet(input_X,input_Y,alpha = 1,lambda = tuningParameter))
        return(list(tuningPar = tuningParameter, coef = coef, var_est = estimated_variance, tau_est = estimated_tau ))
}
