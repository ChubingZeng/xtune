#' Empirical Bayes estimation of Lasso tuning parameter, add ridge future
#'
#' \code{eb_tuning} returns the tuning parameter estimated by empirical bayes method.
#' @param X predictor matrix of dimension \eqn{n*q}.
#' @param Y continuous outcome vector of dimension \eqn{p}.
#' @param var.ini initial estimates of variance from external estimation
#' @param initial_val initial value for \eqn{\tau}. Default value is 0.1.
#' @param maxstep max step for iterations. Default value is 100.
#' @param margin convergence criteria. Default value is 0.01.
#' @param verbosity whether to track the update process
#' @param var.compare whether or not to compare the variance estimates from different methods
#' @param var.fix if TRUE, fix variance estimates
#' @return Returns the estimated tuning parameter and estimated coefficients
#' @export


eb_tuning<-function (X, Y, var.ini = estimateVariance(X,Y),initial_val = 0.1, maxstep = 100,
                     margin = 0.01, verbosity = 0, var.compare = TRUE, var.fix = FALSE)
{
        var_est = var.ini
        gamma = initial_val
        gamma_old = gamma
        n = nrow(X)
        p = ncol(X)
        k = 1
        while (k < maxstep) {
                Rinv <- backsolve(chol(crossprod(X)/var_est + diag(rep(gamma,
                                                                       p))), diag(1, p))
                diagSigma <- rowSums(Rinv^2)
                mu_vec <- (Rinv %*% (crossprod(Rinv, crossprod(X,
                                                               Y))))/var_est
                err <- sum((Y - X %*% mu_vec)^2)
                if (var_est < 2e-09) {
                        warning("Model might be overfitted")
                        break
                }
                if (verbosity > 0) {
                        log.det.Sigma.inv <- -2 * sum(log(diag(Rinv)))
                        mlike <- -1/2 * (log.det.Sigma.inv - p * log(gamma) +
                                                 n * log(var_est) + 1/var_est * err + as.vector(mu_vec^2) %*%
                                                 (rep(gamma, p)))
                        cat("Iteration =", k, " Marg. Likelihood =", formatC(mlike),
                            "\tvar=", var_est, "\tgamma=", gamma, "\n")
                }
                eta <- p - gamma * (sum(diagSigma))
                gamma <- eta/(t(mu_vec) %*% mu_vec)
                if ((gamma - gamma_old) < margin) {
                        break
                }
                if (!var.fix) {
                        var_est <- as.numeric(err/(n - eta))
                }
                k = k + 1
                gamma_old = gamma
        }
        estimated_tau = sqrt(2 * gamma)
        if (var.compare) {
                estimated_variance = ifelse(var.ini < var_est + 10,
                                            var.ini, var_est)
        }
        else {
                estimated_variance = var_est
        }
        tuningParameter = estimated_tau * estimated_variance/(nrow(X))
        coef = coef(glmnet(X, Y, alpha = 1, lambda = tuningParameter))
        return(list(tuningPar = tuningParameter, coef = coef, var_est = estimated_variance,
                    tau_est = estimated_tau))
}


