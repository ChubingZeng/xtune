#' Empirical Bayes estimation of Lasso tuning parameter.
#'
#' \code{customized_lasso} returns the tuning parameter estimated by empirical bayes method.
#' @param input_X predictor matrix of dimension \eqn{n*q}.
#' @param input_Y continuous outcome vector of dimension \eqn{p}.
#' @param initial_val initial value for \eqn{\tau}. Default value is 0.1.
#' @param max_step max step for iterations. Default value is 100.
#' @param margin convergence criteria. Default value is 0.01.
#' @return Returns the estimated tuning parameter and estimated coefficients
#' @examples
#' set.seed(99)
#' n = 100
#' p = 200
#' q = 5
#' x <- matrix(rnorm(2*n*p,0,1),nrow=2*n,ncol=p)
#' z <- matrix(sample(c(0,1),p*q,replace = TRUE,prob = c(0.5,0.5)),p,q)
#' z_design <- cbind(rep(1,p),z)
#' alpha=c(1,seq(from=-1,to=2,length.out = q))
#' betas=rlaplace(n = p, s = 1/exp(z_design%*%alpha))
#' y <- x%*%betas + rnorm(2*n,0,1)
#' x_train = x[1:n,]
#' y_train = y[1:n]
#' x_test = x[(n+1):(2*n),]
#' y_test = y[(n+1):(2*n)]
#' ## customized lasso
#' cus_lasso.fit = customized_lasso(inX = x_train,inY = y_train,inZ = z_design)
#' mean((cbind(rep(1,n),x_test)%*%(cus_lasso.fit$coef) - y_test)^2)
#' ## standard lasso
#' mean((cbind(rep(1,n),x_test)%*%coef(glmnet(x_train,y_train,alpha = 1,lambda=cv.glmnet(x_train,y_train)$lambda.min)) - y_test)^2)
#' ## adaptive lasso
#' ada_lasso = adalasso(x_train,y_train)
#' mean((cbind(rep(1,n),x_test)%*%c(ada_lasso$intercept.adalasso,ada_lasso$coefficients.adalasso)- y_test)^2)

###------------------------------ Customized LASSO ------------------------------###
customized_lasso <- function(input_X, input_Y, input_Z,initial_value = rep(0,ncol(input_Z)),inSigmaSquare = estimateVar_SI(input_X,input_Y)){
        alphaEst1 = tryCatch(lbfgs(approx_likelihood, score_function, input_X = input_X, input_Y = input_Y, input_Z = input_Z,
                                  sigma2_est = inSigmaSquare, initial_value, invisible = 1)$par, error = function(c) {
                                          optim(initial_value, fn = approx_likelihood, input_X = input_X, input_Y = input_Y, input_Z = input_Z, sigma2_est = inSigmaSquare)$par
                                  })
        tauEst1 = exp(input_Z%*%alphaEst1)
        coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = inSigmaSquare, penalty.factor = tauEst1))
        varEst1 = inSigmaSquare
        if(sum(coef1[-1] == 0) > (length(coef1[-1]) - 10)){
                use_eb = eb_tuning(input_X, input_Y)
                coef2 = use_eb$coef
                tauEst2 = use_eb$tau_est
                varEst2 = use_eb$var_est
                alphaEst2 = c(log(tauEst),rep(0,ncol(input_Z)-1))
                return(list(tau_est = tauEst2, var_est = varEst2, alpha_est = alphaEst2 ,coefficients = coef2))
        }
        else{
                return(list(tau_est = tauEst1, var_est = varEst1, alpha_est = alphaEst1 ,coefficients = coef1))
        }
}

