#' Empirical Bayes estimation of Lasso tuning parameter.
#'
#' \code{customized_lasso} returns the estimated coefficients and penalty factor
#' @param input_X predictor matrix of dimension \eqn{n*q}.
#' @param input_Y continuous outcome vector of dimension \eqn{p}.
#' @param input_Z continuous outcome vector of dimension \eqn{q}.
#' @param initial_val initial value for \eqn{\alpha}. Default value is a vector of \eqn{0}s.
#' @param inSigmaSquare variance estimation, default is the estimated variance using R package "selectiveinference"
#' @return returns the estimated coefficients and penalty factor
#' @examples
#' set.seed(99)
#' n = 100
#' p = 400
#' q = 10
#' x <- matrix(rnorm(2*n*p,0,1),nrow=2*n,ncol=p)
#' z <- matrix(sample(c(0,1),p*q,replace = TRUE,prob = c(0.5,0.5)),p,q)
#' z_design <- cbind(rep(1,p),z)

#' alpha=c(2,rnorm(q,0,1))
#' betas=rlaplace(n = p, s = 1/exp(z_design%*%alpha))
#' y <- x%*%betas + rnorm(2*n,0,1)
#' x_train = x[1:n,]
#' y_train = y[1:n]
#' x_test = x[(n+1):(2*n),]
#' y_test = y[(n+1):(2*n)]

#' cus_lasso.fit = customized_lasso(x_train,y_train,z_design)
#' mean((cbind(rep(1,n),x_test)%*%(cus_lasso.fit$coef) - y_test)^2)
#' 1-mean((cbind(rep(1,n),x_test)%*%(cus_lasso.fit$coef) - y_test)^2)/var(y_test)
#'
###------------------------------ Customized LASSO ------------------------------###
customized_lasso <- function(input_X, input_Y, input_Z,initial_value = rep(0,ncol(input_Z)),inSigmaSquare = estimateVar_SI(input_X,input_Y), method = "L-BFGS-B"){
        tryCatch({
                if(method == "L-BFGS-B"){
                        alphaEst1 = lbfgs(approx_likelihood, score_function, input_X = input_X, input_Y = input_Y, input_Z = input_Z,
                                          sigma2_est = inSigmaSquare, initial_value, invisible = 1)$par
                        tauEst1 = exp(input_Z%*%alphaEst1)
                        coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = inSigmaSquare, penalty.factor = tauEst1))
                        varEst1 = inSigmaSquare
                        if(sum(coef1[-1] == 0) > (length(coef1[-1]) - 10)){
                                use_eb = eb_tuning(input_X, input_Y)
                                coef2 = use_eb$coef
                                tauEst2 = rep(use_eb$tau_est,ncol(input_X))
                                varEst2 = use_eb$var_est
                                alphaEst2 = c(log(tauEst2),rep(0,ncol(input_Z)))
                                return(list(tau_est = tauEst2, var_est = varEst2, alpha_est = alphaEst2 ,coefficients = coef2))
                        }
                        else{
                                return(list(tau_est = tauEst1, var_est = varEst1, alpha_est = alphaEst1 ,coefficients = coef1))
                        }
                }
                if(method == "Nelder-Mead"){
                        alphaEst1 = optim(initial_value, fn = approx_likelihood, input_X = input_X, input_Y = input_Y, input_Z = input_Z, sigma2_est = inSigmaSquare)$par
                        tauEst1 = exp(input_Z%*%alphaEst1)
                        coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = inSigmaSquare, penalty.factor = tauEst1))
                        varEst1 = inSigmaSquare
                        if(sum(coef1[-1] == 0) > (length(coef1[-1]) - 10)){
                                use_eb = eb_tuning(input_X, input_Y)
                                coef2 = use_eb$coef
                                tauEst2 = rep(use_eb$tau_est,ncol(input_X))
                                varEst2 = use_eb$var_est
                                alphaEst2 = c(log(tauEst2),rep(0,ncol(input_Z)-1))
                                return(list(tau_est = tauEst2, var_est = varEst2, alpha_est = alphaEst2 ,coefficients = coef2))
                        }
                        else{
                                return(list(tau_est = tauEst1, var_est = varEst1, alpha_est = alphaEst1 ,coefficients = coef1))
                        }
                }
                if(method == "sgd"){
                        alphaEst1 = sgd_momentum(input_X,input_Y,input_Z,sigma_square = inSigmaSquare)
                        tauEst1 = exp(input_Z%*%alphaEst1)
                        coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = inSigmaSquare, penalty.factor = tauEst1))
                        varEst1 = inSigmaSquare
                        if(sum(coef1[-1] == 0) > (length(coef1[-1]) - 10)){
                                use_eb = eb_tuning(input_X, input_Y)
                                coef2 = use_eb$coef
                                tauEst2 = rep(use_eb$tau_est,ncol(input_X))
                                varEst2 = use_eb$var_est
                                alphaEst2 = c(log(tauEst2),rep(0,ncol(input_Z)-1))
                                return(list(tau_est = tauEst2, var_est = varEst2, alpha_est = alphaEst2 ,coefficients = coef2))
                        }
                        else{
                                return(list(tau_est = tauEst1, var_est = varEst1, alpha_est = alphaEst1 ,coefficients = coef1))
                        }
                }


        },error = function(c){
                use_eb = eb_tuning(input_X, input_Y)
                coef2 = use_eb$coef
                tauEst2 = rep(use_eb$tau_est,ncol(input_X))
                varEst2 = use_eb$var_est
                alphaEst2 = c(log(tauEst2),rep(0,ncol(input_Z)-1))
                return(list(tau_est = tauEst2, var_est = varEst2, alpha_est = alphaEst2 ,coefficients = coef2))
        })

}

