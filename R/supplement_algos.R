sup_algos <- function(input_X, input_Y, input_Z,initial_value = rep(0,ncol(input_Z)),inSigmaSquare = estimateVariance(input_X,input_Y), method = "L-BFGS-B",compute.likelihood=F){
        N = nrow(input_X)
        P = ncol(input_X)
        likelihood.score <-c()
        if(method == "L-BFGS-B"){
                alphaEst1 = lbfgs(approx_likelihood, score_function, input_X = input_X, input_Y = input_Y, input_Z = input_Z,
                                  sigma2_est = inSigmaSquare, initial_value, invisible = 1)$par
                tauEst1 = exp(input_Z%*%alphaEst1)
                pen_vec= tauEst1*inSigmaSquare/N
                C= sum(pen_vec)/P
                coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
                varEst1 = inSigmaSquare
        }
        if(method == "Nelder-Mead"){
                alphaEst1 = optim(initial_value, fn = approx_likelihood, input_X = input_X, input_Y = input_Y, input_Z = input_Z, sigma2_est = inSigmaSquare)$par
                tauEst1 = exp(input_Z%*%alphaEst1)
                pen_vec= tauEst1*inSigmaSquare/N
                C= sum(pen_vec)/P
                coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
                varEst1 = inSigmaSquare
        }
        if(method == "sgd"){
                result = sgd_momentum(input_X,input_Y,input_Z,sigma_square = inSigmaSquare,compute.likelihood=compute.likelihood)
                alphaEst1 = result$alpha.est
                likelihood.score = result$likelihood.score
                tauEst1 = exp(input_Z%*%alphaEst1)
                pen_vec= tauEst1*inSigmaSquare/N
                C= sum(pen_vec)/P
                coef1 = coef(glmnet(input_X,input_Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
                varEst1 = inSigmaSquare
        }
        return(list(tau_est = tauEst1, var_est = varEst1, alpha_est = alphaEst1 ,coefficients = coef1,likelihood.score = likelihood.score))
}
