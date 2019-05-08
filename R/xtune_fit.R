#' @import glmnet
#' @importFrom stats optim
xtune.fit <- function(X, Y, Z, sigma.square, method, alpha.init, maxstep, tolerance,
                      maxstep_inner, tolerance_inner, compute.likelihood, verbosity, standardize = standardize,
                      intercept = intercept) {

        n = nrow(X)
        p = ncol(X)
        q = ncol(Z)

        if (method == "lasso") {
                ##------------ extending lasso regression
                ## Initialize
                alpha.old = alpha.init
                likelihood.score = c()
                k = 1
                while (k < maxstep) {
                        # Given alpha, update theta
                        gamma = 2 * exp(-2 * Z %*% alpha.old)  ## variance of beta in the approximated model
                        Sigma_y = sigma.square * diag(n) + (t(t(X) * c(gamma))) %*% t(X)
                        theta = colSums(X * solve(Sigma_y, X))

                        # Compute likelihood
                        if (compute.likelihood == TRUE) {
                                likelihood.score = c(likelihood.score, approx_likelihood.lasso(alpha.old,
                                                                                               X, Y, Z, sigma.square))
                        }

                        # Given theta, update alpha
                        update.result <- update_alpha.lasso(X, Y, Z, alpha.old = alpha.old,
                                                            sigma.square = sigma.square, theta = theta, maxstep_inner = maxstep_inner,
                                                            tolerance_inner = tolerance_inner)
                        alpha.new <- update.result$alpha.est

                        # Check convergence
                        if (sum(abs(alpha.new - alpha.old)) < tolerance) {
                                break
                        }
                        alpha.old <- alpha.new

                        # Track iteration progress
                        if (verbosity == TRUE) {
                                cat("#-----------------Iteration ", k, " Done -----------------#\n",
                                    sep = "")
                        }
                        k <- k + 1
                }
                tauEst = exp(Z %*% alpha.old)
                pen_vec = tauEst * sigma.square/n

                pen_vec[pen_vec > 1e3] <- Inf
                C = ifelse(is.nan(mean(pen_vec[pen_vec!=Inf])),1e5,mean(pen_vec[pen_vec!=Inf]))

                obj <- glmnet(X, Y, alpha = 1,family="gaussian",standardize = standardize, intercept = intercept)
                cus.coef <- coef(obj,x=X,y=Y,exact=TRUE,s= C, penalty.factor = pen_vec,standardize=standardize,intercept = intercept)

        }
        if (method == "ridge") {
                ##---------- ridge regression
                ## Initialize
                alpha.old = alpha.init
                likelihood.score = c()
                k = 1
                while (k < maxstep) {
                        # Given alpha, update theta
                        gamma = exp(-Z %*% alpha.old)  ## gamma is the variance of beta in this vase
                        Sigma_y = sigma.square * diag(n) + (t(t(X) * c(gamma))) %*% t(X)
                        theta = colSums(X * solve(Sigma_y, X))

                        # Compute likelihood
                        if (compute.likelihood == TRUE) {
                                likelihood.score = c(likelihood.score, approx_likelihood.ridge(alpha.old,
                                                                                               X, Y, Z, sigma.square))
                        }

                        # Given theta, update alpha
                        update.result <- update_alpha.ridge(X, Y, Z, alpha.old = alpha.old,
                                                            sigma.square = sigma.square, theta = theta, maxstep_inner = maxstep_inner,
                                                            tolerance_inner = tolerance_inner)
                        alpha.new <- update.result$alpha.est

                        # Check convergence
                        if (sum(abs(alpha.new - alpha.old)) < tolerance) {
                                break
                        }
                        alpha.old <- alpha.new

                        # Track iteration progress
                        if (verbosity == TRUE) {
                                cat("#-----------------Iteration ", k, " Done -----------------#\n",
                                    sep = "")
                        }
                        k <- k + 1
                }
                gamma = exp(-Z %*% alpha.old)
                pen_vec = 1/gamma * sigma.square/n

                pen_vec[pen_vec > 1e3] <- Inf
                C = ifelse(is.nan(mean(pen_vec[pen_vec!=Inf])),1e5,mean(pen_vec[pen_vec!=Inf]))

                obj <- glmnet(X, Y, alpha = 0,family="gaussian",standardize = standardize, intercept = intercept)
                cus.coef <- coef(obj,x=X,y=Y,exact=TRUE,s= C, penalty.factor = pen_vec,standardize=standardize,intercept = intercept)

        }
        return(list(beta.est = cus.coef, penalty.vector = pen_vec, lambda = C, alpha.est = alpha.old, method=method,
                    n_iter = k - 1, sigma.square = sigma.square, likelihood.score = likelihood.score))
}
