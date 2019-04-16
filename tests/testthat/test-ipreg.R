#' @import lbfgs
library(lbfgs)
context("test for ipreg for both empirical bayes tuning and customized")

set.seed(1234)
test_that("estimates by empirical bayes tuning and lbfgs direct update match, single tuning parameters", {
        n = 50
        p = 100
        X <- matrix(rnorm(2*n*p,0,1),nrow=2*n,ncol=p)
        betas=rnorm(n = p, s = 1)
        Y <- X%*%betas + rnorm(2*n,0,1)
        Z_int = matrix(rep(1,p),ncol = 1)
        sigma.square.est = estimateVariance(X,Y)
        ipreg.reweighted = ipreg(X,Y,sigma.square = sigma.square.est,control = list(tolerance = 1e-4,tolerance_inner=1e-4))
        lbfgs_result=lbfgs(approx_likelihood, score_function, input_X = X, input_Y = Y, input_Z = Z_int,
                           sigma2_est = sigma.square.est, rep(0,ncol(Z_int)), invisible = 1,epsilon = 1e-4)
        expect_equal(-ipreg.reweighted$likelihood.score[length(-ipreg.reweighted$likelihood.score)],lbfgs_result$value,tolerance=1e-4)
        expect_equal(mean(sum(lbfgs_result$par-ipreg.reweighted$alpha.hat)^2),0,tolerance=1e-4)
        expect_length(ipreg.reweighted$tuningvector,1)
}
)


test_that("estimates by ipreg reweighted-L2 and lbfgs match, multiple tuning parameters", {
        n = 50
        p = 100
        q = 3
        X <- matrix(rnorm(2*n*p,0,1),nrow=2*n,ncol=p)
        betas=rnorm(n = p, s = 1)
        Y <- X%*%betas + rnorm(2*n,0,1)
        Z= matrix(rnorm(p*q,0,1),ncol=q,nrow=p)
        Z_int = cbind(1,Z)
        sigma.square.est = estimateVariance(X,Y)
        ipreg.reweighted = ipreg(X,Y,Z,sigma.square = sigma.square.est,control = list(tolerance = 1e-4,tolerance_inner=1e-4))
        lbfgs_result=lbfgs(approx_likelihood, score_function, input_X = X, input_Y = Y, input_Z = Z_int,
                           sigma2_est = sigma.square.est, rep(0,ncol(Z_int)), invisible = 1,epsilon = 1e-4)
        expect_equal(-ipreg.reweighted$likelihood.score[length(-ipreg.reweighted$likelihood.score)],lbfgs_result$value,tolerance=1e-4)
        expect_equal(mean(sum(lbfgs_result$par-ipreg.reweighted$alpha.hat)^2),0,tolerance=1e-4)
        }
)

test_that("update lasso and update ridge are equivalent for alpha estimate", {
        n = 100
        p = 20
        q = 2
        X <- matrix(rnorm(2*n*p,0,1),nrow=2*n,ncol=p)
        betas=rnorm(n = p, s = 1)
        Y <- X%*%betas + rnorm(2*n,0,1)
        Z= matrix(rnorm(p*q,0,1),ncol=q,nrow=p)
        sigma.square.est = estimateVariance(X,Y)
        out.lasso=ipreg(X,Y,Z,sigma.square = sigma.square.est,method = "lasso",control = list(tolerance = 1e-4,tolerance_inner=1e-4))
        out.ridge=ipreg(X,Y,Z,sigma.square = sigma.square.est,method = "ridge",control = list(tolerance = 1e-4,tolerance_inner=1e-4))
        expect_equal(mean(sum(log(2) - 2*cbind(1,Z)%*%out.lasso$alpha.hat + cbind(1,Z)%*%out.ridge$alpha.hat)^2),0,tolerance = 1e-4)
}
)
