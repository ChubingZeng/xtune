context("test for each block of utility functions")
library(numDeriv)

set.seed(1234)
test_that("gradiant function match with numeric gradient", {
        X <- matrix(runif(100), nrow = 20)
        Y <- 1:20
        Z <- matrix(runif(10), nrow = 5)
        theta = as.vector(rnorm(ncol(X),0,1))
        delta = as.vector(rnorm(ncol(X),0,1))
        expect_equal(grad(likelihood.alpha.theta.lasso,rep(0,ncol(Z)),Z=Z,theta=theta,delta= delta),
                     as.vector(likelihood.alpha.theta.gradient.lasso(Z,rep(0,ncol(Z)),theta,delta)))
        expect_equal(grad(likelihood.alpha.theta.ridge,rep(0,ncol(Z)),Z=Z,theta=theta,delta= delta),
                     as.vector(likelihood.alpha.theta.gradient.ridge(Z,rep(0,ncol(Z)),theta,delta)))
}
)

