context("test for coefficient estimation")

test_that("test for coef",{
        X <- matrix(runif(30), ncol = 3)
        Y <- 1:10
        Z <- matrix(runif(6), nrow = 3)
        xtune_fitted <- xtune(X,Y,Z)
        expect_equal(coef(xtune_fitted),xtune_fitted$beta.est)
})
