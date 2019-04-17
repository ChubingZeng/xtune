context("test for prediction")

test_that("test for predict",{
        X <- matrix(runif(100), nrow = 20)
        Y <- 1:20
        Z <- matrix(runif(10), nrow = 5)
        newX <- matrix(runif(100), nrow = 10)
        ipreg_fitted <- ipreg(X,Y,Z)
        expect_error(predict(ipreg_fitted,newX),paste("New X does not have the same number of columns as X train"))

        X <- matrix(runif(100), nrow = 20)
        Y <- 1:20
        Z <- matrix(runif(10), nrow = 5)
        newX <- matrix(runif(100), nrow = 20)
        ipreg_fitted <- ipreg(X,Y,Z)
        expect_equal(matrix(predict(ipreg_fitted,newX)),matrix(cbind(1,newX)%*%(ipreg_fitted$beta.est)))
})
