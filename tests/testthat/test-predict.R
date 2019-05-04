context("test for prediction")

test_that("test for predict",{
        X <- matrix(runif(100), nrow = 20)
        Y <- 1:20
        Z <- matrix(runif(10), nrow = 5)
        newX <- matrix(runif(100), nrow = 10)
        xtune_fitted <- xtune(X,Y,Z)
        expect_error(predict(xtune_fitted,newX),paste("New X does not have the same number of columns as X train"))

        X <- matrix(runif(100), nrow = 20)
        Y <- 1:20
        Z <- matrix(runif(10), nrow = 5)
        newX <- matrix(runif(100), nrow = 20)
        xtune_fitted <- xtune(X,Y,Z)
        expect_equal(matrix(predict(xtune_fitted,newX)),matrix(cbind(1,newX)%*%(xtune_fitted$beta.est)))
})
