context("test for prediction")

test_that("test for predict",{
        X <- matrix(runif(50), nrow = 10)
        Y <- 1:10
        Z <- matrix(runif(10), nrow = 5)
        xtune_fitted <- xtune(X,Y,Z)
        expect_error(predict(xtune_fitted),paste("You need to supply a value for 'newX'"))
        newX <- matrix(runif(25), nrow = 5)
        newX[1,1] <-"1"
        expect_error(predict(xtune_fitted,newX = newX),paste("New X contains non-numeric values"))

        newX <- matrix(runif(20), nrow = 5)
        expect_error(predict(xtune_fitted,newX),paste("New X does not have the same number of columns as X train"))

        newX <- matrix(runif(25), nrow = 5)
        expect_equal(matrix(predict(xtune_fitted,newX)),matrix(cbind(1,newX)%*%(xtune_fitted$beta.est)))

        Y <- rbinom(10,1,0.5)
        xtune_fitted <- xtune(X,Y,Z)
        expect_error(predict(xtune_fitted,newX = newX,type = "class"),paste("You need to supply the original X and Y with type = 'class'"))
        expect_equal(sum(!predict(xtune_fitted,newX = newX,type = "class",X=X,Y=Y)%in%c(0,1)),0)
})
