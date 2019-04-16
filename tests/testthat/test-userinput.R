context("test for user input check")

test_that("throw error when method not found", {
        X <- matrix(runif(100), nrow = 20)
        Y <- 1:20
        Z <- matrix(runif(10), nrow = 5)
        expect_error(ipreg(X,Y,Z, method = "badmethod"))
})

test_that("throw error when X or Z do not have at least 2 columns", {
        X <- matrix(runif(10), ncol = 1)
        Y <- 1:10
        Z <- matrix(runif(10), 1)
        expect_error(ipreg(X,Y,Z,method = "lasso"), "X must be a matrix with 2 or more columns")
})

test_that("throw error when dimensions of X and Y do not match", {
        X <- matrix(runif(100), nrow = 20)
        Y <- 1:10
        Z <- matrix(runif(10), nrow = 5)
        expect_error(ipreg(X,Y,Z,method = "lasso"), paste("number of observations in Y (", length(Y), ") not equal to the number of rows of X (",
                                                         nrow(X), ")", sep = ""),fixed = TRUE)
})

test_that("throw error when ncol(X) not equal to nrow(Z)", {
        X <- matrix(runif(10), ncol = 2)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 5)
        expect_error(ipreg(X,Y,Z,method = "lasso"), paste("number of rows in Z (", nrow(Z),
                                                          ") not equal to the number of columns in X (", ncol(X),
                                                          ")", sep = ""), fixed = TRUE)
})

test_that("throw error when Z matrix contains non-numeric value",{
        X <- matrix(runif(10), ncol = 2)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 2)
        Z[1,1] <- "0.88"
        expect_error(ipreg(X,Y,Z,method = "lasso"), paste("Z contains non-numeric values"))
})

test_that("throw error when invalid estimated sigma square is provided",{
        X <- matrix(runif(10), ncol = 2)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 2)
        expect_error(ipreg(X,Y,Z,sigma.square = -1,method = "lasso"), paste("sigma square should be a positive finite number"))
})

test_that("throw error when invalid estimated sigma square is provided",{
        X <- matrix(runif(10), nrow = 5)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 2)
        expect_error(ipreg(X,Y,Z,sigma.square = -1,method = "lasso"), paste("sigma square should be a positive finite number"))
})



