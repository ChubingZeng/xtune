context("test for user input check")

set.seed(1234)
test_that("throw error when method not found", {
        X <- matrix(runif(50), nrow = 10)
        Y <- 1:10
        Z <- matrix(runif(10), nrow = 5)
        expect_error(xtune(X,Y,Z, method = "badmethod"))
})

test_that("throw error when X or Z do not have at least 2 columns", {
        X <- matrix(runif(10), ncol = 1)
        Y <- 1:10
        Z <- matrix(runif(10), 1)
        expect_error(xtune(X,Y,Z), "X must be a matrix with 2 or more columns")
})

test_that("throw error when dimensions of X and Y do not match", {
        X <- matrix(runif(50), nrow = 10)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 5)
        expect_error(xtune(X,Y,Z), paste("number of observations in Y (", length(Y), ") not equal to the number of rows of X (",
                                                         nrow(X), ")", sep = ""),fixed = TRUE)
        Y = 1:10
        Z <- matrix(rep(2,10), nrow = 5)
        expect_warning(xtune(X,Y,Z),"All rows in Z are the same, this Z matrix is not useful, EB tuning will be performed to estimate
                                      a single tuning parameter")
})

test_that("throw error when ncol(X) not equal to nrow(Z)", {
        X <- matrix(runif(10), ncol = 2)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 5)
        expect_error(xtune(X,Y,Z), paste("number of rows in Z (", nrow(Z),
                                                          ") not equal to the number of columns in X (", ncol(X),
                                                          ")", sep = ""), fixed = TRUE)
})

test_that("throw error when Z matrix contains non-numeric value",{
        X <- matrix(runif(10), ncol = 2)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 2)
        Z[1,1] <- "0.88"
        expect_error(xtune(X,Y,Z), paste("Z contains non-numeric values"))
})

test_that("throw error when invalid estimated sigma square is provided",{
        X <- matrix(runif(10), ncol = 2)
        Y <- 1:5
        Z <- matrix(runif(10), nrow = 2)
        expect_error(xtune(X,Y,Z,sigma.square = -1), paste("sigma square should be a positive finite number"))
})

test_that("throw error with wrong Y",{
        X <- matrix(runif(10), nrow = 5)
        Y <- as.character(1:5)
        expect_error(xtune(X,Y,sigma.square = -1), paste("Y must be a quantitive vector or a 0/1 binary factor variable"))
        Y <- rep(c(2,3),each = 3)
        expect_error(xtune(X,Y), paste("Y only has two unique values, but they are not 0/1"))
        Y <- as.factor(rep(c(2,3),each = 3))
        expect_error(xtune(X,Y), paste("Y is binary variable, please use 0/1 encoding"))
})



