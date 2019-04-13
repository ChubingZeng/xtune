context("test for user input check")

test_that("throw error when matching family not found", {

        x <- matrix(runif(10), nrow = 5)
        y <- 1:5
        external <- matrix(runif(10), nrow = 2)
        expect_error(xrnet(x, y, external, family = "badfamily"))
})

test_that("throw error when x or ext do not have at least 2 columns", {
        x <- matrix(runif(10), ncol = 1)
        y <- 1:10
        external <- matrix(runif(10), 1)
        expect_error(xrnet(x, y, external, family = "gaussian"), "Error: x must have at least 2 columns")
})

test_that("throw error when dimensions of x and y do not match", {
        x <- matrix(runif(10), ncol = 2)
        y <- 1:10
        external <- matrix(runif(10), nrow = 2)
        expect_error(xrnet(x, y, external, "gaussian"), "Error: Length of y (10) not equal to the number of rows of x (5)", fixed = TRUE)
})

test_that("throw error when ncol(x) not equal to nrow(external)", {
        x <- matrix(runif(10), ncol = 2)
        y <- 1:5
        external <- matrix(runif(10), nrow = 5)
        expect_error(xrnet(x, y, external, "gaussian"), "Error: Number of columns in x (2) not equal to the number of rows in external (5)", fixed = TRUE)
})
