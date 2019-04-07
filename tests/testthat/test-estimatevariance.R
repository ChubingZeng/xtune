context("test-estimatevariance")
set.seed(1234)

test_that("estimateVariance works as intended", {
  X <- matrix(runif(1000), ncol = 10)
  Y <- runif(100)

  res <- estimateVariance(X, Y)
  expect_length(res, 1)
  expect_type(res, "double")
})

test_that("estimateVariance errors when input is wrong sizes", {
        X <- matrix(runif(900), nrow = 90)
        Y <- runif(100)

        expect_error(estimateVariance(X, Y))
})
