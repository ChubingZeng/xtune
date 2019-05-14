context("test estimate variance")
set.seed(1234)

test_that("estimateVariance works as intended", {
  X <- matrix(runif(30), ncol = 3)
  Y <- runif(10)

  res <- estimateVariance(X, Y)
  expect_length(res, 1)
  expect_type(res, "double")
  expect_true(is.finite(res))
  expect_true(res > 0 )
})

test_that("estimateVariance errors when input is wrong sizes", {
        X <- matrix(runif(30), nrow = 10)
        Y <- runif(5)
        expect_error(estimateVariance(X, Y))
})
