context("test estimate variance")
set.seed(1234)

test_that("estimateVariance works as intended", {
  X <- matrix(runif(500), ncol = 50)
  Y <- runif(10)

  res <- estimateVariance(X, Y)
  expect_length(res, 1)
  expect_type(res, "double")
  expect_true(is.finite(res))
  expect_true(res > 0 )
})

test_that("estimateVariance errors when input is wrong sizes", {
        X <- matrix(runif(900), nrow = 90)
        Y <- runif(100)
        expect_error(estimateVariance(X, Y))
})
