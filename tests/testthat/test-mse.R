context("test for mse()")

test_that("throw warning if missing values are found in pred or true", {
      a <- rnorm(5,0,1)
      a[1] <- NA
      b <- rnorm(5,0,1)
      expect_warning(mse(a,b))

      a <- rnorm(5,0,1)
      b <- rnorm(5,0,1)
      b[1] <- NA
      expect_warning(mse(a,b))
}
)

test_that("throw error if pred and true differ in length", {
        a <- rnorm(5,0,1)
        b <- rnorm(4,0,1)
        expect_error(mse(a,b))
}
)
