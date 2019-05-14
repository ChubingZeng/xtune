context("test for misclassification()")

test_that("throw warning if missing values are found in pred or true", {
        a <- rbinom(5,1,0.5)
        a[1] <- NA
        b <- rbinom(5,1,0.5)
        expect_warning(misclassification(a,b))

        a <- rbinom(5,1,0.5)
        b <- rbinom(5,1,0.5)
        b[1] <- NA
        expect_warning(misclassification(a,b))
}
)

test_that("throw error if pred and true differ in length", {
        a <- rnorm(5,0,1)
        b <- rnorm(4,0,1)
        expect_error(misclassification(a,b))
}
)
