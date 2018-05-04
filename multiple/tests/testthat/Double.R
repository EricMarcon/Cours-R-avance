testthat::context("Double")

# Random integer vector
x <- stats::rpois(10, lambda = 100)

testthat::test_that("Default and integer S3 methods give the same result", {
  testthat::skip_on_cran()
  testthat::expect_equal(Double(x), Double(as.numeric(x)))
})


testthat::test_that("Double(integer) is integer", {
  testthat::skip_on_cran()
  testthat::expect_is(Double(x), "integer")
})
