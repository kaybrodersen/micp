test_that("logitnsumcdf rejects bad input", {
  expect_error(logitnsumcdf("foo", 0, 1, 0, 1),
               "not a numeric or integer vector")
  expect_error(logitnsumcdf(matrix(c(0, 0.1, 0.2, 0.3), nrow = 2), 0, 1, 0, 1),
               "not an atomic vector")
})

test_that("logitnsumcdf returns expected result", {
  expect_equal(logitnsumcdf(0, 0, 1, 0, 1), 0)
  expect_equal(round(logitnsumcdf(1, 0, 1, 0, 1), 2), 0.5)
  expect_equal(logitnsumcdf(2, 0, 1, 0, 1), 1)
})

test_that("logitnsumcdf works on vector input", {
  expect_equal(logitnsumcdf(c(0, 1, 2), 0, 1, 0.2, 0.8),
               c(logitnsumcdf(0, 0, 1, 0.2, 0.8),
                 logitnsumcdf(1, 0, 1, 0.2, 0.8),
                 logitnsumcdf(2, 0, 1, 0.2, 0.8)))
})
