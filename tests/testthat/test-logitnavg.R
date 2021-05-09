test_that("logitnavgcdf rejects bad input", {
  expect_error(logitnavgcdf("foo", 0, 1, 0, 1),
               "not a numeric or integer vector")
  expect_error(logitnsumcdf(matrix(c(0, 0.1, 0.2, 0.3), nrow = 2), 0, 1, 0, 1),
               "not an atomic vector")
})

test_that("logitnavgcdf returns expected result", {
  expect_equal(logitnavgcdf(c(0.8, 0.9), 0.1, 1.3, -0.7, 1.1),
               logitnsumcdf(c(1.6, 1.8), 0.1, 1.3, -0.7, 1.1))
  expect_equal(logitnavgcdf(0, 0, 1, 0, 1), 0)
  expect_equal(round(logitnavgcdf(0.5, 0, 1, 0, 1), 2), 0.5)
  expect_equal(logitnavgcdf(1, 0, 1, 0, 1), 1)
})

test_that("logitnavginv is the inverse of logitnavgcdf", {
  q <- logitnavgcdf(0.6, 2.2, 1.3, -1.7, 1.1)
  expect_equal(logitnavginv(q, 2.2, 1.3, -1.7, 1.1), 0.6, tolerance = 1e-4)
})
