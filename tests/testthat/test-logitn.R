test_that("logitnpdf rejects bad input", {
  expect_error(logitnpdf(c(-1, 0, 1), c(1, 2), 1), "mu must be a scalar")
  expect_error(logitnpdf(c(-1, 0, 1), 1, c(1, 2)), "sigma must be a scalar")
  expect_error(logitnpdf(c(-1, 0, 1), 1, 0), "sigma must be positive")
})

test_that("logitnpdf works on healthy input", {
  # x = 0.5, mu = 2, sigma = 1:
  # 1/sqrt(2*pi) * exp(-((logit(0.5) - 2)^2)/2) / (0.5 * (1 - 0.5)) = 0.2159639.
  expect_equal(round(logitnpdf(0.5, 2, 1), 7), 0.2159639)
})

test_that("logitnpdf is zero outside of the (0, 1) interval", {
  expect_equal(logitnpdf(c(-0.5, 0, 1, 1.5), 0, 1), c(0, 0, 0, 0))
})
