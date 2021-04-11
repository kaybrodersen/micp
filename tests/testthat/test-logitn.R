test_that("logitnpdf rejects bad input", {
  expect_error(logitnpdf("foo", 1, 2), "x is not a numeric or integer vector")
  expect_error(logitnpdf(1, "foo", 2), "mu is not a numeric or integer vector")
  expect_error(logitnpdf(1, 2, "x"), "sigma is not a numeric or integer vector")
  expect_error(logitnpdf(c(-1, 0, 1), 1, 0), "sigma must be positive")
  expect_error(logitnpdf(c(-1, 0, 1), 1, c(-1, 0)), "sigma must be positive")
})

test_that("logitnpdf works on healthy input", {
  # p_logitnormal(x = 0.5 | mu = 2, sigma = 1.5)
  # = 1/(1.5*sqrt(2*pi)) * exp(-((logit(0.5)-2)^2)/(2*1.5^2)) / (0.5 * (1-0.5))
  # = 0.4373602.
  expect_equal(round(logitnpdf(0.5, 2, 1.5), 7), 0.4373602)
  expect_equal(round(logitnpdf(c(.5, .5), 2, 1.5), 7), c(0.4373602, 0.4373602))
})

test_that("logitnpdf is zero outside of the (0, 1) interval", {
  expect_equal(logitnpdf(c(-0.5, 0, 1, 1.5), 0, 1), c(0, 0, 0, 0))
})

test_that("logitncdf rejects bad input", {
  expect_error(logitncdf("foo", 1, 2), "x is not a numeric or integer vector")
  expect_error(logitncdf(1, "foo", 2), "mu is not a numeric or integer vector")
  expect_error(logitncdf(1, 2, "x"), "sigma is not a numeric or integer vector")
  expect_error(logitncdf(c(-1, 0, 1), 1, 0), "sigma must be positive")
  expect_error(logitncdf(c(-1, 0, 1), 1, c(-1, 0)), "sigma must be positive")
})

test_that("logitncdf works on healthy input", {
  # F_logitnormal(x = 0.5 | mu = 2, sigma = 1.5) =
  # 1/2 * (1 + erf((logit(0.5) - 2) / sqrt(2*1.5^2))) = 0.09121122.
  expect_equal(round(logitncdf(0.5, 2, 1.5), 7), 0.0912112)
  expect_equal(round(logitncdf(c(.5, .5), 2, 1.5), 7), c(0.0912112, 0.0912112))
})

test_that("logitnpdf is zero left of x = 0 irrespective of the parameters", {
  expect_equal(logitncdf(c(-0.5, 0), 0, 1), c(0, 0))
  expect_equal(logitncdf(c(-0.5, 0), 1, 1.5), c(0, 0))
  expect_equal(logitncdf(c(-0.5, 0), -1.5, 1.5), c(0, 0))
})

test_that("logitnpdf is one right of x = 1 irrespective of the parameters", {
  expect_equal(logitncdf(c(1, 1.5), 0, 1), c(1, 1))
  expect_equal(logitncdf(c(1, 1.5), 1, 1.5), c(1, 1))
  expect_equal(logitncdf(c(1, 1.5), -1.5, 1.5), c(1, 1))
})
