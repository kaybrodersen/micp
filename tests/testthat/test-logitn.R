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
  # F_logitnormal(x = 0.5 | mu = 2, sigma = 1.5)
  # = 1/2 * (1 + erf((logit(0.5) - 2) / sqrt(2*1.5^2))) = 0.09121122.
  expect_equal(round(logitncdf(0.5, 2, 1.5), 7), 0.0912112)
  expect_equal(round(logitncdf(c(.5, .5), 2, 1.5), 7), c(0.0912112, 0.0912112))
})

test_that("logitncdf is the inverse of logitninv", {
  x <- logitninv(0.5, 2, 1.5)
  expect_equal(round(logitncdf(x, 2, 1.5), 4), 0.5)
})

test_that("logitncdf is zero left of x = 0 irrespective of the parameters", {
  expect_equal(logitncdf(c(-0.5, 0), 0, 1), c(0, 0))
  expect_equal(logitncdf(c(-0.5, 0), 1, 1.5), c(0, 0))
  expect_equal(logitncdf(c(-0.5, 0), -1.5, 1.5), c(0, 0))
})

test_that("logitncdf is one right of x = 1 irrespective of the parameters", {
  expect_equal(logitncdf(c(1, 1.5), 0, 1), c(1, 1))
  expect_equal(logitncdf(c(1, 1.5), 1, 1.5), c(1, 1))
  expect_equal(logitncdf(c(1, 1.5), -1.5, 1.5), c(1, 1))
})

test_that("logitninv rejects bad input", {
  expect_error(logitninv("foo", 1, 2), "p is not a numeric or integer vector")
  expect_error(logitninv(1, "foo", 2), "mu is not a numeric or integer vector")
  expect_error(logitninv(1, 2, "x"), "sigma is not a numeric or integer vector")
  expect_error(logitninv(0.5, 1, 0), "sigma must be positive")
})

test_that("logitninv is the inverse of logitncdf", {
  # p = F_logitnormal(x = 0.5 | mu = 2, sigma = 1.5)
  # => F^-1_logitnormal(p | mu = 2, sigma = 1.5) = 0.5.
  p <- logitncdf(0.5, 2, 1.5)
  expect_equal(round(logitninv(p, 2, 1.5), 4), 0.5)
})

test_that("logitninv is 0 and 1 at its extremes", {
  expect_equal(logitninv(0, 0, 1), 0)
  expect_equal(logitninv(1, 0, 1), 1)
})

test_that("logitninv is NA outside of the [0, 1] interval", {
  expect_equal(logitninv(-0.1, 0, 1), NA_real_)
  expect_equal(logitninv(1.1, 0, 1), NA_real_)
})

test_that("logitninv works on vector input", {
  expect_equal(logitninv(c(0.5, 0.7), 0, 1),
               c(logitninv(0.5, 0, 1), logitninv(0.7, 0, 1)))
  expect_equal(logitninv(0.5, c(0, 0.6), 1),
               c(logitninv(0.5, 0, 1), logitninv(0.5, 0.6, 1)))
  expect_equal(logitninv(0.5, 0, c(1, 2)),
               c(logitninv(0.5, 0, 1), logitninv(0.5, 0, 2)))
  expect_equal(logitninv(c(0.5, 0.6), 0, c(1, 2)),
               c(logitninv(0.5, 0, 1), logitninv(0.6, 0, 2)))
})

test_that("logitnmean rejects bad input", {
  expect_error(logitnmean("foo", 0.8), "mu is not a numeric or integer vector")
  expect_error(logitnmean(0.5, "x"), "sigma is not a numeric or integer vector")
  expect_error(logitnmean(0.5, 0), "sigma must be positive")
})

test_that("logicnmean returns NA for NA parameters", {
  expect_equal(logitnmean(NA_real_, 0.6), NA_real_)
  expect_equal(logitnmean(0.2, NA_real_), NA_real_)
})

test_that("logitnmean is 0.5 for mu = 0", {
  expect_equal(logitnmean(0, 0.6), 0.5)
  expect_equal(logitnmean(0, 0.8), 0.5)
})

test_that("logitnmean matches the empirical expectation", {
  # Generate logit-normal variates (by drawing from a normal distribution and
  # applying the sigmoid transform). Then test that the expectation returned by
  # `logitnmean()` matches the sample mean.
  set.seed(1)
  x <- rnorm(1e5, 0.2, 0.6)
  y <- 1 / (1 + exp(-x))
  expect_equal(logitnmean(0.2, 0.6), mean(y), tolerance = 1e-3)
})
