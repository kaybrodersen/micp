test_that("vbicp.unb rejects bad input", {
  ks <- c(6, 8, 5)
  ns <- c(10, 10, 10)
  expect_error(vbicp.unb(NULL, ns), "ks is not an atomic vector")
  expect_error(vbicp.unb("foo", ns), "ks is not a numeric or integer vector")
  expect_error(vbicp.unb(ks, NULL), "ns is not an atomic vector")
  expect_error(vbicp.unb(ks, "foo"), "ns is not a numeric or integer vector")
  expect_error(vbicp.unb(ks[1:2], ns), "length(ks) not equal to length(ns)",
               fixed = TRUE)
})

test_that("vbicp.unb warns when using the deprecated `verbose` flag", {
  ks <- c(6, 8, 5)
  ns <- c(10, 10, 10)
  expect_warning(vbicp.unb(ks, ns, verbose = 0), "deprecated")
  expect_warning(vbicp.unb(ks, ns, verbose = 1), "deprecated")
})

test_that("vbicp.unb is silent by default", {
  expect_silent(vbicp.unb(c(6, 7), c(10, 10)))
})

test_that("vbicp.unb works on minimal input (2 observations)", {
  expect_error(vbicp.unb(6, 10), "length(ks) not greater than or equal to 2",
               fixed = TRUE)
  ks <- c(6, 7)
  ns <- c(10, 10)
  expect_silent(q <- vbicp.unb(ks, ns))
})
