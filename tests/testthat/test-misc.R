test_that("repmat returns unaltered input as matrix by default", {
  expect_equal(repmat(1), matrix(1))
  expect_equal(repmat(matrix(c(1, 2, 3))), matrix(c(1, 2, 3)))
  expect_equal(repmat(c(1, 2, 3)), t(matrix(c(1, 2, 3))))
})
