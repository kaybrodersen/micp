test_that("repmat returns unaltered input as matrix by default", {
  expect_equal(repmat(1), matrix(1))
  expect_equal(repmat(matrix(c(1, 2, 3))), matrix(c(1, 2, 3)))
  expect_equal(repmat(c(1, 2, 3)), t(matrix(c(1, 2, 3))))
})

test_that("repmat replicates row-wise", {
  expect_equal(repmat(1, 3), matrix(c(1, 1, 1)))
  expect_equal(repmat(c(1, 2), 3), rbind(c(1, 2), c(1, 2), c(1, 2)))
})

test_that("repmat replicates column-wise", {
  expect_equal(repmat(1, 1, 3), t(matrix(c(1, 1, 1))))
  expect_equal(repmat(c(1, 2), 1, 3), t(matrix(c(1, 2, 1, 2, 1, 2))))
})
