test_that("safesigm generally corresponds to the sigmoid function", {
  # sigmoid(1) = 1 / (1 + exp(-1)) ~= 0.7310586.
  expect_equal(safesigm(1), 0.7310586, tolerance = 1e-6)
})

test_that("safesigm returns at least 1e-8", {
  # sigmoid(-100) = 1 / (1 + exp(100)) << 1e-8 => 1e-8.
  expect_equal(safesigm(-100), 1e-8)
})

test_that("safesigm works on vector input", {
  expect_equal(safesigm(c(1, -100)), c(0.7310586, 1e-8), tolerance = 1e-6)
})

test_that("trapz rejects invalid input", {
  expect_error(trapz(0, 1), "not greater than or equal to 2")
  expect_error(trapz(c(0, 1), 1), "length(y) not equal to length(x)",
               fixed = TRUE)
})

test_that("trapz returns expected result on healthy input", {
  # Area of a triangle between (0, 0), (0, 1), (1, 1) is 0.5.
  expect_equal(trapz(c(0, 1), c(0, 1)), 0.5)
  # Triangle plus an additional square: 1.5.
  expect_equal(trapz(c(0, 1, 2), c(0, 1, 1)), 1.5)
})

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

test_that("repmat works in both directions", {
  expect_equal(repmat(1, 2, 3), rbind(c(1, 1, 1), c(1, 1, 1)))
  expect_equal(repmat(c(1, 2), 2, 3),
               rbind(c(1, 2, 1, 2, 1, 2), c(1, 2, 1, 2, 1, 2)))
})
