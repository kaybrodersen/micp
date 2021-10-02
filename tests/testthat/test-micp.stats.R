test_that("micp.stats rejects bad vector input", {
  expect_error(micp.stats(c(0, 0), cbind(c(80, 80), c(100, 100))),
               "ks and ns must have same dimensions")
  expect_error(micp.stats(5, c(10, 20)), "ks and ns must have same dimensions")
  expect_error(micp.stats(rbind(c(10, 20, 30), c(30, 40, 50)),
                          rbind(c(80, 80), c(100, 100))),
               "ks and ns must have same dimensions")
  expect_error(micp.stats(NULL, c(10, 20)),
               "ks and ns must have same dimensions")
  expect_error(micp.stats(c(90, 80), c(10, 100)), "ks cannot be bigger than ns")
  expect_error(micp.stats(c(-5, 80), c(100, 100)), "ks must be non-negative")
  expect_error(micp.stats(c(0, 0), c(0, 0)), "ns must not be all zero")
})

test_that("print.micp.stats() works as expected", {
  ks <- c(82,  75,  92,  85,  88)
  ns <- c(100, 100, 100, 100, 100)
  stats <- micp.stats(ks, ns)
  result <- print(stats)
  expected <-
"Variational Bayesian mixed-effects inference on classification
accuracy

Population inference
  posterior mean accuracy:    0.82 (p = 0)
  posterior 95% interval:     [0.72, 0.9]

Subject-specific inference
  posterior logit means:      1.52, 1.14, 2.27, 1.71, 1.93
  posterior logit precisions: 16.62, 20.25, 10.39, 14.86, 13

Bayesian model comparison
  free energy F: -20.28"
  expect_equal(result, expected)
})

test_that("Readme example 1 (accuracy) is protected", {
  ks <- c(82,  75,  92,  85,  88)
  ns <- c(100, 100, 100, 100, 100)
  stats <- micp.stats(ks, ns)
  expect_equal(stats,
               structure(list(mu = 0.82032710454459, p = 2.75599137011806e-07,
    ci = c(0.719635834669835, 0.896141401582931), q = list(mu.mu = 1.54878602041367,
        eta.mu = 10.4537530374307, a.lambda = 3.5, b.lambda = 0.543049708704979,
        mu.rho = c(1.51926357950875, 1.1396109966045, 2.26878726731442,
        1.70991565502155, 1.92549078835988), eta.rho = structure(c(16.6232112629893,
        20.2545516033541, 10.3861283514406, 14.8619686845336,
        12.9965332885574), .Dim = c(1L, 5L)), F = -20.2828862747299),
    model = "unb.vb"), class = "micp")
  )
})

test_that("Readme example 2 (balanced accuracy) is protected", {
  ks <- rbind(c(40, 44, 18, 42, 44), c(48, 41, 65, 49, 32))
  ns <- rbind(c(45, 51, 20, 46, 48), c(55, 49, 80, 54, 32))
  stats <- micp.stats(ks, ns)
  expect_equal(stats,
               structure(list(mu = 0.85604319744731, p = 4.27705153315168e-14,
    ci = c(0.792830361850701, 0.906300586991726), qp = list(mu.mu = 1.86980619354249,
        eta.mu = 10.7371337504752, a.lambda = 3.5, b.lambda = 0.556310999785524,
        mu.rho = c(2.01659820026315, 1.84594065093726, 2.03237011959905,
        2.19099919982749, 2.22326478622646), eta.rho = structure(c(6.61265411348049,
        7.95312731300912, 3.99593991211396, 6.10808209238122,
        6.17810629478053), .Dim = c(1L, 5L)), F = -15.0780771057361),
    qn = list(mu.mu = 1.77483245245463, eta.mu = 7.88026416561418,
        a.lambda = 3.5, b.lambda = 0.392938141573965, mu.rho = c(1.89790764076061,
        1.65832991669147, 1.497940973866, 2.16847857238434, 2.94130480123907
        ), eta.rho = structure(c(7.61060695729261, 7.96119677863841,
        13.3233758075488, 6.34873964217762, 2.90034572940184), .Dim = c(1L,
        5L)), F = -17.7387649038124), mu.phij = c(0.87040600575145,
    0.846362352466996, 0.844121101187142, 0.89260213656786, 0.919415872518673
    ), model = "tnb.vb"), class = "micp")
  )
})
