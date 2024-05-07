library(mmpr)
library(purrr)
context("Test growth estimation API")

test_that("test growth.lm",{
  mu <- 0.08
  t <- 1:100

  d.biomass <- simulate_growth(t, mu, .error.range = 0.001, .error.sd = 0.1)

  # test with characters
  m <- growth.lm(d.biomass, "time_h", "OD")
  expect_equal(
    m$coefficients[[1]], -mu, tolerance = 0.001
  )
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )

  # test with variable names
  m <- growth.lm(d.biomass, time_h, OD)
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )

  # test with mixture
  m <- growth.lm(d.biomass, "time_h", OD)
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )
  m <- growth.lm(d.biomass, time_h, "OD")
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )

  # expect that getting the summary does not result in warnings
  expect_silent(
    summary(m)
  )

  expect_equal(
    summary(m)$r.squared, 1, tolerance = 0.001
  )

  # test with perfect fit
  d.biomass <- simulate_growth(t, mu, .error.range = 0, .error.sd = 0)
  m <- growth.lm(d.biomass, time_h, OD)
  # we expect a warning when
  expect_warning(
    summary(m)
  )

  # test with perfect fit (with default)
  d.biomass <- simulate_growth(t, mu)
  m <- growth.lm(d.biomass, time_h, OD)
  # we expect a warning when
  expect_warning(
    summary(m)
  )

})

test_that("test growth.ts",{
  mu <- 0.08
  t <- 1:100

  d.biomass <- simulate_growth(t, mu, .error.range = 0.001, .error.sd = 0.1)

  # test with characters
  m <- growth.ts(d.biomass, "time_h", "OD")
  expect_equal(
    m$coefficients[[1]], -mu, tolerance = 0.001
  )
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )

  # test with variable names
  m <- growth.ts(d.biomass, time_h, OD)
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )

  # test with mixture
  m <- growth.ts(d.biomass, "time_h", OD)
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )
  m <- growth.ts(d.biomass, time_h, "OD")
  expect_equal(
    m$coefficients[[2]], mu, tolerance = 0.001
  )
})

test_that("test growth.lsq",{
  mu <- 0.08
  t <- 1:100

  d.biomass <- simulate_growth(t, mu, .error.range = 0.001, .error.sd = 0.1)

  # test with characters
  m <- growth.lsq(d.biomass, "time_h", "OD")
  expect_equal(
    m$intercept, -mu, tolerance = 0.001
  )
  expect_equal(
    m$slope, mu, tolerance = 0.001
  )
  expect_equal(
    m$r.squared, 1, tolerance = 0.001
  )

  # test with variable names
  m <- growth.lsq(d.biomass, time_h, OD)
  expect_equal(
    m$slope, mu, tolerance = 0.001
  )

  # test with mixture
  m <- growth.lsq(d.biomass, "time_h", OD)
  expect_equal(
    m$slope, mu, tolerance = 0.001
  )
  m <- growth.lsq(d.biomass, time_h, "OD")
  expect_equal(
    m$slope, mu, tolerance = 0.001
  )
})

test_that("test simulate_growth", {
  mu <- 0.08
  t <- 1:100

  d.biomass <- simulate_growth(t, mu)
  expect_equal(unique(d.biomass$mu), mu)
  # test few points
  i <- 2
  expect_equal(
    d.biomass$OD[[i]], d.biomass$OD[[i-1]]*exp(mu * (t[[i]] - t[[i-1]]))
  )
  i <- 51
  expect_equal(
    d.biomass$OD[[i]], d.biomass$OD[[i-1]]*exp(mu * (t[[i]] - t[[i-1]]))
  )
  i <- 99
  expect_equal(
    d.biomass$OD[[i]], d.biomass$OD[[i-1]]*exp(mu * (t[[i]] - t[[i-1]]))
  )

  # test multiple mu's
  mu <- c(0.06, 0.07, 0.08)
  d.biomass <- simulate_growth(t, mu)
  expect_equal(unique(d.biomass$mu), mu)
  expect_equal(nrow(d.biomass), length(mu) * length(t))
})
