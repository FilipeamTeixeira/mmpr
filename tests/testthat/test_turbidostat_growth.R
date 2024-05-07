# library(mmpr)
library(purrr)
library(dplyr)
context("Turbidostat Growth Analysis API")

data.file.correct <- "latest_20170113_MC0_AC_measurements.db"

test_that("test turbidostat_growth with simple data", {

  t <- 1:10
  mu <- c(0.06, 0.05, 0.08, 0.059)
  decision <- 1:4

  f <- function(m, d) {
    simulate_growth(t, m, .error.range = 0.001) %>%
      mutate(decision = d)
  }

  d <- map2(mu, decision, f) %>%
    bind_rows()

  # using linear model
  d.growth <- turbidostat_growth(d, time_h, OD)
  expect_true( "slope" %in% colnames(d.growth))
  expect_true( "w_size" %in% colnames(d.growth))
  expect_true( "time_h" %in% colnames(d.growth))

  # test values obtained
  expect_equal(mean(d.growth$time_h), 10)
  expect_equal(mean(d.growth$w_time), 9)
  expect_equal(
    d.growth$slope, mu, tolerance = 0.001
  )

  # reorder data
  randomly <- function(x) sample(xtfrm(x))
  d.growth <- d %>%
    ungroup() %>%
    arrange(randomly(OD)) %>%
    group_by(mu) %>%
    arrange(decision) %>%
    turbidostat_growth(time_h, OD)

  # test values obtained
  expect_equal(mean(d.growth$time_h), 10)
  expect_equal(mean(d.growth$w_time), 9)
  expect_equal(
    d.growth$slope, mu, tolerance = 0.001
  )

  # using theil-sen
  d.growth <- turbidostat_growth(d, time_h, OD, method="ts")
  expect_equal(
    d.growth$slope, mu, tolerance = 0.001
  )

  # using least squares
  d.growth <- turbidostat_growth(d, time_h, OD, method="lsq")
  expect_equal(
    d.growth$slope, mu, tolerance = 0.001
  )

  # decision stored as 'p' - lazy
  expect_equal(
    d %>%
      rename( p = decision) %>%
      turbidostat_growth(time_h, OD, .group = p) %>%
      pull(slope),
    mu, tolerance = 0.001
  )

  # decision stored as 'p' - string
  expect_equal(
    d %>%
      rename( p = decision) %>%
      turbidostat_growth(time_h, OD, .group = "p") %>%
      pull(slope),
    mu, tolerance = 0.001
  )

})



test_that("test turbdistat_growth with pycultivator", {

  d.od <- load_pycultivator_sqlite(data.file.correct)
  d.tb <- load_turbidostat_sqlite(data.file.correct)

  # combine the datasets
  d <- combine_turbidostat(d.od, d.tb) %>%
    filter(!is.na(decision))

  # now calculate growth rates
  library(dplyr)

  df.lm.rates <- d %>%
    group_by(channel) %>%
    turbidostat_growth(time_h, OD, method="lm")

  df.ts.rates <- d %>%
    group_by(channel) %>%
    turbidostat_growth(time_h, OD, method="ts")

  expect_equal(
    df.lm.rates %>%
      nrow(),
    df.ts.rates %>%
      nrow()
  )

  # test on perfect dataset
  data <- data.frame(
    time_h = seq(1:10) * (5/60)
  )

})
