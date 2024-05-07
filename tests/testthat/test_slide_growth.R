library(mmpr)
library(purrr)
context("Test sliding window API")

test_that("test slide_growth on simple data", {
  mu <- 0.06
  t <- 1:50
  w_size <- 10

  d.test <- simulate_growth(t, mu, .error.range = 0.001)

  d.growth <- slide_growth_window(d.test, time_h, OD, w_size = w_size)

  expect_true("slope" %in% colnames(d.growth))
  expect_equal(nrow(d.growth), length(t))
  expect_equal(mean(d.growth$slope), mu, tolerance = 0.001)

  # test with variable names as characters
  d.growth <- slide_growth_window(d.test, "time_h", "OD", w_size = w_size)

  expect_true("slope" %in% colnames(d.growth))
  expect_equal(nrow(d.growth), length(t))
  expect_equal(mean(d.growth$slope), mu, tolerance = 0.001)

  # test with .to name as character
  d.growth <- slide_growth_window(
    d.test, "time_h", "OD", w_size = w_size, .tidy = FALSE, .to="bla"
  )

  expect_true("bla" %in% colnames(d.growth))

  # test with .to name as variable
  d.growth <- slide_growth_window(
    d.test, "time_h", "OD", w_size = w_size, .tidy = FALSE, .to=bla
  )

  expect_true("bla" %in% colnames(d.growth))
})

test_that("test slide_growth on grouped data", {
  mu <- c(0.06, 0.07, 0.08)
  t <- 1:50
  w_size <- 10

  d.test <- map(mu, simulate_growth, t=t, .error.range=0.001) %>%
    bind_rows()

  d.growth <- d.test %>%
    group_by(mu) %>%
    do( slide_growth_window(., time_h, OD, w_size = w_size) )

  expect_true("slope" %in% colnames(d.growth))
  expect_equal(nrow(d.growth), length(mu) * length(t) )

  # test that the slope matches the mu of each group
  f <- function(m) {
    d <- d.growth %>% filter(mu == m)
    mean(d$slope) -  m < 0.001
  }

  expect_true(all(map_lgl(mu, f)))

})

test_that("test slide_growth on combined data", {
  mu1 <- 0.06
  mu2 <- 0.07
  t <- 1:20
  w_size <- 5

  d1 <- simulate_growth(t, mu1, .error.range = 0.0001)
  d2 <- simulate_growth(t + length(t), mu2, initial = last(d1$OD), .error.range = 0.0001)

  db <- bind_rows(d1, d2)
  d.growth <- db %>%
    slide_growth_window(time_h, OD, w_size=w_size) %>%
    ungroup()

  # we expect the first points to approximate 0.06
  stable_range <- 1:(length(t) - ceiling(w_size / 2))
  expect_lt(
   sd(d.growth %>% slice(stable_range) %>% pull(slope)), 0.001
  )
  expect_equal(
   mean((d.growth %>% slice(stable_range))$slope, na.rm=T), mu1, tolerance = 0.001
  )

  # we expect the middle to be unstable
  unstable_range <- (length(t) - ceiling(w_size / 2)):(length(t) + ceiling(w_size / 2))
  expect_gt(
   sd((d.growth %>% slice(unstable_range))$slope, na.rm=T), 0.001
  )

  # we expect the end to be stable again
  stable_range <- (length(t) + ceiling(w_size / 2)):(length(t)*2)
  expect_lt(
   sd((d.growth %>% slice(stable_range))$slope, na.rm=T), 0.001
  )
  expect_equal(
   mean((d.growth %>% slice(stable_range))$slope, na.rm=T), mu2, tolerance = 0.001
  )

})

test_that("test slide_growth on pycultivator", {
  # apply on pycultivator
  fp <- find_pycultivator_data(".", type="csv")$path[[2]]
  df <- load_pycultivator_data(fp, type="csv")
  expect_gt(nrow(df), 0)
  suppressWarnings(
    df.rates <- df %>%
      filter(od_led != 680, time_h < 50) %>%
      slide_growth_window(time_h, OD)
  )
  expect_gt(
    df.rates %>%
    nrow(), 0
  )
})

test_that("test slide_growth.lm", {
  # create test dataset
  mu <- c(0.06, 0.07, 0.08)
  t <- 0:50
  df <- simulate_growth(t, mu, .error.range = 0.001)
  df.rates <- df %>%
    group_by(mu) %>%
    do( slide_growth_window(., time_h, OD, method="lm") )

  df.test <- df.rates %>%
    summarise(mu.mean = mean(mu))

  expect_true(
    all(df.test$mu - df.test$mu.mean < 0.00001)
  )
})

test_that("test slide_growth_window.ts", {
  # create test dataset
  mu <- c(0.06, 0.07, 0.08)
  t <- 0:50
  df <- simulate_growth(t, mu, .error.range = 0.001)

  df.rates <- df %>%
    group_by(mu) %>%
    do( slide_growth_window(., time_h, OD, method="ts"))

  df.test <- df.rates %>%
    summarise(mu.mean = mean(mu))

  expect_true(
    all(df.test$mu - df.test$mu.mean < 0.00001)
  )
})

test_that("test slide_growth_window.lsq", {
  # create test dataset
  mu <- c(0.06, 0.07, 0.08)
  t <- 0:50
  df <- simulate_growth(t, mu, .error.range = 0.001)
  df.rates <- df %>%
    group_by(mu) %>%
    do( slide_growth_window(., time_h, OD, method="lsq"))

  df.test <- df.rates %>%
    summarise(mu.mean = mean(mu))

  expect_true(
    all(df.test$mu - df.test$mu.mean < 0.00001)
  )
})

test_that("test slide_growth_windows", {
  # create test dataset
  mu <- c(0.06)
  t <- 1:50
  d.test <- simulate_growth(t, mu, .error.range = 0.001)
  w_sizes <- c(4, 5, 10)

  # create dataset
  d.growth <- d.test %>%
    slide_growth_windows(time_h, OD, w_sizes=w_sizes)

  # run tests
  expect_equal(
    nrow(d.growth),
    # 2 windows are moved for w_size 4 (i.e. 3 points) and 3 for w_size 5 (2)
    length(t) * length(mu) * length(w_sizes) - 2
  )
})

## Do benchmarking on rollapply vs slider with map and linear models
if(FALSE) {
  library(microbenchmark)
  library(dplyr)
  library(purrr)

  # test dataset
  mu <- data.frame( mu = c(0.07, 0.06, 0.08))
  t <- seq(0, 1000)
  df <- map_dfr(
    mu, simulate_growth, t=t, .error.rate = 0.01
  )

  # benchmark
  microbenchmark(
    # roll_window.data.frame(df %>% group_by(mu), min_size=10, max_size=10),
    slide_growth_window(df %>% group_by(mu), time_h, OD,  w_size = 10),
    slide_growth_window(df %>% group_by(mu), time_h, OD, w_size = 10, method="ts"),
    slide_growth_window(df %>% group_by(mu), time_h, OD, w_size = 10, method="lsq"),
    times = 10
  )

  # 11-02-2018 (100 timepoints, 10 point window size)
  # (n = 10)   min       lq     mean   median       uq      max
  # lm  = 1.834779 1.886371 1.919355 1.928894 1.951307 2.005578
  # ts  = 1.781273 1.805944 1.883788 1.834687 1.914629 2.288441
  # lsq = 1.283224 1.317137 1.366656 1.339496 1.371183 1.674734

  # 11-02-2018 (1000 timepoints, 10 point window size)
  # (n = 10)   min       lq     mean   median       uq      max
  # lm  = 17.63337 17.74609 17.91656 17.79245 18.11788 18.36498
  # ts  = 16.50141 16.53895 16.66797 16.62372 16.84351 16.87758
  # lsq = 11.46567 11.55824 11.65580 11.62823 11.73883 11.90041
}
