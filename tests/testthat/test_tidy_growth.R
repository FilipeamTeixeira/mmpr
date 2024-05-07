library(mmpr)
library(purrr)
context("Test growth estimation API")

test_that("test tidy_growth on simple data",{

  mu <- 0.06
  t <- 1:50
  w_size <- 10

  d.test <- simulate_growth(t, mu, .error.range = 0.001)

  d.growth.raw <- slide_growth_window(d.test, time_h, OD, w_size = w_size, .tidy=F)

  expect_true( "data" %in% colnames(d.growth.raw))
  expect_true( "model" %in% colnames(d.growth.raw))

  d.growth <- d.growth.raw %>%
    mutate(tidy_data = map( model, tidy_slide_growth, x=time_h)) %>%
    unnest(tidy_data)

  expect_true( "data" %in% colnames(d.growth))
  expect_true( "model" %in% colnames(d.growth))
  expect_true( "slope" %in% colnames(d.growth))

})

test_that("test tidy_growth on data with NA model",{

  mu <- 0.06
  t <- 1:50
  w_size <- 10

  d.test <- simulate_growth(t, mu, .error.range = 0.001)

  d.growth.raw <- bind_rows(
    slide_growth_window(d.test, time_h, OD, w_size = w_size, .tidy=F),
    data.frame(time_h = max(t) + 1, data = NA, w_size = 0, model = NA)
  )

  expect_true("data" %in% colnames(d.growth.raw))
  expect_true("model" %in% colnames(d.growth.raw))

  d.growth <- d.growth.raw %>%
    mutate(tidy_data = map( model, tidy_slide_growth, x=time_h)) %>%
    unnest(tidy_data, keep_empty = T)

  expect_true("data" %in% colnames(d.growth))
  expect_true("model" %in% colnames(d.growth))
  expect_true("slope" %in% colnames(d.growth))

  expect_equal( max(t) + 1, max(d.growth$time_h))

})

test_that("test tidy_growth with custom model column",{

  mu <- 0.06
  t <- 1:50
  w_size <- 10

  d.test <- simulate_growth(t, mu, .error.range = 0.001)

  d.growth.raw <- slide_growth_window(d.test, time_h, OD, w_size = w_size, .tidy=F, .to=bla)

  expect_true("data" %in% colnames(d.growth.raw))
  expect_true("bla" %in% colnames(d.growth.raw))

  d.growth <- d.growth.raw %>%
    mutate(tidy_data = map( bla, tidy_slide_growth, x=time_h)) %>%
    unnest(tidy_data)

  expect_true("data" %in% colnames(d.growth))
  expect_true("bla" %in% colnames(d.growth))
  expect_true("slope" %in% colnames(d.growth))

})
