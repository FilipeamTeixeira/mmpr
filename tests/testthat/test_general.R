library(mmpr)
context("General API")

test_that("test stamp_filename", {
  fn <- "bla.csv"
  date_prefix <- strftime(Sys.Date(), format = "%Y%m%d")
  expect_equal(stamp_filename(fn), sprintf("%s_%s", date_prefix, fn))

  fn <- "bla.csv"
  date_prefix <- strftime(Sys.Date(), format = "%Y-%m-%d")
  expect_equal(stamp_filename(fn, .stamp.format = "%Y-%m-%d"), sprintf("%s_%s", date_prefix, fn))
})
