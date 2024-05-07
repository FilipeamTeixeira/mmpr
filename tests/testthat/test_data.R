library(mmpr)
context("Data processing API")

# test_that("test parse_date", {
#   t_now <- as.POSIXct("2009-08-01 00:00:00")
#   fmt <- "%Y-%m-%d %H:%M:%S"
#   expect_equal(parse_date( strftime(t_now, fmt), fmt), t_now)
#   expect_equal(parse_date(strftime(t_now, fmt), "%Y-%m"), as.POSIXct(NA))
# })

test_that("test extract_t_zero", {
  fn <- "latest_20160413_mc0_jj_measurements.csv"
  expect_equal(extract_t_zero(fn), "20160413")
  fn <- "latest_2016_mc0_jj_measurements.csv"
  expect_equal(extract_t_zero(fn), "2016")
  fn <- "latest_2016_measurements.csv"
  expect_equal(extract_t_zero(fn), NA)
  fn <- "latest_20160304_measurements.csv"
  expect_equal(extract_t_zero(fn), NA)
  fn <- "latest_2016__measurements.csv"
  expect_equal(extract_t_zero(fn), "2016")
  fn <- "20160413"
  expect_equal(extract_t_zero(fn), NA)
})

test_that("test extract_project_id", {
  fn <- "latest_20160413_mc0_jj_measurements.csv"
  expect_equal(extract_project_id(fn), "mc0_jj")
  fn <- "latest_2016_mc0_jj_measurements.csv"
  expect_equal(extract_project_id(fn), "mc0_jj")
  fn <- "latest_2016_measurements.csv"
  expect_equal(extract_project_id(fn), NA)
  fn <- "latest_20160304_measurements.csv"
  expect_equal(extract_t_zero(fn), NA)
  fn <- "latest_2016__measurements.csv"
  expect_equal(extract_project_id(fn), "")
  fn <- "mc0_jj_measurements.csv"
  expect_equal(extract_project_id(fn), NA)
})

test_that("test find_pycultivator_data", {
  expect_equal(
    nrow(find_pycultivator_data(".")), 4
  )
  expect_equal(
    nrow(find_pycultivator_data(".", c("csv", "sqlite"))), 4
  )
  expect_setequal(
    find_pycultivator_data(".")$project_id,
    c("mc2_wd", "MC2_MV", "mc2_wd", "MC0_AC")
  )
  expect_setequal(
    find_pycultivator_data(".", "csv")$project_id,
    c("mc2_wd", "MC2_MV")
  )
  expect_equal(
    find_pycultivator_data(".", "sqlite")$project_id,
    c("mc2_wd", "MC0_AC")
  )
  expect_equal(
    nrow(find_pycultivator_data("..")), 0
  )
  expect_equal(
    nrow(find_pycultivator_data("./data")), 0
  )
})

test_that("test find_pycultivator_csv", {
  expect_equal(
    nrow(find_pycultivator_csv(".")), 2
  )
  expect_setequal(
    find_pycultivator_csv(".")$project_id,
    c("mc2_wd", "MC2_MV")
  )
  expect_equal(nrow(find_pycultivator_csv("..")), 0)
  expect_equal(nrow(find_pycultivator_csv("./data")), 0)
})

test_that("test find_pycultivator_sqlite", {
  expect_equal(nrow(find_pycultivator_sqlite(".")), 2)
  expect_equal(
    find_pycultivator_sqlite(".")$project_id,
    c("mc2_wd", "MC0_AC" )
  )
  expect_equal(
    nrow(find_pycultivator_sqlite("..")), 0
  )
  expect_equal(
    nrow(find_pycultivator_sqlite("./data")), 0
  )
})


test_that("test load_pycultivator", {
  fp <- find_pycultivator_data(".", type="csv")$path[[1]]
  expect_gt(nrow(load_pycultivator_data(fp, type="csv")), 0)
  fp <- find_pycultivator_data(".", type="sqlite")$path[[1]]
  expect_gt(nrow(load_pycultivator_data(fp, type="db")), 0)
  expect_equal(nrow(load_pycultivator_csv("./bla.csv")), 0)
  # add extra tests for time zero
})

test_that("test load_pycultivator_csv", {
  fp <- find_pycultivator_data(".", type="csv")$path[[1]]
  expect_false(is.null(fp))
  expect_gt(nrow(load_pycultivator_csv(fp)), 0)
  fp <- find_pycultivator_data(".", type="csv")$path[[2]]
  expect_false(is.null(fp))
  df <- load_pycultivator_csv(fp)
  expect_gt(nrow(df), 0)
  expect_is(df$OD, "numeric")
  expect_equal(nrow(load_pycultivator_csv("./bla.csv")), 0)
})

test_that("test load_pycultivator_sqlite", {
  fp <- find_pycultivator_data(".", type="sqlite")$path[[1]]
  expect_gt(nrow(load_pycultivator_sqlite(fp)), 0)
  expect_equal(nrow(load_pycultivator_sqlite("./bla.db")), 0)
})

test_that("test parse_pycultivator_csv", {
  fp <- find_pycultivator_data(".", type="csv")$path[[1]]
  df <- load_pycultivator_csv(fp)
  expect_equal(first(df$time_h), 0.75)
})

test_that("test parse_pycultivator_sqlite", {
  fp <- find_pycultivator_data(".", type="sqlite")$path[[1]]
  df <- load_pycultivator_sqlite(fp)
  expect_equal(first(df$time_h), 0)
})
