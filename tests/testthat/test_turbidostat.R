library(mmpr)
context("Turbidostat analysis API")

data.file.correct <- "latest_20170113_MC0_AC_measurements.db"
data.file.wrong <- "latest_20160714_mc2_wd_measurements.db"

test_that("test load_turbidostat_sqlite", {
  # try the normal file
  expect_gt(
    nrow(load_turbidostat_sqlite(data.file.correct)), 0
  )
  # try with incorrect db file
  expect_warning(
    n.wrong <- nrow(load_turbidostat_sqlite(data.file.wrong))
  )
  # ensure that we have 0 rows returned
  expect_equal(n.wrong, 0)
})

test_that("test combine_turbidostat", {

  df.od <- load_pycultivator_sqlite(data.file.correct)
  df.tb <- load_turbidostat_sqlite(data.file.correct)

  expect_gt(nrow(df.od), 0)
  expect_gt(nrow(df.tb), 0)

  df.turbidostat <- df.od %>%
    combine_turbidostat(
      df.tb
    )

  expect_gt(nrow(df.turbidostat), 0)

})

test_that("test count_occurence", {
  v <- rep(1:5, 10)
  expect_equal(
    count_occurence(v, 1, include = T),
    rep(1:10, each=5)
  )

  v <- rep(1:5, 10)
  expect_equal(
    count_occurence(v, 1, include = F),
    c(0,rep(1:9, each=5), rep(10, each=4))
  )

  df <- data.frame(
    A = rep(c("a", "b"), each=50),
    C = rep(c(1,2,3,4), each=25),
    B = rep(1:5, 20)
  )

  df.count <- df %>%
    group_by(A, C) %>%
    mutate(
      count = count_occurence(B, 1, F)
    )

})


test_that("test count_occurence_in", {
  df <- data.frame(
    A = rep(c("a", "b"), each=50),
    B = rep(1:5, 20)
  )

  df.count <- df %>%
    count_occurence_in("B", 1)

  expect_equal(
    max(df.count$count), 20
  )

  df.count <- df %>%
    group_by(A) %>%
    count_occurence_in("B", 1)

  expect_equal(
    max(df.count$count), 10
  )

})
