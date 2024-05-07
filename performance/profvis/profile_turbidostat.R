# Profile

library(dplyr)
library(mmpr)

data.file.correct <- here::here("tests/testthat/latest_20170113_MC0_AC_measurements.db")
data.file.wrong <- here::here("tests/testthat/latest_20160714_mc2_wd_measurements.db")

profvis::profvis({
  d.tb <- load_turbidostat_sqlite(data.file.correct)
})

profvis::profvis({
  # combine the datasets
  d <- combine_turbidostat(d.od, d.tb) %>%
    filter(!is.na(decision))
})

profvis::profvis({
  # now calculate growth rates
  df.lm.rates <- d %>%
    group_by(channel) %>%
    turbidostat_growth(time_h, OD, method="lm")
})
