# CHANGELOG

## 0.9.0 [WIP]

* Make `unnest` in `turbidostat_growth` compatible with tidyr 1.0.0
* Uses `unnest_legacy` to speed things up (due to issue in `vctrs` 0.2.0)
* Some small improvements to value checking in tidy functions

## 0.8.3

* Fixes RMarkdown templates
* Clean imports
* Add documentation about reading turbidostat files
* Add test for matching descisions to measurement id's

## 0.8.2

* Ignore ill-formatted pyCultivator files.

## 0.8.1

* Implement auto-configuration of GIT and Packrat to MMP Analysis Project template
* Make `turbidostat_growth` robust against unordered data
* `simulate_growth` no longer returns the name column produced by the `accumulate` function

## 0.8.0

* Renamed parse_csv and parse_sqlite to parse_pycultivator_csv and parse_pycultivator_sqlite
* Removed slide_growth_window.* after deprecating
* Removed batch_slide_growth
* Provide Markdown Templates
* Provide Project Templates

## 0.7.0

* Remove filters from package
* Report length in time of dilution section when calculating growth in turbidostat data
* Fixed some docstrings
* Fixed row_number warning by being explicit about dplyr::row_number

## 0.6.1

* Support bare variable names for key and .to name
* Implement basic least-squares regression for faster computation (method = "lsq")
* Rename `slide_growth` to `slide_growth_window`
* Rename `batch_slide_growth` to `slide_growth_windows`
* Require dplyr 0.7.5

## 0.5.10

* Change to using map instead of do in sliding windows
* Require slider 0.1.3 to solve problem with map

## 0.5.9

* Update documentation
* Fix bug in load_pycultivator_csv
* Fix issue when estimating growth on data with negative OD values

## 0.5.8

* Implement a growth simulation function (`simulate_growth`)
* Add support for theil sen estimation on turbidostat data
* Add unit tests for growth rates functions
* Unit test compare linear model and theil sen
* Add unit tests for turbidostat functions

## 0.5.7

* Remove old deprecated `roll_*` functions
* Remove zoo as dependency
* Update to SlideR 0.1.2 (and require it)

## 0.5.6

* Fix bug in turbidostat_growth.lm causing it to not calculate per group
* Resolve issue where time_h would not start from 0

## 0.5.5

* Add growth rate calculation for turbidostat data
* Resolve failing unit tests

## Prior 0.5.5

* Growth calculations using sliding windows for batch cultivation data
* Functions for loading pyCultivator data
