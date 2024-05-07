MMPR
====

R Package for the Molecular Microbial Physiology Group.

``` r
# look for valid data files
data.dir <- "."
data.files <- find_pycultivator_data(data.dir)

# load data file
df.raw <- load_pycultivator_csv( fp = data.files$path[[1]] )

# estimate growth rates per channel on only OD values obtained at 720nm
# using a window of 12 points (1 point = 5 minutes, 12 = 1 hour)
df.raw %>%
  group_by(channel) %>%
  filter(od_led == 720) %>%
  slide_growth_window(w_size = 12)
```

Installation
------------

The package is in development and can be downloaded with `devtools`:

``` r
# Install the zyp package first to fix a problem with the Kendall package
# install.packages("zyp")
# install.packages("devtools")
# installation of SlideR is required
# devtools::install_git("https://gitlab.com/mmp-uva/slider.git")
devtools::install_git("https://gitlab.com/mmp-uva/mmpr.git")
```

### Requirements

The package requires the following packages:

-   `slider`
    (`devtools::install_git("https://www.gitlab.com/mmp-uva/slider.git")`)
-   `rlang` (`install.packages("rlang")`)
-   `lazyeval` (`install.packages("lazyeval")`)
-   `dplyr` (`install.packages("dplyr")`)

If you want to load SQLITE database files you will need the `RSQlite`
package.

-   RSQLite (`install.packages("RSQlite")`)

Usage
-----

To load the package:

``` r
library(mmpr)
```

### Data import

The package provides helper functions to load data files generated with
the pyCultivator package.

#### Automatic search for compatible files

``` r
# search for all compatible files
find_pycultivator_data("./")
# same, but explicitly set the allowed types
find_pycultivator_data( path = "./", types=c("csv", "sqlite"))
# only csv
find_pycultivator_data( path = "./", types=c("csv"))
find_pycultivator_data( path = "./", types="csv")
# only sqlite
find_pycultivator_data( path = "./", types=c("sqlite"))
find_pycultivator_data( path = "./", types="sqlite")
```

### Loading and parsing of compatible files

``` r
# will load a csv file
load_pycultivator_data(path = "./<file_name>.csv", type="csv")
# will load a sqlite file
load_pycultivator_data(path = "./<file_name>.db", type="sqlite")
```

*NOTE*: These functions will not check if the path leads to a file of
the given type. Thus trying to load a CSV file with
`load_pycultivator_data( type="sqlite")` will fail horribly.

#### Loading and parsing CSV files

``` r
load_pycultivator_csv( fp = "./<file_name>.csv" )
```

#### Loading and parsing of SQLITE database files

``` r
load_pycultivator_sqlite( fp = "./<file_name>.db" )
```

### Growth rate calculations

The package provides many different growth calculation methods.

The following functions are available:

-   `slide_growth_window`: Estimates growth rate using a sliding window
    of *one* size
-   `slide_growth_windows`: Estimates growth rates using multiple
    sliding windows of *different* sizes.

The following settings can be set:

-   `x`: Name of time variable (defaults to `time_h`)
-   `y`: Name of biomass variable (defaults to `OD`)

-   `method`: Estimation method (Fast Least Squares (“lsq”), Linear
    Model (“lm”), or Theil-Sen estimator (“ts”)).
-   `partial`: Whether to retain windows smaller than the window size
    (at the ends of the dataset).
-   `align`: Position of the key relative to the window.
-   `tidy`: Whether to extract statistics from the obtained models and
    present in columns.

-   `w_size`: Size of the sliding window (`slide_growth_window`).
-   `w_min`: Smallest window size (`slide_growth_windows`).
-   `w_max`: Largest window size (`slide_growth_windows`).
-   `w_step`: Step size, to increase window from smalles to largest
    window (`slide_growth_windows`).

#### Basic usage

Using `slide_growth_window` one can estimate growth rates using a
sliding window.

``` r
# create example data.frame using simulate_growth function
mu <- 0.08
t <- seq(0, 100)
df <- simulate_growth(t, mu, .error.range = 0.001)

# calculate growth rate
slide_growth_window(df, time_h, OD)
```

The function can be used in combination with dplyr and do, to estimate
growth rates per group.

``` r
# we use purrr to generate data.frame with multiple growth rates
library(purrr)

# create example data.frame using simulate_growth function
mu <- c(0.07, 0.06, 0.08)
t <- seq(0, 100)
df <- map_dfr(mu, ~simulate_growth(t, .x, .error.range = 0.001))

# calculate growth rate
df %>%
  group_by(mu) %>%
  do( slide_growth_window(., time_h, OD) )
```

Using the same but for multiple window sizes:

``` r
# we use purrr to generate data.frame with multiple growth rates
library(purrr)

# create example data.frame using simulate_growth function
mu <- data.frame( mu = c(0.07, 0.06, 0.08))
t <- seq(0, 100)
df <- map_dfr(mu, ~simulate_growth(t, .x, .error.range = 0.001))
w_sizes <- c(5, 10, 20)

# calculate growth rate
df %>%
  group_by(mu) %>%
  do( slide_growth_windows(., time_h, OD, w_sizes = w_sizes))
```

### Turbidostat

For turbidostat data (mainly focussed on pyCultivator data) an
additional function is available to rapidly calculate growth rates for
each dilution cycle in the data. The method is similar to the sliding
windows method, instead of grouping the data in overlapping windows we
group the data per dilution cycle. Then we apply our growth rate
estimating algorithm on each group. This approach requires knowledge
about when each dilution cycle starts and/or ends.

#### Simple introduction on simulated data

First let’s look at simulated data to better understand the method. The
function `turbidostat_growth` will create a nested data.frame for every
decision in the data. NOTE: This operation will keep the columns of
existing groups, BUT the resulting data.frame is no longer grouped.

Every obtained growth rate will be annotated with:

-   the existing group(s)
-   cycle label
-   the number of rows in the corresponding cycle
-   the maximum value of the x variable
-   the distance between the maximum and minimum value of the x variable
-   parameters from the model (depending on the method used)

``` r
# simulate 4 dilution cycles named 1 - 4
decision <- 1:4
# simulate 1 hour of growth per dilution cycle
t <- 1:60 * (5/60)
# every dilution cycle has a different growth rate
mu <- c(0.06, 0.05, 0.08, 0.059)

# generate data
df <- map2_dfr(mu, decision, function(m, d) {
  simulate_growth(t, m, .error.range = 0.001) %>%
    mutate(decision = d)
})

# calculates growth rates for each cycle (1-4)
df %>%
  turbidostat_growth(time_h, OD)
```

#### Turbidostat data from pyCultivator (legacy)

PyCultivator (legacy) databases contain information about when each
decision is made in a separate table. `MMPR` contains helper functions
for loading the turbidostat data and combining it with OD data.

This process consists of the following steps:

1.  Load data from database
2.  Assign a number to each dilution event in the turbidostat data
3.  Combine turbidostat data with od data

The first two steps are combined in `load_turbidostat_sqlite`.

``` r
# file path of the database
fp <- "<file_name>.db"
# load data from database
df.od <- load_pycultivator_sqlite(fp) # OD data
df.tb <- load_turbidostat_sqlite(fp)  # turbidostat data

# combine decision data with od data
df.turbidostat <- df.od %>%
  # usually we want to calculate growth rates using OD values from one specific wavelength
  # filtering at early stage improves performance of the process.
  filter(od_led == 720) %>%
  # add turbidostat information, by default this matches using foreign key (id <=> measurement_id)
  combine_turbidostat(df.tb)

# calculate growth rates
df.growth.rates <- df.turbidostat %>%
  group_by(channel) %>%
  turbidostat_growth(time_h, OD, method="lm")
```

### Speeding things up

The growth rate calculations can be fairly slow, especially when doing
sliding windows. In the case where you have multiple timeseries and a
multi-core computer, you can benefit from parallelisation. Functional
programming on data.frame combined with the package `future` is a very
powerful tool here:

``` r
# we use purrr to generate data.frame with multiple growth rates
library(purrr)
# install.packages("future")
library(future)

# create example data.frame
mu <- data.frame( mu = c(0.07, 0.06, 0.08))
t <- seq(0, 100)
df <- map_dfr(mu, ~simulate_growth(t, .x, .error.range = 0.001))

# enable multiprocessing
plan("multiprocess") # either uses parallel processes or parallel sessions

# calculate growth rates
df %>% 
  group_by(mu) %>%
  # to divide the work we need to divide the dataset into independent chunks of data
  # we leverage nesting create subsets for every group
  nest() %>%
  mutate(
    # we use the `future` function to parallise each call made by map (i.e. 1 call per group)
    growth = map(data, function (.x) future(slide_growth_window(.x, time_h, OD, w_size = 10)))
  ) %>% 
  # once completed we can collect the result of each parallel call using the `values()` function
  # here we immediately unnest the results
  unnest(growth %>% values())
```

or for turbidostat data:

``` r
# we use purrr to generate data.frame with multiple growth rates
library(purrr)
# install.packages("future")
library(future)


# simulate 4 dilution cycles named 1 - 4
decisions <- 1:4
# simulate 1 hour of growth per dilution cycle
t <- 1:60 * (5/60)
# create example data.frame
df.data <- data.frame(
  # 8 channels
  channel = 1:8, 
  # each channel grows at it's own rate
  spec_mu = c(0.07, 0.06, 0.08, 0.09, 0.05, 0.04, 0.06, 0.08)
) %>%
  # generate OD data for each channel using it's spec_mu, and unnest it
  unnest(
    map(spec_mu, function(.x, .error.rate = 0.1) {
      # for each dilution cycle generate a random mu close to the spec_mu.
      mu <- stats::rnorm(length(decisions), mean = .x, sd = .error.rate * .x)
      # browser()
      # generate OD data for each decision and label it with the decision number
      map2_dfr(decisions, mu, function(d, m) {
        simulate_growth(t, m, .error.range = 0.001) %>%
          mutate(decision = d)
      })
    })
  )

# enable multiprocessing
plan("multiprocess") # either uses parallel processes or parallel sessions

# calculate growth rates
df.data %>% 
  group_by(channel) %>%
  # to divide the work we need to divide the dataset into independent chunks of data
  # we leverage nesting create subsets for every group
  nest() %>%
  mutate(
    growth = map(data, function (.x) future(turbidostat_growth(.x, time_h, OD, method="lsq")))
  ) %>%
  # once completed we can collect the result of each parallel call using the `values()` function
  # here we immediately unnest the results
  unnest(growth %>% values())
```
