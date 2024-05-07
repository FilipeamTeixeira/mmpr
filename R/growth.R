#' Estimates Exponential Growth using a Linear Regression Model
#' Applies a linear model to the given data.frame
#'
#' @param data data.frame contain growth data
#' @param x Name of column containing time information (in hours)
#' @param y Name of column containin biomass information
#' @param .transform Whether to apply `log` transformation to the `y` column
#' @return Linear Model
#'
#' @importFrom stats as.formula
#'
#' @export
growth.lm <- function(
  data, x, y, .transform=TRUE
) {
  result <- NULL

  x <- enquo(x) %>% resolve_quosure()
  y <- enquo(y) %>% resolve_quosure()

  if (!is.data.frame(data))
    stop("Expected data to be a data.frame")

  if (!(quo_name(x) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(x)))

  if (!(quo_name(y) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(y)))

  if (.transform) {
    data <- mutate(data, !!quo_name(y) := log(UQ(y)))
  }

  # create formula
  f <- as.formula(sprintf("%s ~ %s", quo_name(y), quo_name(x)))

  # build model and return it
  data <- data %>%
    filter(!is.na(UQ(y)) & !is.infinite(UQ(y)) )

  if (nrow(data) >= 3)
    result <- lm(f, data)

  result
}


#' Estimates Exponential Growth using a Theil-Sen Regression Model
#' Applies the theil-sen regression model to the given data.frame
#'
#' @param data data.frame contain growth data
#' @param x Name of column containing time information (in hours)
#' @param y Name of column containin biomass information
#' @param .transform Whether to apply `log` transformation to the `y` column
#' @return Theil-Sen model
#'
#' @importFrom stats as.formula
#'
#' @export
growth.ts <- function(
  data, x, y, .transform=TRUE
) {

  result <- NULL

  if (!requireNamespace("zyp", quietly = TRUE)) {
    stop("Package \"zyp\" needed for this function to work. Please install it.",
          call. = FALSE)
  }

  x <- enquo(x) %>% resolve_quosure()
  y <- enquo(y) %>% resolve_quosure()

  if (!is.data.frame(data))
    stop("Expected data to be a data.frame")

  if (!(quo_name(x) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(x)))

  if (!(quo_name(y) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(y)))

  if (.transform)
    data <- mutate(data, !! quo_name(y) := log(UQ(y)))

  # create formula
  f <- as.formula(sprintf("%s ~ %s", quo_name(y), quo_name(x)))

  # build model and return it
  data <- data %>%
    filter(!is.na(UQ(y)) & !is.infinite(UQ(y)) )

  if (nrow(data) >= 3)
    result <- zyp::zyp.sen(f, data)

  result
}

#' Estimates Exponential Growth using a Least Squares Regression Model
#' Applies the least squares regression model to the given data.frame
#' NOTE: this is stripped (faster) version of regular Linear Models
#'
#' @param data data.frame contain growth data
#' @param x Name of column containing time information (in hours)
#' @param y Name of column containin biomass information
#' @param .transform Whether to apply `log` transformation to the `y` column
#' @return data.frame with estimated coefficients and r.squared
#'
#' @importFrom stats as.formula
#'
#' @export
growth.lsq <- function(
  data, x, y, .transform=TRUE
) {
  result <- NULL

  x <- enquo(x) %>% slider::resolve_quosure()
  y <- enquo(y) %>% slider::resolve_quosure()

  if (!is.data.frame(data))
    stop("Expected data to be a data.frame")

  if (!(quo_name(x) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(x)))

  if (!(quo_name(y) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(y)))

  if (.transform)
    data <- mutate(data, !!quo_name(y) := log(UQ(y)))

  # create formula
  f <- as.formula(sprintf("%s ~ %s", quo_name(y), quo_name(x)))

  # build model and return it
  data <- data %>%
    filter(!is.na(UQ(y)) & !is.infinite(UQ(y)) )

  if (nrow(data) >= 3)
    result <- lm.lsq(f, data)

  result
}

#' Calculates the first degree least-squares regression model
#'
#' @param formula formula defining which columns to use from the given data frame
#' @param data data.frame containing the columns specified in the formula
#'
#' @export
lm.lsq <- function(
  formula, data
) {
  # extract variables from formula
  variables <- as.character(attr(terms(formula), "variables")[-1])

  if (length(variables) != 2)
    stop("lm.lsq expects a formula with 2 variables")

  # extract
  x <- data[[ variables[[2]] ]]
  y <- data[[ variables[[1]] ]]

  r <- stats::cor(x, y)
  m <- r * (stats::sd(y) / stats::sd(x))
  c <- mean(y) - m * mean(x)

  result <- data.frame( slope = m, intercept = c, r.squared = r**2 )

  return(result)
}

#' Simulate exponential growth
#'
#' Can provide noise to the data using `.error.range`
#'   and `.error.sd`. Uses rnorm to add random noise.
#'
#' @param t A vector with time points (in hours)
#' @param mu Growth rate to grow at
#' @param initial Initial amount of biomass
#' @param error.range Range of error values
#' @param error.sd Standard deviation of error values
#'
#' @export
simulate_growth <- function(
  t, mu, initial=1, .error.range=0, .error.sd=1
) {

  eg_f <- function( t, v, m) {
    n <- v[["n"]]
    pt <- v[["t"]]

    n <- (n * exp(m * (t - pt))) + .error.range * stats::rnorm(1, 0, .error.sd )

    c("n" = n, "t" = t)
  }

  map_dfr(mu, function(mu){

    f <- partial(eg_f, m=mu)

    purrr::accumulate(t[2:length(t)], ~f(.y, .x) , .init = c("n" = initial, "t"=first(t))) %>%
      tibble::enframe(.) %>%
      select(-name) %>%
      mutate(
        mu = mu,
        time_h = map_dbl(value, "t"),
        OD = map_dbl(value, "n")
      ) %>%
      select(-value)

  })
}
