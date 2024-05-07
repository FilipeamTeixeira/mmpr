#' Estimate exponential growth rate using a sliding window.
#' The growth rate is estimates for every window generated on the data.
#'
#' @param data data.frame with all growth data.
#' @param x char Name of column in data with time information.
#' @param y char Name of column in data with biomass information.
#' @param w_size Size of the sliding window
#' @param align Alignment of the sliding window, relative to key time.
#' @param method char Method used to determine growth rate in each window.
#' @param .tidy Whether to expand the models into columns.
#' @param .to Name of column to store the model in
#' @param .transform Whether to apply log transformation to the `y` column.
#' @param ... extra arguments passed to slide_windows
#'
#' @return Nested data.frame with a linear model for each window.
#'
#' @export
slide_growth_window <- function(
  data, x, y, w_size=10, align="center", method="lsq",
  .to="model", .tidy=TRUE, .transform=TRUE, ...
) {

  x <- enquo(x) %>% slider::resolve_quosure()
  y <- enquo(y) %>% slider::resolve_quosure()
  .to <- enquo(.to) %>% slider::resolve_quosure()

  model_f <- switch (method,
    "lm" = growth.lm,
    "ts" = growth.ts,
    "lsq" = growth.lsq,
    NULL
  )

  if (is.null(model_f)) {
    stop("Unknown method", method)
  }

  if (!(quo_name(x) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(x)))

  if (!(quo_name(y) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(y)))

  # create windows and fit models to each window
  result <- data %>%
    slider::slide_windows(key=UQ(x), w_size=w_size, align=align, ...) %>%
    mutate(
      !!quo_name(.to) := map(data, model_f, x=UQ(x), y=UQ(y), .transform=.transform )
    )

  if (.tidy) {
    result <- result %>%
      mutate(tidy = map(UQ(.to), tidy_slide_growth, x=UQ(x), method=method) ) %>%
      unnest_legacy(tidy)

    # remove unwanted columns from tidy dataset
    if ("data" %in% colnames(result)) {
      result <- result %>% select(-data)
    }

    # remove unwanted columns from tidy dataset
    if (rlang::quo_name(.to) %in% colnames(result)) {
      result <- result %>% select(-UQ(.to))
    }
  }

  # result <- f(
  #   data, x=UQ(x), y=UQ(y), w_size=w_size,
  #   align=align, .to=UQ(.to), .tidy=.tidy, .transform=.transform, ...
  # )

  return(result)
}

#' Estimate exponential growth rate using multiple sliding windows
#'
#' Window sizes can set a-priori as a vector of desired sizes,
#'   using the `w_sizes` argument. It can systematically
#'   vary window sizes from a minimum to maximum in steps.
#'   Use `w_min`, `w_max` and `w_step` for this.
#'
#' @param data data.frame with all growth data.
#' @param x char Name of column in data with time information.
#' @param y char Name of column in data with biomass information.
#' @param w_sizes integer vector with all the desired window sizes.
#' @param w_min smallest window size
#' @param w_max largest window size
#' @param w_step window size increase
#' @param align Alignment of the sliding window, relative to key time.
#' @param method char Method used to determine growth rate in each window.
#' @param .tidy Whether to expand the models into columns.
#' @param .to Name of column to store the model in
#' @param .transform Whether to apply log transformation to the `y` column.
#'
#' @export
slide_growth_windows <- function(
  data, x, y, w_sizes=NULL, w_min=10, w_max=NULL, w_step=10,
  align="center", method="lsq",
  .to="model", .tidy=TRUE, .transform=TRUE, ...
) {

  x <- enquo(x)
  y <- enquo(y)
  .to <- enquo(.to)

  if (is.null(w_sizes)) {
    if(is.null(w_max))
      w_max <- nrow(data %>% distinct(UQ(x)))
    if ( (w_max - w_min) / w_step < 0 )
      stop("Step size has to match direction of sequence")
    w_sizes <- seq( w_min, w_max, by=w_step)
  }

  ## calculate growth rates for each sliding window size
  result <- map_dfr(
    w_sizes, slide_growth_window,
    data=data, x=UQ(x), y=UQ(y), align=align, method=method,
    .to=UQ(.to), .tidy=.tidy, .transform=.transform, ...
  )

  return(result)
}
