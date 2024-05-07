#' Calculate growth rates on turbidostat data
#'
#' This method uses a grouping factor to
#' nest the data.frame (by default, decision). Then a function is applied
#' to each nested data.frame to estimate the growth rate for that decision.
#'
#' By default, the `decision_growth` function is used to estimate growth rate,
#' but custom functions can be passed using the `.f` argument.
#'
#' @param data data.frame to use
#' @param x Name of the column with time data (in hours)
#' @param y Name of the column with OD data
#' @param method Name of the method to use for growth calculation (see `decision_growth` for options)
#' @param .group Name of the column to group the data by and calculate one growth rate per group
#' @param .to Name of the column to store the model in
#' @param .tidy Whether to tidy the model into a data.frame
#' @param .transform Whether to apply log transformation to the y column
#' @param .f function used to calculate growth with (if NULL use decision_growth)
#' @param ... extra arguments passes to other functions
#'
#' @export
turbidostat_growth <- function(
  data, x, y, method="lsq", .group="decision", .to="model", .tidy=TRUE, .transform=TRUE, .f=NULL, ...
) {

  x <- enquo(x) %>% resolve_quosure()
  y <- enquo(y) %>% resolve_quosure()
  .group <- enquo(.group) %>% resolve_quosure()
  .to <- enquo(.to) %>% resolve_quosure()

  if (!(quo_name(x) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(x)))

  if (!(quo_name(y) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(y)))

  if (!(quo_name(.group) %in% colnames(data)))
    stop(sprintf("Column '%s' does not exist in given data.frame", quo_name(.group)))

  # if .f is null, use method
  if (is.null(.f))
    .f <- partial(decision_growth, method=method)

  result <- data %>%
    # remove NA values from label
    filter(!is.na(!!.group)) %>%
    # make sure we group be decision
    group_by(!!.group, add = TRUE) %>%
    # make sure data is order by time
    arrange(!!x) %>%
    nest() %>%
    # add descriptive information outside nested data.frame
    mutate(
      w_size = map_dbl(data, nrow),
      w_time = map_dbl(data, function(d) {
        .x <- d %>% pull(!!x)

        return(max(.x) - min(.x))
      }),
      # use last timepoint as timepoint of decision
      time_h = map_dbl(data, function(d) {
        d %>% pull(!!x) %>% max()
      })
    ) %>%
    mutate(
      !!quo_name(.to) := map(data, .f, x=UQ(x), y=UQ(y), .transform=.transform)
    )

  if (.tidy) {

    result <- result %>%
      mutate( tidy_data = map(UQ(.to), tidy_slide_growth, x=UQ(x), method=method), UQ(.to) )

    # remove unwanted columns from tidy dataset
    if ("data" %in% colnames(result)) {
      result <- result %>% select(-data)
    }

    # remove unwanted columns from tidy dataset
    if (quo_name(.to) %in% colnames(result)) {
      result <- result %>% select(-UQ(.to))
    }

    result <- result %>% unnest_legacy(tidy_data)
  }

  return(result)
}

#' Estimates growth in one dilution segment of turbidostat data.
#'
#'
#' @param data data.frame with the dilution segment
#' @param x Name of column with time (in hours) information
#' @param y Name of column with biomass information
#' @param method Name of the method to use for growth calculation
#'    ('lm' for linear models, 'lsq' for fast least-squares, 'ts' for theil-sen)
#' @param .f Custom function to use growth rate estimation (overrides the method option)
#' @param .reduce Reduces the data to points between the lowest and highest value (by index)
#' @param ... extra arguments passed to other functions
#'
#' @export
decision_growth <- function(
  data, x, y, method="lsq", .f=NULL, .reduce = TRUE, ...
) {
  result <- NULL

  x <- enquo(x) %>% resolve_quosure()
  y <- enquo(y) %>% resolve_quosure()

  # load function belonging to the given method
  if (is.null(.f)) {
    .f <- switch(method,
                "lm" = growth.lm,
                "ts" = growth.ts,
                "lsq" = growth.lsq,
                NULL
    )
  }

  if (!rlang::is_callable(.f))
    stop("Invalid function or unknown method")

  if (.reduce) {
    data <- data %>%
      filter(
        row_number() >= which.min(UQ(y)),
        row_number() <= which.max(UQ(y))
      )
  }

  if (is.data.frame(data) && nrow(data) > 3) {
    result <- .f(data, UQ(x), UQ(y), ...)
  }

  result
}
