#' Tidy the nested growth rate models obtained from sliding windows
#'
#' @param model data.frame The model to tidy
#' @param x name of column that is the x-variable in the model
#' @param method name of method to tidy models with
#' @param .f reference to function used to tidy the models
#' @param ... extra arguments passes to other functions
#'
#' @export
tidy_slide_growth <- function(
  model, x, method="lsq", .f=NULL, ...
) {

  x <- enquo(x) %>% resolve_quosure()

  result <- data.frame()

  if (is.null(.f)) {
    .f <- switch (method,
                  "lm" = tidy_slide_growth.lm,
                  "ts" = tidy_slide_growth.ts,
                  "lsq" = tidy_slide_growth.lsq,
                  NULL
    )
  }

  # check if valid method was provided
  stopifnot(rlang::is_callable(.f))

  if (!is.null(model) && !is.na(model)) {
    result <- .f(model, x=UQ(x), ...)
  }

  return(result)
}

#' Tidy nested Linear Regression Models.
#'
#' @param model The model to tidy
#' @param x reference to the column used to create windows
#' @param ... extra arguments passes to other functions
#'
#' @export
tidy_slide_growth.lm <- function(
  model, x, ...
) {

  x <- enquo(x) %>% resolve_quosure()

  df.tidy <- model %>%
    broom::tidy()

  if (nrow(df.tidy) > 0 && ("term" %in% colnames(df.tidy)) && ("estimate" %in% colnames(df.tidy)) ) {
    df.tidy <- df.tidy %>%
      filter(term == quo_name(x)) %>%
      select(-term) %>%
      rename(slope = estimate)
  }

  df.glance <- model %>%
    broom::glance()

  if ("statistic" %in% colnames(df.glance)) {
    df.glance <- df.glance %>%
      select(-statistic)
  }

  if ("p.value" %in% colnames(df.tidy)) {
    df.glance <- df.glance %>%
      select(-p.value)
  }

  result <- bind_cols(
    df.tidy, df.glance
  )

  return(result)
}

#' Tidy nested Theil-Sen Regression Models.
#'
#' @param model The model to tidy
#' @param x reference to the column used to create windows
#' @param ... extra arguments passes to other functions
#'
#' @export
tidy_slide_growth.ts <- function(
  model, x, ...
) {

  x <- enquo(x) %>% resolve_quosure()

  result <- tidy.sen(model)

  if (nrow(result) > 0) {
    result <- result %>%
      filter(term == quo_name(x)) %>% select(-term) %>%
      rename(slope = estimate)
  }

  return(result)
}

#' Tidy nested Least-Squares Regression Models.
#'
#' @param model The model to tidy
#' @param x reference to the column used to create windows
#' @param ... extra arguments passes to other functions
#'
#' @export
tidy_slide_growth.lsq <- function(
  model, x, ...
) {

  # x <- enquo(x) %>% resolve_quosure()

  # we just return the "model" so that it get's unnested
  result <- model

  return(result)
}

#' Extract the Theil-Sen model statistics from a Theil-Sen model object.
#'
#' @param model A Theil-Sen model obtained with the 'zyp' package
#'
#' @export
tidy.sen <- function(
  model
) {
  result <- data.frame()

  if (!is.na(model$coefficients[[1]])) {
    df <- data.frame(model$coefficients, row.names = NULL)
    colnames(df) <- c("estimate")

    df["term"] <- names(model$coefficients)
    ci <- zyp::confint.zyp(model)
    df.ci <- data.frame(ci)

    colnames(df.ci) <- c("conf.low", "conf.high")
    df.ci["term"] <- rownames(ci)

    result <- left_join(df, df.ci, by=c("term"))
  }
  return( result )
}
