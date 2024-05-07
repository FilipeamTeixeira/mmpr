
#' Execute `do` on a data.frame in parallel
#'
#' Requires the `future` library
#'
parallel <- function(d, .f, .name="values", ...) {
  .name <- enquo(.name) %>% resolve_quosure()

  d %>%
    nest_legacy() %>%
    mutate(
      !!.name := map(future( .f(.)) ),
      !!.name := values(!!.name)
    ) %>%
    unnest_legacy( !!.name, ...)
}

#' Make timestamped filename
#'
#' @param name character name of the file (with extension)
#' @param stamp object which can be converted to `POSIXlt`
#' @param .stamp.format character string specifying the format of the stamp. Defaults to `\%Y\%m\%d`.
#' @param .path character string specifying an addition path that should be prepended to the name
#'
#' @return character string with the timestamped file path
#'
#' @export
stamp_filename <- function(name, stamp = NULL, .stamp.format = "%Y%m%d", .path = NULL) {

  if (is.null(stamp)) {
    stamp <- strftime(Sys.Date(), format=.stamp.format)
  }

  # add timestamp
  name <- sprintf("%s_%s", stamp, name)

  if (!is.null(.path)) {
    return(file.path(.path, name))
  }

  return(name)
}
