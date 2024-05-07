

#' Extract file information from a file name string
#' @param str The filename
#' @param prefix The prefix used to generate data file name
#'
#' @return data.frame with the information extracted from the given path
extract_file_info <- function(str, prefix="latest") {
  # collect info
  fn <- sub(pattern = "\\.[[:alpha:]]+", "", basename(str))
  ext <- sub(".*[[:alpha:]]+[[:punct:]].*[[:punct:]]", "", str)
  project_id <- extract_project_id(fn, prefix=prefix)
  t_zero <- extract_t_zero(fn, prefix=prefix) %>%
    as.character() %>%
    readr::parse_datetime(format = "%Y%m%d")

  return(
    data.frame(
      path = str, file_name = fn, t_zero=t_zero, format= ext,
      project_id = project_id,
      name = sprintf("%s (%s)", project_id, t_zero),
      stringsAsFactors = F
    )
  )
}

#' Extract the time zero timestamp from a file name string
#'
#' @param str The filename
#' @param prefix The prefix used to generate data file name
#' @return The timestamp or NA if not found
#'
#' @export
extract_t_zero <- function(str, prefix="latest") {
  pattern <- "([0-9]+)_(.*)_measurements(\\.[a-z]+)?$"

  if (!(is.null(prefix) || prefix == "")) {
    pattern <- paste(prefix, pattern, sep="_")
  }

  result <- sub(pattern, "\\1", str)

  if (result == str) {
    result <- NA
  }
  return(result)
}

#' Extract the project_id from a filename string
#'
#' @param str The filename
#' @param prefix The prefix used to generate data file name
#' @return The project_id or NA if not found
#'
#' @export
extract_project_id <- function(str, prefix="latest") {
  pattern <- "([0-9]+)_(.*)_measurements(\\.[a-z]+)?$"
  if (!(is.null(prefix) || prefix == "")) {
    pattern <- paste(prefix, pattern, sep="_")
  }

  result <- sub(pattern, "\\2", str)

  if (result == str) {
    result <- NA
  }
  return(result)
}

#' Locate pycultivator compitable files in the given directory
#'
#' @param path The directory that will be searched
#' @param types The type of data files to search for
#' @param prefix The prefix used to generate data file name
#' @return data.frame with the found files and annotations
#'
#' @export
find_pycultivator_data <- function(
  path = "./data", types=c(), prefix="latest", ...
) {
  if (length(types) == 0) {
    types <- c("csv", "sqlite")
  }
  files <- data.frame()
  if (any(c("csv") %in% types)) {
    files <- bind_rows(
      files, find_pycultivator_csv(path = path, prefix=prefix)
    )
  }
  if (any(c("sqlite", "db") %in% types)) {
    files <- bind_rows(
      files, find_pycultivator_sqlite(path = path, prefix=prefix)
    )
  }
  return(files)
}

#' Locate pycultivator compatible CSV files in the given directory
#'
#' @param path The directory with the files
#' @param prefix The prefix used to generate data file name
#' @param rm.na logical indicatng whether to remove data-files whose file name could not parsed (default=TRUE)
#' @return data.frame with the found files and annotations
#'
#' @export
find_pycultivator_csv <- function(path = "./data", prefix="latest", rm.na = TRUE) {
  result <- data.frame()
  pattern <- paste(prefix, ".*_measurements\\.csv$", sep="_")
  files <- list.files(
    path, pattern = pattern, full.names = T
  )
  # extract file info
  if (length(files) > 0) {
    result <- map_dfr(files, extract_file_info, prefix=prefix)
    if (rm.na) {
      result <-result %>%
        filter(!is.na(t_zero), !is.na(project_id), !is.na(name))
    }
  }
  return(result)
}

#' Locate pycultivator compatible SQLITE files in the given directory
#'
#' @param path The directory with the files
#' @param prefix Prefix used to generate data file name
#' @param rm.na logical indicatng whether to remove data-files whose file name could not parsed (default=TRUE)
#' @return data.frame with the found files and annotations
#'
#' @export
find_pycultivator_sqlite <- function(path = "./data", prefix="latest", rm.na=TRUE) {
  result <- data.frame()
  pattern <- paste(prefix, ".*_measurements.(db|sqlite)$", sep="_")
  files <- list.files(
    path, pattern = pattern, full.names = T
  )
  # extract file info
  if (length(files) > 0) {
    result <- map_dfr(files, extract_file_info, prefix=prefix)
    if (rm.na) {
      result <-result %>%
        filter(!is.na(t_zero), !is.na(project_id), !is.na(name))
    }
  }
  return(result)
}

#' Load cultivation data from a, with pyCultivator generated, data source
#'
#' @param path A string with the path to the data file
#' @param t_zero A string representing the time zero timepoint or
#'  NA to use the first timepoint in the dataset
#' @param time_fmt Format to use when parsing Date/Time strings
#' @param type File format to use when reading the file
#'  ('sqlite' for sqlite and 'csv' for csv files.)
#' @return data.frame The retrieved data
#'
#' @export
load_pycultivator_data <- function(
  path, t_zero=NA, time_fmt="%Y-%m-%d %H:%M:%S", type="csv"
) {
  data <- data.frame()
  if (any(c("csv") == type)) {
    data <- load_pycultivator_csv(
      path = path, t_zero = t_zero, time_fmt = time_fmt
    )
  }
  if (any(c("sqlite", "db") == type)) {
    data <- load_pycultivator_sqlite(
      path = path, t_zero = t_zero, time_fmt = time_fmt
    )
  }
  return(data)
}

#' Load cultivation data from a, with pyCultivator generated, CSV file
#'
#' @param path A string with the path to the data file
#' @param t_zero A string representing the time zero timepoint or
#'  NA to use the first timepoint in the dataset
#' @param time_fmt Format to use when parsing Date/Time strings
#'
#' @importFrom utils read.csv2
#'
#' @export
load_pycultivator_csv <- function(
  path, t_zero=NA, time_fmt="%Y-%m-%d %H:%M:%S", sep=";", dec=","
) {
  stopifnot(!is.null(path))
  data <- data.frame()
  if (file.exists(path)) {
    # read data
    data <- read.csv2(path, sep=sep, dec=dec, stringsAsFactors = F) %>%
      parse_pycultivator_csv(t_zero, time_fmt)
  }
  return(data)
}

#' Load cultivation data from a, with pyCultivator generated, SQLITE file
#'
#' @param path A string with the path to the data file
#' @param t_zero A string representing the time zero timepoint or
#'  NA to use the first timepoint in the dataset
#' @param time_fmt Format to use when parsing Date/Time strings
#'
#' @export
load_pycultivator_sqlite <- function(
  path, t_zero=NA, time_fmt="%Y-%m-%d %H:%M:%S"
) {

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package \"RSQLite\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  data <- data.frame()

  if (file.exists(path)) {
    #data_db <- src_sqlite(path, create = FALSE) old
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    # Replace src_sqlite() with tbl() and pass the database connection
    data_db <- tbl(con, "measurement")

    #stopifnot("measurement" %in% src_tbls(data_db)) old
    stopifnot("measurement" %in% tbls(con)) old #new

    #data <- collect(tbl(data_db, "measurement"), n = Inf) %>%
    data <- collect(data_db, n = Inf) %>% #new
      parse_pycultivator_sqlite(t_zero, time_fmt)
  }
  return(data)
}

#' Parse data obtained from a, with pyCultivator generated, SQLITE file
#'
#' @export
parse_pycultivator_sqlite <- function( data, t_zero=NA, time_fmt="%Y-%m-%d %H:%M:%S") {

  if (is.na(t_zero)) {
    t_zero <- data %>% pull(time) %>% min()
  }

  data$experiment_id <- as.numeric(data$experiment_id)
  data$channel_id <- as.numeric(data$channel_id)

  data <- data %>%
    rename(OD = od_value, channel = channel_id) %>%
    group_by(channel, od_led) %>%
    mutate( time_h = as.numeric(difftime(
      time %>% as.character() %>% readr::parse_datetime(time_fmt),
      t_zero %>% as.character() %>% readr::parse_datetime(time_fmt), units = "hours"
    ))) %>%
    filter(time_h >= 0)
  return(data)
}

#' Parse data obtained from a pycultviator CSV file
#'
#' @export
parse_pycultivator_csv <- function( data, t_zero=NA, time_fmt="%Y-%m-%d %H:%M:%S") {

  if (is.na(t_zero)) {
    t_zero <- data %>% pull(time) %>% min()
  }

  if ("od_value" %in% colnames(data)) {
    data <- data %>%
      rename(OD = od_value)
  }

  data <- data %>%
    group_by(channel, od_led) %>%
    mutate( time_h = as.numeric(difftime(
      time %>% as.character() %>% readr::parse_datetime(time_fmt),
      t_zero %>% as.character() %>% readr::parse_datetime(time_fmt), units = "hours"
    ))) %>%
    filter(time_h >= 0)
  return(data)
}
