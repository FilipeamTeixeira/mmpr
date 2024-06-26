% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{find_pycultivator_csv}
\alias{find_pycultivator_csv}
\title{Locate pycultivator compatible CSV files in the given directory}
\usage{
find_pycultivator_csv(path = "./data", prefix = "latest",
  rm.na = TRUE)
}
\arguments{
\item{path}{The directory with the files}

\item{prefix}{The prefix used to generate data file name}

\item{rm.na}{logical indicatng whether to remove data-files whose file name could not parsed (default=TRUE)}
}
\value{
data.frame with the found files and annotations
}
\description{
Locate pycultivator compatible CSV files in the given directory
}
