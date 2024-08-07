% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadWriter.R
\name{column.2.row.names}
\alias{column.2.row.names}
\title{Convert a Column to Row Names in a Tibble or DataFrame}
\usage{
column.2.row.names(
  tibble,
  rowname_column = 1,
  make_names = FALSE,
  as_df = TRUE,
  warn = TRUE,
  overwrite = TRUE,
  ...
)
}
\arguments{
\item{rowname_column}{Index of the column to be used as row names.
Default: 1.}

\item{make_names}{Boolean indicating whether to call \code{make.names} to sanitize row names.
Default: FALSE.}

\item{as_df}{Boolean indicating whether to convert the input to a dataframe if it's not already one.
Default: TRUE.}

\item{warn}{Warn user if row names pre-exist.}

\item{...}{Pass arguments to make.names()..}

\item{df}{A dataframe or tibble without row names.
Default: No default value, a dataframe must be provided.}
}
\description{
Converts the first column (or a specified column) of a dataframe or tibble into row names.
This function differs from \code{tibble::column_to_rownames} in that it takes column names or inices and
it offers the option to sanitize row names using \code{make.names}, provides a warning if there are
duplicated values in the row name column
}
