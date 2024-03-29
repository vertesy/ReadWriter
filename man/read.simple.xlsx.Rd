% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadWriter.R
\name{read.simple.xlsx}
\alias{read.simple.xlsx}
\title{Read a multi-sheet XLSX easily}
\usage{
read.simple.xlsx(
  pfn = Stringendo::kollapse(...),
  which_sheets,
  col_names = TRUE,
  row_names = FALSE,
  trim_ws = TRUE,
  ...
)
}
\arguments{
\item{pfn}{Path and filename of the XLSX file.
Default: Constructed using \code{Stringendo::kollapse(...)}.}

\item{which_sheets}{Indices or names of sheets to read from the XLSX file.
Default: All sheets.}

\item{col_names}{Logical, whether to use the first row as column names.
Default: TRUE.}

\item{row_names}{Numeric, whether to convert a column to row names.
Default: 1. Use 0 for no conversion.}

\item{...}{Pass arguments to read.xlsx().}
}
\value{
A list of data frames, each representing a sheet from the XLSX file.
}
\description{
Reads specified sheets from an XLSX file into a list of data frames.
It allows customization of column names, row names, and trimming of white spaces.
}
