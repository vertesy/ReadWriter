% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadWriter.R
\name{read.simple.tsv}
\alias{read.simple.tsv}
\title{read.simple.tsv}
\usage{
read.simple.tsv(
  ...,
  sep_ = "\\t",
  colnames = TRUE,
  wRownames = TRUE,
  coltypes = NULL,
  NaReplace = TRUE,
  asTibble = FALSE
)
}
\arguments{
\item{...}{Multiple simple variables to parse.}

\item{sep_}{Separator character, Default: '  '}

\item{colnames}{Are there column names?, Default: TRUE}

\item{wRownames}{With rownames?, Default: TRUE}

\item{coltypes}{What type of variables are in columns? Auto-guessing can be very slow., Default: NULL}

\item{NaReplace}{Replace NA-values?, Default: TRUE}

\item{asTibble}{Load as tibble or dataframe?, Default: FALSE (=load as df)}
}
\description{
Read in a file with excel style data: rownames in col1,
headers SHIFTED. The header should start with a TAB / First column name
should be empty.
}
\examples{
\dontrun{
if (interactive()) {
  # read.simple.tsv("path/to/my.file")
}
}
}
\seealso{
\code{\link[readr]{read_delim}}
\code{\link[gtools]{na.replace}}
}
