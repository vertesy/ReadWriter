% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadWriter.R
\name{write.simple.append}
\alias{write.simple.append}
\title{Write Simple Append}
\usage{
write.simple.append(
  input_df,
  filename = substitute(input_df),
  suffix = NULL,
  extension = "tsv",
  manualFileName = NULL,
  manualDirectory = NULL,
  o = FALSE,
  v = TRUE
)
}
\arguments{
\item{input_df}{Data frame to write out. Default: None, must be provided.}

\item{filename}{The base name for the output file. Default: Name of the input data frame.}

\item{suffix}{An optional suffix to add to the filename. Default: NULL.}

\item{extension}{File extension to use, Default: 'tsv'.}

\item{manualFileName}{Manually defined filename, overrides automatic naming. Default: NULL.}

\item{manualDirectory}{Directory to save the file in, overrides default directory. Default: NULL.}

\item{o}{If TRUE, opens the file after writing on OS X using 'system(open ...)'. Default: FALSE.}

\item{v}{verbose Print path? Default: TRUE.}
}
\value{
Appends data to an existing .tsv file.
}
\description{
Appends a data frame without row names to an existing .tsv file with the same number
of columns. The output filename is auto-generated from the variable's name or manually specified.
The file is saved in the specified output directory or the current working directory. The path and
variable name can be passed separately and will be concatenated to form the filename.
}
\examples{
\dontrun{
if (interactive()) {
  write.simple.append(input_df = myDataFrame)
}
}
}
