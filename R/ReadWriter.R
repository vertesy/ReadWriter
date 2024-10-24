######################################################################
# ReadWriter.R
######################################################################
# source('~/GitHub/Packages/ReadWriter/R/ReadWriter.R')
# devtools::load_all('~/GitHub/Packages/ReadWriter');
# devtools::document('~/GitHub/Packages/ReadWriter');



# ____________________________________________________________________________________________ ----
## Aux -------------------------------------------------------------------------------------------------

#' @title Convert a Column to Row Names in a Tibble or DataFrame
#'
#' @description Converts the first column (or a specified column) of a dataframe or tibble into row names.
#' This function differs from `tibble::column_to_rownames` in that it takes column names or inices and
#' it offers the option to sanitize row names using `make.names`, provides a warning if there are
#' duplicated values in the row name column
#'
#' @param tibble A dataframe or tibble without row names.
#'           Default: No default value, a dataframe must be provided.
#' @param rowname_column Index of the column to be used as row names.
#'                    Default: 1.
#' @param make_names Boolean indicating whether to call `make.names` to sanitize row names.
#'                   Default: FALSE.
#' @param as_df Boolean indicating whether to convert the input to a dataframe if it's not already one.
#'              Default: TRUE.
#' @param warn Warn user if row names pre-exist. Default: TRUE.
#' @param overwrite Overwrite row names if they already exist. Default: TRUE.
#'
#' @param ... Pass arguments to make.names()..
#' @export

column.2.row.names <- function(tibble, rowname_column = 1,
                               make_names = FALSE, as_df = TRUE,
                               warn = TRUE,
                               overwrite = TRUE,
                               ...) {
  "This is the function that should be used from 11.2023"

  # Assertions
  stopifnot(
    is.data.frame(tibble),
    # is.numeric(rowname_column),
    rowname_column > 0,
    rowname_column <= ncol(tibble),
    is.logical(make_names), is.logical(as_df)
  )

  if (!is.null(rownames(tibble))) {
    if (warn) {
      options(warn = -1) # this should not be necessary
      warning("tibble/df already has row names:", immediate. = TRUE)
      print(head(rownames(tibble)))
    }
  }
  # browser()

  if (as_df) {
    tibble <- as.data.frame(tibble)
  }

  # Extracting the specified column to be used as row names
  row_names <- tibble[[rowname_column]]

  # Check for duplicated row names
  if (anyDuplicated(rowname_column)) {
    is.duplicated <- rowname_column[which(duplicated(rowname_column))]
    warning(
      length(is.duplicated), " duplicated entries in: ", substitute(rowname_column),
      "\narg make_names = TRUE will enforce uniqueness"
    )
  }

  # Applying make.names if requested
  if (make_names) {
    row_names <- make.names(row_names, unique = TRUE, ...)
  }

  # Removing the rowname column from the dataframe
  tibble <- tibble[, -rowname_column, drop = FALSE]

  # Setting the row names
  if (overwrite) {
    message("Overwriting row names.")
    rownames(tibble) <- NULL
    rownames(tibble) <- row_names
  } else {
    message("Nothing changed, original row names kept.")
  }

  # Output assertion
  stopifnot(is.data.frame(tibble), !is.null(rownames(tibble)))

  return(tibble)
}



# _________________________________________________________________________________________________
#' @title FirstCol2RowNames
#'
#' @description Set First Col to Row Names
#' @param Tibble A dataframe without rownames (tibble style)
#' @param rownamecol rowname column, Default: 1
#' @param make_names call make.names to remove weird characters, Default: FALSE
#' @param as.df Convert tibble to data frame? Default: TRUE
#' @export
FirstCol2RowNames <- function(Tibble, rownamecol = 1, make_names = FALSE, as.df = TRUE) {
  .Deprecated("column.2.row.names")

  row.names <- Tibble[[rownamecol]]
  if (as.df) {
    Tibble <- as.data.frame(Tibble)
  }

  Tibble <- Tibble[, -rownamecol, drop = FALSE]
  if (make_names) {
    row.names <- make.names(row.names, unique = TRUE)
  }

  rownames(Tibble) <- row.names
  iprint("Rownames", head(row.names), "...")

  return(Tibble)
}


# _________________________________________________________________________________________________
#' @title FirstCol2RowNames.as.df
#' @description Set First Col to Row Names
#' @param Tibble A dataframe without rownames (tibble style)
#' @param rownamecol rowname column, Default: 1
#' @param make_names call make.names to remove weird characters, Default: FALSE
#' @export

FirstCol2RowNames.as.df <- function(Tibble, rownamecol = 1, make_names = FALSE) {
  .Deprecated("column.2.row.names")

  row.names <- Tibble[[rownamecol]]
  Tibble <- as.data.frame(Tibble)
  rownames(Tibble) <- if (make_names) make.names(row.names, unique = TRUE) else row.names
  return(Tibble[, -rownamecol, drop = FALSE])
}


# __________________________________________________________________________________________________
#' @title Construct File Path
#'
#' @description Constructs a complete file path using either provided manual file name and directory
#'   or defaults to processing a given filename and using the current working directory.
#'
#' @param filename The base file name to process. Default: NULL.
#' @param suffix The file name suffix to be appended. Default: NULL.
#' @param extension The file extension to be appended. Default: NULL.
#' @param manual_file_name An optional manual specification for the file name. Default: NULL.
#' @param manual_directory An optional manual specification for the directory. Default: NULL.
#' @param v verbose Print path? Default: TRUE.
#'
#' @return A string representing the constructed file path.
#' @importFrom Stringendo sppp ParseFullFilePath
#' @examples
#' construct.file.path(
#'   filename = "report", manual_file_name = NULL, manual_directory = NULL,
#'   extension = "txt"
#' )
construct.file.path <- function(
    filename = NULL,
    suffix = NULL,
    extension = NULL,
    manual_file_name = NULL,
    manual_directory = NULL,
    v = TRUE) {
  filename <- as.character(filename) # unclear why thus bf needed.

  # Input argument assertions
  stopifnot(
    is.null(filename) || is.character(filename),
    is.null(manual_file_name) || is.character(manual_file_name),
    is.null(manual_directory) || is.character(manual_directory),
    is.null(extension) || is.character(extension)
  )

  fname <- if (!is.null(manual_file_name)) manual_file_name else Stringendo::sppp(filename, suffix)
  out_dir <- if (!is.null(manual_directory)) manual_directory else getwd()

  # Construct the full file path
  FnP <- Stringendo::ParseFullFilePath(out_dir, fname, extension)

  # Output assertion
  stopifnot(is.character(FnP), nzchar(FnP))

  if (v) {
    try(message(osXpath(FnP)))
    message(FnP, "\n")
  }
  return(FnP)
}


# ____________________________________________________________________________________________ ----
## Reading files in -------------------------------------------------------------------------------------------------


# _________________________________________________________________________________________________
#' @title read.simple.vec
#' @description read.simple.vec
#' @description Read each line of a file to an element of a vector (read in new-line separated values, no header!).
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple.vec("path/to/a/single.column.file")
#' }
#' }
#' @export
read.simple.vec <- function(...) {
  pfn <- Stringendo::kollapse(...) # merge path and filename
  read_in <- as.vector(unlist(read.table(pfn, stringsAsFactors = FALSE, sep = "\n")))
  iprint(length(read_in), "elements")
  return(read_in)
}


# _________________________________________________________________________________________________
#' @title read.simple
#' @description It is essentially read.table() with file/path parsing.
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple("path/to/a/file")
#' }
#' }
#' @export
read.simple <- function(...) {
  pfn <- Stringendo::kollapse(...) # merge path and filename
  read_in <- read.table(pfn, stringsAsFactors = FALSE)
  return(read_in)
}


# _________________________________________________________________________________________________
#' @title read.simple_char_list
#' @description Read in a file.
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple_char_list("path/to/my.file")
#' }
#' }
#' @export
read.simple_char_list <- function(...) {
  pfn <- Stringendo::kollapse(...) # merge path and filename
  read_in <- unlist(read.table(pfn, stringsAsFactors = FALSE))
  # iprint("New variable head: ", what(read_in))
  iprint("New variable head: ", is(read_in), "range", range(read_in))
  return(read_in)
}


# _________________________________________________________________________________________________
#' @title read.simple.table
#' @description Read in a file. default: header defines colnames, no rownames.
#' For rownames give the col nr. with rownames, eg. 1 The header should start
#' with a TAB / First column name should be empty.
#' @param ... Multiple simple variables to parse.
#' @param colnames Are there column names? Default: TRUE
#' @param coltypes What type of variables are in columns? Auto-guessing can be very slow. Default: NULL
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple.table("path/to/my.file")
#' }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_tsv
#' @importFrom gtools na.replace
read.simple.table <- function(..., colnames = TRUE, coltypes = NULL) {
  pfn <- Stringendo::kollapse(...) # merge path and filename
  # read_in = read.table( pfn , stringsAsFactors = FALSE, sep = "\t", header = colnames )
  read_in <- readr::read_tsv(pfn, col_names = colnames, col_types = coltypes)
  iprint("New variable dim: ", dim(read_in))
  read_in <- as.data.frame(gtools::na.replace(data.matrix(read_in), replace = 0))
  return(read_in)
}



# _________________________________________________________________________________________________
#' @title read.simple.tsv
#' @description Read in a file with excel style data: rownames in col1,
#' headers SHIFTED. The header should start with a TAB / First column name
#' should be empty.
#' @param ... Multiple simple variables to parse.
#' @param sep_ Separator character, Default: '  '
#' @param colnames Are there column names?, Default: TRUE
#' @param wRownames With rownames?, Default: TRUE
#' @param coltypes What type of variables are in columns? Auto-guessing can be very slow., Default: NULL
#' @param NaReplace Replace NA-values?, Default: TRUE
#' @param asTibble Load as tibble or dataframe?, Default: FALSE (=load as df)
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple.tsv("path/to/my.file")
#' }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_tsv
#' @importFrom gtools na.replace
read.simple.tsv <- function(
    ..., sep_ = "\t", colnames = TRUE, wRownames = TRUE,
    coltypes = NULL, NaReplace = TRUE, asTibble = FALSE) {
  pfn <- Stringendo::kollapse(...) # merge path and filename
  read_in <- suppressWarnings(readr::read_tsv(pfn, col_names = colnames, col_types = coltypes))
  iprint("New variable dim: ", dim(read_in) - 0:1)

  # if (wRownames) { read_in = FirstCol2RowNames(read_in, as.df = !asTibble ) }
  if (wRownames) {
    read_in <- column.2.row.names(read_in, as_df = !asTibble)
  }

  if (NaReplace) {
    read_in <- as.data.frame(gtools::na.replace(read_in, replace = 0))
  }
  return(read_in)
}




# _________________________________________________________________________________________________
#' @title read.simple.csv
#' @description Read in a file with excel style data: rownames in col1,
#' headers SHIFTED. The header should start with a TAB / First column name
#' should be empty.
#' @param ... Multiple simple variables to parse.
#' @param colnames Are there column names?, Default: TRUE
#' @param coltypes What type of variables are in columns? Auto-guessing can be very slow., Default: NULL
#' @param wRownames With rownames?, Default: TRUE
#' @param NaReplace Replace NA-values?, Default: TRUE
#' @param asTibble Load as tibble or dataframe?, Default: FALSE (=load as df)
#' @param nmax Max number of rows to read, Default: Inf
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple.csv("path/to/my.file")
#' }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_csv
#' @importFrom gtools na.replace
read.simple.csv <- function(
    ..., colnames = TRUE, coltypes = NULL, wRownames = TRUE,
    NaReplace = TRUE, asTibble = FALSE, nmax = Inf) {
  # browser()
  pfn <- Stringendo::kollapse(...) # merge path and filename
  read_in <- suppressWarnings(readr::read_csv(pfn,
    col_names = colnames, col_types = coltypes,
    n_max = nmax
  ))
  iprint("New variable dim: ", dim(read_in) - 0:1)

  # if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (wRownames) {
    read_in <- column.2.row.names(read_in, as_df = !asTibble)
  }

  if (NaReplace) {
    read_in <- as.data.frame(gtools::na.replace(read_in, replace = 0))
  }
  return(read_in)
}

# _________________________________________________________________________________________________
#' @title read.simple.csv.named.vector
#'
#' @description Read in a data frame (csv), and extact a value and a name column, and convert them
#' to a named vector. By default, it assumes the names in the first column and the values
#' excel style named vectors, names in col1,
#' headers SHIFTED. The header should start with a TAB / First column name
#' should be empty.
#' @param file Path to the *.csv file.
#' @param sep Separator character, Default: ';' alternative: ','.
#' @param col_names Are there column names?, Default: TRUE
#' @param value_col Column number of the values in the input data frame. Default: 2
#' @param name_col Column number of the names in the input data frame. Default: 1
#' @param ... Additional arguments passed to \code{\link[readr]{read_csv}} or read_csv2.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple.csv.named.vector("path/to/my.file.csv")
#' }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @export
#' @importFrom readr read_csv read_csv2
read.simple.csv.named.vector <- function(file, sep = ";", col_names = FALSE,
                                         value_col = 2, name_col = 1, ...) {
  stopifnot(is.character(file), length(file) == 1, file.exists(file))

  if (sep == ";") {
    df <- readr::read_csv2(file, col_names = col_names, ...)
  } else if (sep == ",") {
    df <- readr::read_csv(file, col_names = col_names, ...)
  } else {
    stop("Unknown separator: ", sep)
  }

  if (ncol(df) < 2) stop("Less than 2 columns in file: ", file)

  vect <- df[[value_col]]
  names(vect) <- df[[name_col]]
  message("New vectors length is: ", length(vect), "e.g. ", kppc(head(vect)), " ...")

  return(vect)
}


# _________________________________________________________________________________________________
#' @title read.simple.ssv
#' @description Space separeted values. Read in a file with excel style data:
#' rownames in col1, headers SHIFTED. The header should start with a
#' TAB / First column name should be empty.
#' @param ... Multiple simple variables to parse.
#' @param sep_ Separator character, Default: ' '
#' @param colnames Are there column names?, Default: TRUE
#' @param wRownames With rownames?, Default: TRUE
#' @param NaReplace Replace NA-values?, Default: TRUE
#' @param coltypes What type of variables are in columns? Auto-guessing can be very slow., Default: NULL
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple.ssv("path/to/my.file")
#' }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_delim
#' @importFrom gtools na.replace
read.simple.ssv <- function(
    ..., sep_ = " ", colnames = TRUE, wRownames = TRUE, NaReplace = TRUE,
    coltypes = NULL) {
  pfn <- Stringendo::kollapse(...) # merge path and filename
  read_in <- suppressWarnings(readr::read_delim(pfn, delim = sep_, col_names = colnames, col_types = coltypes))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) {
    read_in <- FirstCol2RowNames(read_in)
  }
  if (NaReplace) {
    read_in <- as.data.frame(gtools::na.replace(read_in, replace = 0))
  }
  return(read_in)
}



# _________________________________________________________________________________________________
#' @title read.simple.tsv.named.vector
#' @description Read in a file with excel style named vectors, names in col1,
#' headers SHIFTED. The header should start with a TAB / First column name
#' should be empty.
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # read.simple.tsv.named.vector("path/to/my.file")
#' }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @export
#' @importFrom readr read_tsv
read.simple.tsv.named.vector <- function(...) {
  message("This function should be updated as `read.simple.csv.named.vector`!")
  pfn <- Stringendo::kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = sep_, row.names = 1, header = TRUE )
  read_in <- readr::read_tsv(pfn)
  vect <- read_in[[2]]
  names(vect) <- read_in[[1]]
  iprint("New vectors length is: ", length(vect))
  return(vect)
}



# _________________________________________________________________________________________________
#' @title Read a multi-sheet XLSX easily
#'
#' @description Reads specified sheets from an XLSX file into a list of data frames.
#'              It allows customization of column names, row names, and trimming of white spaces.
#'
#' @param pfn Path and filename of the XLSX file.
#'            Default: Constructed using `Stringendo::kollapse(...)`.
#' @param which_sheets Indices or names of sheets to read from the XLSX file.
#'                     Default: All sheets.
#' @param col_names Logical, whether to use the first row as column names.
#'                  Default: TRUE.
#' @param row_names Numeric, whether to convert a column to row names.
#'                  Default: 1. Use 0 for no conversion. Default: FALSE.
#' @param trim_ws Logical, whether to trim white spaces from column names.
#' @param ... Pass arguments to read.xlsx().
#'
#' @return A list of data frames, each representing a sheet from the XLSX file.
#' @importFrom openxlsx read.xlsx getSheetNames
#' @seealso \code{\link[openxlsx]{read.xlsx}}
#'
#' @export

read.simple.xlsx <- function(
    pfn = Stringendo::kollapse(...), which_sheets,
    col_names = TRUE, row_names = FALSE,
    trim_ws = TRUE
    , ...) {
  # Assertions for input arguments
  stopifnot(is.character(pfn), length(pfn) > 0)
  if (!missing(which_sheets)) stopifnot(is.numeric(which_sheets) | is.character(which_sheets))
  stopifnot(is.logical(col_names), is.logical(trim_ws))

  # Check if openxlsx package is installed
  if (!require("openxlsx")) {
    stop("Package 'openxlsx' is required but not installed. Please install it using install.packages('openxlsx').")
  }
  # browser()
  # Read sheet names and count
  ls.sheet.names <- openxlsx::getSheetNames(pfn)
  nr.sheets <- length(ls.sheet.names)
  stopifnot(nr.sheets > 0) # Assert that there are sheets in the file

  # Prepare sheet index
  range.of.sheets <- if (missing(which_sheets)) 1:nr.sheets else which_sheets

  # Read specified sheets
  ls.excel.sheets <- lapply(range.of.sheets, function(i) {
    sheet_data <- openxlsx::read.xlsx(pfn,
      sheet = i, colNames = col_names,
      rowNames = row_names, ...
    )
    if (row_names) {
      sheet_data <- column.2.row.names(sheet_data,
        rowname_column = row_names,
        make_names = FALSE, as_df = TRUE
      )
    }
    sheet_data
  })

  # Output assertions
  stopifnot(all(sapply(ls.excel.sheets, function(x) is.data.frame(x))))

  names(ls.excel.sheets) <- ls.sheet.names[range.of.sheets]
  return(ls.excel.sheets)
}


# ____________________________________________________________________________________________ ----
## Writing files out ------------------------------------------------------------------------------

#' @title Append or write a vector to standard file, one element per line.
#'
#' @description Alternative to clipboard. This function takes a vector and appends it
#' to a specified file.
#'
#' @param vec A vector to be written to the file. Default: `LETTERS[1:11]`.
#' @param append Logical flag indicating whether to append the vector to the file. Default: `TRUE`.
#' @param file_path A string specifying the file path where the vector will be written. Default:
#' `"/groups/knoblich/Projects/connectomics/Analysis/__clipboard.txt"`.
#'
#' @examples
#' \dontrun{
#' write.simplest(letters[1:5], append = TRUE)
#' }
#'
#' @return A message indicating the length of the vector and the file path to which it was written.
#' @importFrom checkmate assert_vector assert_character assert_flag
#'
#' @export
write.simplest <- function(vec = LETTERS[1:11], append = TRUE,
                           file_path = "/groups/knoblich/Projects/connectomics/Analysis/__clipboard.txt") {
  stopifnot(
    is.vector(vec),
    is.character(file_path),
    length(file_path) == 1,
    file_path != ""
  )

  # Append vector to file
  if (append) {
    write("\n\n# -----------------------------------------------------------------------",
      file = file_path, append = TRUE
    )
  }

  write(kppws(substitute(vec), idate()), file = file_path, append = TRUE)
  write.table(vec,
    file = file_path, sep = "\n", row.names = FALSE, col.names = FALSE,
    quote = FALSE, append = append
  )
  message("Vector of length ", length(vec), " e.g.: ", kppc(head(vec)), ", is written to: \n", file_path)

  guessed_local_path <- gsub(
    x = file_path,
    pattern = "/groups/knoblich/Projects/connectomics/Analysis/",
    replacement = "/Volumes/Analysis/"
  )
  message("open ", guessed_local_path)
}

# write.simplest()


# _________________________________________________________________________________________________
#' @title Write Simple
#'
#' @description Writes a matrix-like R object (e.g., a data frame) to a file as tab-separated
#'   values (.tsv). The output filename can be auto-generated from the variable's name or manually
#'   specified. The file is saved in the specified output directory or the current working directory.
#'   The path and variable name can be passed separately and will be concatenated to form the filename.
#' @param input_df Data frame to write out. Default: None, must be provided.
#' @param filename The base name for the output file. Default: Name of the input data frame.
#' @param suffix An optional suffix to add to the filename. Default: NULL.
#' @param extension File extension to use. Default: 'tsv'.
#' @param manual_file_name Manually defined filename, overrides automatic naming. Default: NULL.
#' @param manual_directory Directory to save the file in, overrides default directory. Default: NULL.
#' @param o If TRUE, opens the file after writing on OS X using 'system(open ...)'. Default: FALSE.
#' @param v verbose Print path? Default: TRUE.
#' @return Outputs a .tsv file and optionally prints the length of the input data frame.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   write.simple(input_df = myDataFrame)
#' }
#' }
#' @export
write.simple <- function(input_df, filename = substitute(input_df), suffix = NULL, extension = "tsv",
                         manual_file_name = NULL, manual_directory = NULL, o = FALSE,
                         v = TRUE) {
  # Input argument assertions

  stopifnot(
    is.data.frame(input_df),
    is.null(suffix) || is.character(suffix),
    is.character(extension),
    is.null(manual_file_name) || is.character(manual_file_name),
    is.null(manual_directory) || is.character(manual_directory),
    is.logical(o)
  )

  FnP <- construct.file.path(
    v = v,
    filename = FixPlotName(make.names(filename)), suffix = suffix, extension = extension,
    manual_file_name = manual_file_name, manual_directory = manual_directory
  )

  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

  # Optional: open the file after writing (specific to OS X)
  if (o) {
    system(paste0("open ", FnP), wait = FALSE)
  }
  iprint("Length: ", length(input_df))
}



# _________________________________________________________________________________________________
#' @title Write Simple Vector
#'
#' @description Writes a vector-like R object to a file as newline separated values (.vec).
#'   The output filename can be auto-generated from the variable's name or manually specified. The file
#'   is saved in the specified output directory or the current working directory. The path and variable
#'   name can be passed separately and will be concatenated to form the filename.
#' @param input_vec Vector to write out. Default: None, must be provided.
#' @param filename The base name for the output file. Default: Name of the input vector.
#' @param suffix An optional suffix to add to the filename. Default: NULL.
#' @param extension File extension to use. Default: 'vec'.
#' @param manual_file_name Manually defined filename, overrides automatic naming. Default: NULL.
#' @param manual_directory Directory to save the file in, overrides default directory. Default: NULL.
#' @param o If TRUE, opens the file after writing on OS X using 'system(open ...)'. Default: FALSE.
#' @param v verbose Print path? Default: TRUE.
#'
#' @return Outputs a .vec file and optionally prints the length of the input vector.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   write.simple.vec(input_vec = myVector)
#' }
#' }
#' @export
write.simple.vec <- function(input_vec, filename = substitute(input_vec), suffix = NULL, extension = "vec",
                             manual_file_name = NULL, manual_directory = NULL, o = FALSE,
                             v = TRUE) {
  # Input argument assertions
  stopifnot(
    is.vector(input_vec),
    is.null(suffix) || is.character(suffix),
    is.character(extension),
    is.null(manual_file_name) || is.character(manual_file_name),
    is.null(manual_directory) || is.character(manual_directory),
    is.logical(o)
  )

  FnP <- construct.file.path(
    v = v,
    filename = FixPlotName(make.names(filename)), suffix = suffix, extension = extension,
    manual_file_name = manual_file_name, manual_directory = manual_directory
  )

  # Write the vector to a file
  write.table(input_vec, file = FnP, sep = "\n", row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Optional output: length of the vector
  invisible(length(input_vec))

  # Optional: open the file after writing (specific to OS X)
  if (o) {
    system(paste0("open ", FnP), wait = FALSE)
  }
}


# _________________________________________________________________________________________________
#' @title write.simple.tsv
#'
#' @description Write out a matrix-like R-object WITH ROW- AND COLUMN- NAMES to a file with as tab separated
#' values (.tsv). Your output filename will be either the variable's name. The output file will be
#' located in "OutDir" specified by you at the beginning of the script, or under your current
#' working directory. You can pass the PATH and VARIABLE separately (in order), they will be
#' concatenated to the filename. If col.names = NA and row.names = TRUE a blank column name is added,
#' which is the convention used for CSV files to be read by spreadsheets.
#' @param input_df Your Dataframe with row- and column-names
#' @param ... Pass any other argument to the kollapse() function used for file name.
#' @param separator Field separator, such as "," for csv
#' @param filename The base name for the output file. Default: Name of the input vector.
#' @param extension e.g.: tsv
#' @param suffix A suffix added to the filename, Default: NULL
#' @param manual_file_name Specify full filename if you do not want to name it by the variable name.
#' @param manual_directory Specify the directory where the file should be saved.
#' @param row_names Write row names? TRUE by default
#' @param col_names Write column names? NA by default, TRUE if row_names == FALSE
#' @param gzip Compress the file after saving? FALSE by default
#' @param o Open the file after saving? FALSE by default
#' @param v verbose Print path? Default: TRUE.
#' @param ... Additional arguments passed to write.table()
#'
#' @examples YourDataFrameWithRowAndColumnNames <- cbind("A" = rnorm(100), "B" = rpois(100, 8))
#' rownames(YourDataFrameWithRowAndColumnNames) <- letters[1:NROW(YourDataFrameWithRowAndColumnNames)]
#' write.simple.tsv(YourDataFrameWithRowAndColumnNames)
#'
#' @export
write.simple.tsv <- function(
    input_df,
    separator = "\t", extension = "tsv",
    filename = substitute(input_df),
    suffix = NULL,
    manual_file_name = NULL,
    manual_directory = NULL,
    row_names = TRUE,
    col_names = NA,
    gzip = FALSE,
    o = FALSE,
    v = TRUE,
    ...) {
  #
  if (row_names == FALSE) {
    col_names <- TRUE
  }

  " write.simple.tsv should have background compression as a feature #14 "

  if (separator %in% c(",", ";")) extension <- "csv"

  fname <- Stringendo::kollapse(..., print = FALSE)
  if (nchar(fname) < 2) fname <- filename

  FnP <- construct.file.path(
    v = v,
    filename = FixPlotName(make.names(fname)), suffix = suffix, extension = extension,
    manual_file_name = manual_file_name, manual_directory = manual_directory
  )
  # print(FnP)

  write.table(input_df,
    file = FnP, sep = separator,
    row.names = row_names,
    col.names = col_names,
    quote = FALSE, ...
  )

  printme <- if (length(dim(input_df))) {
    paste0("Dim: ", dim(input_df))
  } else {
    paste0("Length (of your vector): ", length(input_df))
  }
  iprint(printme)
  if (o) {
    system(paste0("open ", FnP), wait = FALSE)
  }
  if (gzip) {
    system(paste0("gzip ", FnP), wait = FALSE)
  }
}




# _________________________________________________________________________________________________
#' @title Write Simple Append
#'
#' @description Appends a data frame without row names to an existing .tsv file with the same number
#'   of columns. The output filename is auto-generated from the variable's name or manually specified.
#'   The file is saved in the specified output directory or the current working directory. The path and
#'   variable name can be passed separately and will be concatenated to form the filename.
#' @param input_df Data frame to write out. Default: None, must be provided.
#' @param filename The base name for the output file. Default: Name of the input data frame.
#' @param suffix An optional suffix to add to the filename. Default: NULL.
#' @param extension File extension to use, Default: 'tsv'.
#' @param manualFileName Manually defined filename, overrides automatic naming. Default: NULL.
#' @param manualDirectory Directory to save the file in, overrides default directory. Default: NULL.
#' @param o If TRUE, opens the file after writing on OS X using 'system(open ...)'. Default: FALSE.
#' @param v verbose Print path? Default: TRUE.
#'
#' @return Appends data to an existing .tsv file.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   write.simple.append(input_df = myDataFrame)
#' }
#' }
#' @export
write.simple.append <- function(input_df, filename = substitute(input_df), suffix = NULL, extension = "tsv",
                                manualFileName = NULL, manualDirectory = NULL, o = FALSE,
                                v = TRUE) {
  stopifnot(
    # is.data.frame(input_df),
    is.null(suffix) || is.character(suffix),
    is.character(extension),
    is.null(manualFileName) || is.character(manualFileName),
    is.null(manualDirectory) || is.character(manualDirectory),
    is.logical(o)
  )

  FnP <- construct.file.path(
    v = v,
    filename = FixPlotName(make.names(filename)), suffix = suffix, extension = extension,
    manualFileName = manualFileName, manualDirectory = manualDirectory
  )

  # Write (append) the data frame to a file
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)

  # Optional: open the file after writing (specific to OS X)
  if (o) {
    system(paste0("open ", FnP), wait = FALSE)
  }
}



# _________________________________________________________________________________________________

# _________________________________________________________________________________________________
#' @title Write Simple XLSX
#'
#' @description Write out a list of matrices or data frames with row and column names
#'   to an Excel (.xlsx) file. The output filename is generated based on the provided parameters
#'   and stored in the specified output directory or the current working directory.
#'   The function offers various styling and formatting options for the Excel file.
#'
#' @param named_list A list of data frames or matrices to write out.
#'                   Default: No default value, a list must be provided.
#' @param rowname_column The column name or index to use as row names in the Excel file.
#'                    Required, no default value.
#' @param filename The base name for the output file, derived from the 'named_list' variable if not specified.
#'                 Default: Derived using 'substitute(named_list)'.
#' @param suffix A suffix to be added to the output filename. Default: NULL.
#' @param manual_file_name Manually defined filename, overrides automatic naming. Default: NULL.
#' @param manual_directory Directory to save the file in, overrides default directory. Default: NULL.
#' @param o Logical; if TRUE, opens the file after writing using the system's default application.
#'          Default: FALSE.
#' @param TabColor Color for the tabs in Excel. Default: 'darkgoldenrod1'.
#' @param Creator The creator of the Excel document. Default: ''.
#' @param HeaderCex Font size for the header. Default: 12.
#' @param HeaderLineColor Color for the header line. Default: 'darkolivegreen3'.
#' @param HeaderCharStyle Character style for the header (e.g., 'bold', 'italic', 'underline').
#'                        Default: 'bold'.
#' @param has_row_names Logical; if set to FALSE, converts the first column to row names. Default: TRUE
#' @param FreezeFirstRow Logical; if TRUE, freezes the first row in Excel. Default: TRUE.
#' @param FreezeFirstCol Logical; if TRUE, freezes the first column in Excel. Default: FALSE.
#' @param v verbose Print path? Default: TRUE.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Example usage:
#'   # write.simple.xlsx(my.list.of.data.frames, rowname_column = "gene")
#' }
#' }
#' @seealso
#'   \code{\link[openxlsx]{write.xlsx}}
#' @export
#' @importFrom openxlsx write.xlsx createStyle

write.simple.xlsx <- function(
    named_list,
    rowname_column = 1, #  'gene' # for Seurat df.markers
    filename = substitute(named_list),
    suffix = NULL,
    manual_file_name = NULL,
    manual_directory = NULL,
    o = FALSE,
    TabColor = "darkgoldenrod1", HeaderLineColor = "darkolivegreen3",
    HeaderCex = 12, Creator = "",
    HeaderCharStyle = c("bold", "italic", "underline")[1],
    has_row_names = TRUE,
    FreezeFirstRow = TRUE, FreezeFirstCol = FALSE,
    v = TRUE) {
  # Assertions for input arguments
  stopifnot(
    is.list(named_list),
    all(sapply(named_list, function(x) is.matrix(x) || is.data.frame(x)))
  )

  if (!("list" %in% class(named_list))) named_list <- list(named_list) # convert to a list if needed

  # Create header style
  hs <- openxlsx::createStyle(
    textDecoration = HeaderCharStyle, fontSize = HeaderCex,
    fgFill = HeaderLineColor
  )

  # assign row names if required
  if (!has_row_names) {
    assignRownames <- function(x) column.2.row.names(df, rowname_column = rowname_column, make_names = TRUE)
    named_list <- lapply(named_list, assignRownames)
    message("Converting column ", rowname_column, " to row names: ", head(rownames(named_list[[1]])))
  }

  FnP <- construct.file.path(
    v = v,
    filename = FixPlotName(make.names(filename)), suffix = suffix, extension = "xlsx",
    manual_file_name = manual_file_name, manual_directory = manual_directory
  )

  openxlsx::write.xlsx(
    x = named_list, file = FnP, rowNames = has_row_names,
    firstRow = FreezeFirstRow, firstCol = FreezeFirstCol,
    headerStyle = hs, tabColour = TabColor,
    colWidths = "auto", creator = Creator
  )

  # Output assertion
  stopifnot(file.exists(FnP))

  if (o) {
    system(paste0("open ", fix_special_characters_bash(FnP)), wait = FALSE)
  }
} # fun



# ____________________________________________________________________________________________ ----
## Reexport files ------------------------------------------------------------------------------

#' @title Convert and save a .qs file to different formats
#'
#' @description
#' This function reads in a `.qs` file and resaves it as either a `.tsv`, `.csv`, semicolon-separated
#' `.csv` (csv2), or Excel file based on the `out_file` argument.
#'
#' @param path A character string specifying the path to the `.qs` file. Default: none.
#' @param out_file A character string specifying the output file format. One of `"tsv"` (default),
#'   `"csv"`, `"csv2"` (semicolon-separated), or `"excel"`. Default: `"tsv"`.
#'
#' @return The function does not return a value but writes the file to disk in the specified format.
#'
#' @importFrom qs qread
#' @export
#'

qs.2.table <- function(path, out_file = c("tsv", "csv", "csv2", "excel")[1]) {
  # Ensure that the file exists and is a .qs file
  stopifnot(file.exists(path), stringi::stri_detect(str = path, regex = "\\.qs$"))

  # Ensure out_file is one of the allowed choices
  out_file <- match.arg(out_file, c("tsv", "csv", "csv2", "excel"))

  # Read in the .qs file
  data <- qs:qread(path)

  # Determine the output file extension and write the file based on the output format
  path_out <- ppp(base_filename, out_file)

  if (out_file == "excel") {
    # out_path <- ppp(base_filename, "xlsx")
    ppp(base_filename, out_file)
    ReadWriter::write.simple.xlsx(data, out_path)
  }

  if (out_file == "tsv") {
    ReadWriter::write.simple.tsv(data, path_out, separator = "\t")
  } else if (out_file == "csv") {
    ReadWriter::write.simple.tsv(data, path_out, separator = ",")
  } else if (out_file == "csv2") {
    out_path <- paste0(base_filename, ".csv")
    ReadWriter::write.simple.tsv(data, path_out, separator = ";")
  } else if (out_file == "excel") {
    out_path <- paste0(base_filename, ".xlsx")
    ReadWriter::write.simple.xlsx(data, out_path)
  }

  message("File saved as: ", out_path)
}
