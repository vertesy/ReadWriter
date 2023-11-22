######################################################################
# ReadWriter.R
######################################################################
# source('~/GitHub/Packages/ReadWriter/R/ReadWriter.R')
# devtools::load_all('~/GitHub/Packages/ReadWriter');
# devtools::document('~/GitHub/Packages/ReadWriter');


## File handling, export, import [read & write] -------------------------------------------------------------------------------------------------

### Aux -------------------------------------------------------------------------------------------------


#' @title Convert a Column to Row Names in a Tibble or DataFrame
#'
#' @description Converts the first column (or a specified column) of a dataframe or tibble into row names.
#' This function differs from `tibble::column_to_rownames` in that it takes column names or inices and
#' it offers the option to sanitize row names using `make.names`, provides a warning if there are
#' duplicated values in the row name column
#'
#' @param df A dataframe or tibble without row names.
#'           Default: No default value, a dataframe must be provided.
#' @param rowname_col Index of the column to be used as row names.
#'                    Default: 1.
#' @param make_names Boolean indicating whether to call `make.names` to sanitize row names.
#'                   Default: FALSE.
#' @param as_df Boolean indicating whether to convert the input to a dataframe if it's not already one.
#'              Default: TRUE.
#' @param warn Warn user if row names pre-exist.
#' @param ... Pass arguments to make.names()..
#' @export

column.2.row.names <- function(tibble, rowname_col = 1, make_names = FALSE, as_df = TRUE
                               , warn = TRUE, ...) {

  # Assertions
  stopifnot(is.data.frame(tibble), is.numeric(rowname_col), rowname_col > 0, rowname_col <= ncol(tibble)
            , is.logical(make_names), is.logical(as_df))

  if (!is.null(rownames(tibble))) {
    if (warn) {
      warning("tibble/df already has row names (now overwritten):", immediate. = T)
      print(head(rownames(tibble)))
    }
  }

  if (as_df) { tibble <- as.data.frame(tibble) }

  # Extracting the specified column to be used as row names
  row_names <- tibble[[rowname_col]]

  # Check for duplicated row names
  if(anyDuplicated(rowname_col)) {
    is.duplicated <- rowname_col[which(duplicated(rowname_col))]
    warning(length(is.duplicated), " duplicated entries in: ", substitute(rowname_col)
            , "\narg make_names = TRUE will enforce uniqueness")
  }

  # Applying make.names if requested
  if (make_names) { row_names <- make.names(row_names, unique = TRUE, ...) }

  # Removing the rowname column from the dataframe
  tibble <- tibble[, -rowname_col, drop = FALSE]

  # Setting the row names
  rownames(tibble) <- row_names

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

  .Deprecated('column.2.row.names')

  row.names <- Tibble[[rownamecol]]
  if (as.df) { Tibble <- as.data.frame(Tibble) }

  Tibble <- Tibble[ ,-rownamecol, drop =F]
  if (make_names) { row.names <- make.names(row.names, unique = TRUE) }

  rownames(Tibble) <- row.names
  iprint("Rownames", head(row.names), '...')

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

  .Deprecated('column.2.row.names')

  row.names = Tibble[[rownamecol]]
  Tibble = as.data.frame(Tibble)
  rownames(Tibble) = if (make_names) make.names(row.names, unique = TRUE) else row.names
  return(Tibble[, -rownamecol, drop = F])
}



### Reading files in -------------------------------------------------------------------------------------------------


# _________________________________________________________________________________________________
#' @title read.simple.vec
#' @description read.simple.vec
#' @description Read each line of a file to an element of a vector (read in new-line separated values, no header!).
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # read.simple.vec("path/to/a/single.column.file")
#'  }
#' }
#' @export
read.simple.vec <- function(...) {
  pfn = kollapse(...) # merge path and filename
  read_in = as.vector(unlist(read.table( pfn , stringsAsFactors = FALSE, sep = "\n" )) )
  iprint(length(read_in), "elements")
  return(read_in);
}


# _________________________________________________________________________________________________
#' @title read.simple
#' @description It is essentially read.table() with file/path parsing.
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # read.simple("path/to/a/file")
#'  }
#' }
#' @export
read.simple <- function(...) {
  pfn = kollapse(...) # merge path and filename
  read_in = read.table( pfn , stringsAsFactors = FALSE)
  return(read_in)
}


# _________________________________________________________________________________________________
#' @title read.simple_char_list
#' @description Read in a file.
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # read.simple_char_list("path/to/my.file")
#'  }
#' }
#' @export
read.simple_char_list <- function(...) {
  pfn = kollapse(...) # merge path and filename
  read_in = unlist(read.table( pfn , stringsAsFactors = FALSE ) )
  # iprint("New variable head: ", what(read_in))
  iprint("New variable head: ", is(read_in), 'range', range(read_in))
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
#' if(interactive()){
#'  # read.simple.table("path/to/my.file")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_tsv
#' @importFrom gtools na.replace
read.simple.table <- function(..., colnames = TRUE, coltypes = NULL) {
  pfn = kollapse(...) # merge path and filename
  # read_in = read.table( pfn , stringsAsFactors = FALSE, sep = "\t", header = colnames )
  read_in = readr::read_tsv( pfn, col_names = colnames, col_types = coltypes )
  iprint("New variable dim: ", dim(read_in))
  read_in = as.data.frame(gtools::na.replace(data.matrix(read_in), replace = 0))
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
#' if(interactive()){
#'  # read.simple.tsv("path/to/my.file")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_tsv
#' @importFrom gtools na.replace
read.simple.tsv <- function(..., sep_ = "\t", colnames = TRUE, wRownames = TRUE, coltypes = NULL, NaReplace = TRUE, asTibble = FALSE) {
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = , sep_, row.names = 1, header = TRUE )
  read_in = suppressWarnings(readr::read_tsv( pfn, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in, as.df = !asTibble ) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
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
#' @param nmax Max number of rows to read, Default: Inf
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # read.simple.csv("path/to/my.file")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_csv
#' @importFrom gtools na.replace
read.simple.csv <- function(...,  colnames = TRUE, coltypes = NULL, wRownames = TRUE, NaReplace = TRUE, nmax = Inf) {
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_csv( pfn, col_names = colnames, col_types = coltypes, n_max = nmax ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
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
#' if(interactive()){
#'  # read.simple.ssv("path/to/my.file")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom readr read_delim
#' @importFrom gtools na.replace
read.simple.ssv <- function(..., sep_ = " ", colnames = TRUE, wRownames = TRUE, NaReplace = TRUE, coltypes = NULL) {
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_delim( pfn, delim = sep_, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
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
#' if(interactive()){
#'  # read.simple.tsv.named.vector("path/to/my.file")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @export
#' @importFrom readr read_tsv
read.simple.tsv.named.vector <- function(...) {
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = sep_, row.names = 1, header = TRUE )
  read_in = readr::read_tsv( pfn )
  vect = read_in[[2]]
  names(vect) = read_in[[1]]
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
#'            Default: Constructed using `kollapse(...)`.
#' @param which_sheets Indices or names of sheets to read from the XLSX file.
#'                     Default: All sheets.
#' @param col_names Logical, whether to use the first row as column names.
#'                  Default: TRUE.
#' @param row_names Numeric, whether to convert a column to row names.
#'                  Default: 1. Use 0 for no conversion.
#' @param ... Pass arguments to read.xlsx().
#' @return A list of data frames, each representing a sheet from the XLSX file.
#' @importFrom openxlsx read.xlsx getSheetNames
#' @export

read.simple.xlsx <- function(pfn = kollapse(...), which_sheets
                             , col_names = TRUE, row_names = 0
                             , trim_ws = TRUE, ...) {

  # Assertions for input arguments
  stopifnot(is.character(pfn), length(pfn) > 0)
  if (!missing(which_sheets)) stopifnot(is.numeric(which_sheets) | is.character(which_sheets))
  stopifnot(is.logical(col_names), is.logical(trim_ws))

  # Check if openxlsx package is installed
  if (!require("openxlsx")) {
    stop("Package 'openxlsx' is required but not installed. Please install it using install.packages('openxlsx').")
  }

  # Read sheet names and count
  ls.sheet.names = openxlsx::getSheetNames(pfn)
  nr.sheets = length(ls.sheet.names)
  stopifnot(nr.sheets > 0) # Assert that there are sheets in the file

  # Prepare sheet index
  range.of.sheets = if (missing(which_sheets)) 1:nr.sheets else which_sheets

  # Read specified sheets
  ls.excel.sheets = lapply(range.of.sheets, function(i) {
    sheet_data <- openxlsx::read.xlsx(pfn, sheet = i, colNames = col_names
                                      , rowNames =1, ...)
    if (row_names) {
      sheet_data <- column.2.row.names(sheet_data, rowname_col = row_names
                                       , make_names = FALSE, as_df = TRUE)
    }
    sheet_data
  })

  # Output assertions
  stopifnot(all(sapply(ls.excel.sheets, function(x) is.data.frame(x))))

  names(ls.excel.sheets) <- ls.sheet.names[range.of.sheets]
  return(ls.excel.sheets)
}




### Writing files out ------------------------------------------------------------------------------


# _________________________________________________________________________________________________
#' @title write.simple
#'
#' @description Write out a matrix-like R-object to a file with as tab separated
#'   values (.tsv). Your output filename will be either the variable's name. The
#'   output file will be located in "OutDir" specified by you at the beginning
#'   of the script, or under your current working directory. You can pass the
#'   PATH and VARIABLE separately (in order), they will be concatenated to the
#'   filename.
#' @param input_df Data frame to write out.
#' @param extension File extension to add, Default: 'tsv'
#' @param suffix A suffix added to the filename, Default: NULL
#' @param ManualName A string for a manually defined filename. Default: ''
#' @param o Set to TRUE to open file after writing out using 'system(open ...)' on OS X., Default: FALSE
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # write.simple.vec(my.vector)
#'  }
#' }
#' @export
write.simple <- function(input_df, extension = 'tsv'
                         , filename = substitute(input_df)
                         , ManualName = ""
                         , o = FALSE, ...  ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = Stringendo::sppp(filename, suffix) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP = ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
  iprint("Length: ", length(input_df))
} # fun


# _________________________________________________________________________________________________
#' @title write.simple.vec
#'
#' @description Write out a vector-like R-object to a file with as newline
#'   separated values (.vec). Your output filename will be either the variable's
#'   name. The output file will be located in "OutDir" specified by you at the
#'   beginning of the script, or under your current working directory. You can
#'   pass the PATH and VARIABLE separately (in order), they will be concatenated
#'   to the filename.
#' @param input_vec Vector to write out.
#' @param extension File extension to add, Default: 'vec'
#' @param suffix A suffix added to the filename, Default: NULL
#' @param ManualName A string for a manually defined filename. Default: ''
#' @param o Set to TRUE to open file after writing out using 'system(open ...)' on OS X., Default: FALSE
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # write.simple.vec(my.vector)
#'  }
#' }
#' @export
write.simple.vec <- function(input_vec, extension = 'vec'
                             , filename = substitute(input_vec)
                             , suffix = NULL, ManualName = ""
                             , o = FALSE, ... ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = Stringendo::sppp(filename, suffix) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_vec, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE  )
  iprint("Length: ", length(input_vec))
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun


# _________________________________________________________________________________________________
#' @title write.simple.tsv
#'
#' @description Write out a matrix-like R-object WITH ROW- AND COLUMN- NAMES to a file with as tab separated
#' values (.tsv). Your output filename will be either the variable's name. The output file will be
#' located in "OutDir" specified by you at the beginning of the script, or under your current
#' working directory. You can pass the PATH and VARIABLE separately (in order), they will be
#' concatenated to the filename.
#' @param input_df Your Dataframe with row- and column-names
#' @param separator Field separator, such as "," for csv
#' @param extension e.g.: tsv
#' @param suffix A suffix added to the filename, Default: NULL
#' @param ManualName Specify full filename if you do not want to name it by the variable name.
#' @param row_names Write row names? TRUE by default
#' @param col_names Write column names? NA by default, TRUE if row_names == FALSE
#' @param o Open the file after saving? FALSE by default
#' @param gzip Compress the file after saving? FALSE by default
#' @param ... Pass any other argument to the kollapse() function used for file name.
#' @export
#' @examples YourDataFrameWithRowAndColumnNames = cbind("A" = rnorm(100), "B" = rpois(100, 8))
#' rownames(YourDataFrameWithRowAndColumnNames) = letters[1:NROW(YourDataFrameWithRowAndColumnNames)]
#' write.simple.tsv(YourDataFrameWithRowAndColumnNames)

write.simple.tsv <- function(input_df, separator = "\t", extension = 'tsv'
                             , filename = substitute(input_df)
                             , suffix = NULL, ManualName = ""
                             , row_names = TRUE
                             , col_names = NA
                             , o = FALSE, gzip = FALSE
                             # , converFromTibble = T
                             , ...  ) {

  # if (converFromTibble) { if (tibble::is_tibble(input_df)) { input_df <- as.data.frame(input_df) } }
  if (row_names == FALSE) { col_names = TRUE }
  if (separator %in% c(',', ';')) extension <- 'csv'

  fname = kollapse (..., print = FALSE)
  if (nchar (fname) < 2 ) { fname <-Stringendo::sppp(filename, suffix) }

  if (nchar(ManualName)) {FnP = kollapse(ManualName)
  } else { FnP = ww.FnP_parser(fname, extension) }
  utils::write.table (input_df, file = FnP, sep = separator
                      , row.names = row_names
                      , col.names = col_names
                      , quote = FALSE  )
  printme = if (length(dim(input_df))) {
    paste0("Dim: ", dim(input_df) )
  }else {
    paste0("Length (of your vector): ", length(input_df) )
  }
  iprint (printme)
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
  if (gzip) { system(paste0("gzip ", FnP), wait = FALSE) }
} # fun

# If col.names = NA and row.names = TRUE a blank column name is added, which is the convention used
# for CSV files to be read by spreadsheets.




# _________________________________________________________________________________________________
#' @title write.simple.append
#'
#' @description Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of
#'  the same number of columns. Your output filename will be either the
#'  variable's name. The output file will be located in "OutDir" specified by
#'  you at the beginning of the script, or under your current working directory.
#'  You can pass the PATH and VARIABLE separately (in order), they will be
#'  concatenated to the filename.
#' @param input_df Data frame to write out.
#' @param extension File extension to add, Default: 'tsv'
#' @param suffix A suffix added to the filename, Default: NULL
#' @param ManualName A string for a manually defined filename. Default: ''
#' @param o Set to TRUE to open file after writing out using 'system(open ...)' on OS X., Default: FALSE
#' @param ... Multiple simple variables to parse.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # write.simple.append(my.data.frame)
#'  }
#' }
#' @export
write.simple.append <- function(input_df, extension = 'tsv'
                                , filename = substitute(input_df)
                                , suffix = NULL, ManualName = ""
                                , o = FALSE, ... ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = Stringendo::sppp(filename, suffix) }
  if (nchar(ManualName)) { FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE  )
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun


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
#' @param filename The base name for the output file, derived from the 'named_list' variable if not specified.
#'                 Default: Derived using 'substitute(named_list)'.
#' @param rowname_col The column name or index to use as row names in the Excel file.
#'                    Required, no default value.
#' @param suffix A suffix to be added to the output filename. Default: NULL.
#' @param o Logical; if TRUE, opens the file after writing using the system's default application.
#'          Default: FALSE.
#' @param TabColor Color for the tabs in Excel. Default: 'darkgoldenrod1'.
#' @param Creator The creator of the Excel document. Default: ''.
#' @param HeaderCex Font size for the header. Default: 12.
#' @param HeaderLineColor Color for the header line. Default: 'darkolivegreen3'.
#' @param HeaderCharStyle Character style for the header (e.g., 'bold', 'italic', 'underline').
#'                        Default: 'bold'.
#' @param FreezeFirstRow Logical; if TRUE, freezes the first row in Excel. Default: TRUE.
#' @param FreezeFirstCol Logical; if TRUE, freezes the first column in Excel. Default: FALSE.
#' @param has_row_names Logical; if set to FALSE, converts the first column to row names. Default: TRUE
#' @param ... Additional arguments passed to 'write.xlsx()'.
#' @examples
#' \dontrun{
#'   if (interactive()) {
#'     # Example usage:
#'     # write.simple.xlsx(my.list.of.data.frames, rowname_col = "gene")
#'   }
#' }
#' @seealso
#'   \code{\link[openxlsx]{write.xlsx}}
#' @export
#' @importFrom openxlsx write.xlsx createStyle

write.simple.xlsx <- function(named_list
                              , filename = substitute(named_list)
                              , rowname_col #  'gene' # for Seurat df.markers
                              , suffix = NULL
                              , o = FALSE
                              , TabColor = "darkgoldenrod1", HeaderLineColor = "darkolivegreen3"
                              , HeaderCex = 12, Creator = ""
                              , HeaderCharStyle = c("bold", "italic", "underline")[1]
                              , FreezeFirstRow = TRUE, FreezeFirstCol = FALSE
                              , has_row_names = TRUE, ...) {

  # Assertions for input arguments
  stopifnot(is.list(named_list), all(sapply(named_list, function(x) is.matrix(x) || is.data.frame(x))))

  fname <- Stringendo::sppp(filename, suffix)
  if ( !('list' %in% class(named_list))  ) named_list <- list(named_list) # convert to a list if needed

  # Further checks and adjustments for filename
  "This is a very hack solution that should be"
  if (nchar(fname) > 100) fname <- kpp('_Output', idate())
  FnP <- kpp(kpps(getwd(), fname), "xlsx")

  # Create header style
  hs <- openxlsx::createStyle(textDecoration = HeaderCharStyle, fontSize = HeaderCex
                              , fgFill = HeaderLineColor)

  # assign row names if required
  if (!has_row_names) {
    # assignRownames <- function(x) tibble::rownames_to_column(as.data.frame(x), var = "genes")
    assignRownames <- function(x) column.2.row.names(df, rowname_col = rowname_col, make_names = T)
    named_list <- lapply(named_list, assignRownames)
  }

  print(FnP)

  openxlsx::write.xlsx(x = named_list, file = FnP, rowNames = has_row_names
                       , firstRow = FreezeFirstRow, firstCol = FreezeFirstCol
                       , headerStyle = hs, tabColour = TabColor
                       , colWidths = "auto", creator = Creator)

  # Output assertion
  stopifnot(file.exists(FnP))

  if (o) { system(paste0("open ", fix_special_characters_bash(FnP)), wait = FALSE) }
} # fun


