######################################################################
# ReadWriter.R
######################################################################
# source('~/GitHub/Packages/ReadWriter/R/ReadWriter.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T)

## File handling, export, import [read & write] -------------------------------------------------------------------------------------------------

### Aux -------------------------------------------------------------------------------------------------




# _________________________________________________________________________________________________
#' @title FirstCol2RowNames
#' @description Set First Col to Row Names
#' @param Tibble A dataframe without rownames (tibble style)
#' @param rownamecol rowname column, Default: 1
#' @param make_names call make.names to remove weird characters, Default: FALSE
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
FirstCol2RowNames <- function(Tibble, rownamecol = 1, make_names = FALSE) {
  Tibble = as.data.frame(Tibble)
  NN = Tibble[[rownamecol]]
  rownames(Tibble) = if (make_names) make.names(NN, unique = TRUE) else NN
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
  iprint("New variable head: ", what(read_in))
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
read.simple.tsv <- function(..., sep_ = "\t", colnames = TRUE, wRownames = TRUE, coltypes = NULL, NaReplace = TRUE) {
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = , sep_, row.names = 1, header = TRUE )
  read_in = suppressWarnings(readr::read_tsv( pfn, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
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
#' @title convert.tsv.data
#' @description Fix NA issue in dataframes imported by the new read.simple.tsv.
#' Set na_rep to NA if you want to keep NA-s
#' @param df_by_read.simple.tsv Data frame (e.g. by read.simple.tsv).
#' @param digitz Number of digits when rounding up, Default: 2
#' @param na_rep Replace NA?, Default: 0
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gtools]{na.replace}}
#' @export
#' @importFrom gtools na.replace
convert.tsv.data <- function(df_by_read.simple.tsv, digitz = 2, na_rep = 0 ) {
  DAT = data.matrix(df_by_read.simple.tsv)
  SNA = sum(is.na(DAT))
  try(iprint("Replaced NA values:", SNA, "or", percentage_formatter(SNA/length(DAT))), silent = TRUE)
  gtools::na.replace(round(DAT, digits = digitz), replace = na_rep)
}




# _________________________________________________________________________________________________
#' @title read.simple.xls
#' @description Read multi-sheet excel files. row_namePos = NULL for automatic
#' names Look into: http://readxl.tidyverse.org/.
#' @param pfn Path and File name, Default: kollapse(...)
#' @param row_namePos Where is the rowname, Default: NULL
#' @param ... Multiple simple variables to parse.
#' @param header_ Is there header? Default: TRUE
#' @param WhichSheets Which sheets to read in
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # read.simple.xls("path/to/my.file")
#'  }
#' }
#' @seealso
#'  \code{\link[gdata]{read.xls}}
#' @export
#' @importFrom gdata read.xls sheetNames
read.simple.xls <- function(pfn = kollapse(...), row_namePos = NULL, ..., header_ = TRUE, WhichSheets) {
  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  if (grepl("^~/", pfn)) {
    iprint("You cannot use the ~/ in the file path! It is replaced by '/Users/abel.vertesy/'.")
    pfn = gsub(pattern = "^~/", replacement = "/Users/abel.vertesy/", x = pfn)
  } else {print(pfn)}

  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  # merge path and filename
  TheSheetNames = gdata::sheetNames(pfn, verbose = FALSE);
  NrSheets = length(TheSheetNames)
  iprint(NrSheets, "sheets in the file.")
  ExpData = list.fromNames(TheSheetNames)
  RangeOfSheets = if (missing(WhichSheets)) 1:NrSheets else WhichSheets
  for (i in RangeOfSheets ) {
    iprint("sheet", i)
    ExpData[[i]] = gdata::read.xls(pfn, sheet = i, row.names = row_namePos, header = header_)
  } #for
  lapply(ExpData, function(x) print(dimnames(x)) )
  return(ExpData);
}



### Writing files out -------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------
#' @title write.simple
#' @description Write out a matrix-like R-object to a file with as tab separated
#'   values (.tsv). Your output filename will be either the variable's name. The
#'   output file will be located in "OutDir" specified by you at the beginning
#'   of the script, or under your current working directory. You can pass the
#'   PATH and VARIABLE separately (in order), they will be concatenated to the
#'   filename.
#' @param input_df Data frame to write out.
#' @param extension File extension to add, Default: 'tsv'
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
write.simple <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ...  ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP = ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
  iprint("Length: ", length(input_df))
} # fun


# _________________________________________________________________________________________________
#' @title write.simple.vec
#' @description Write out a vector-like R-object to a file with as newline
#'   separated values (.vec). Your output filename will be either the variable's
#'   name. The output file will be located in "OutDir" specified by you at the
#'   beginning of the script, or under your current working directory. You can
#'   pass the PATH and VARIABLE separately (in order), they will be concatenated
#'   to the filename.
#' @param input_vec Vector to write out.
#' @param extension File extension to add, Default: 'vec'
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
write.simple.vec <- function(input_vec, extension = 'vec', ManualName = "", o = FALSE, ... ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
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
#' @param extension e.g.: tsv
#' @param ManualName Specify full filename if you do not want to name it by the variable name.
#' @param o Open the file after saving? FALSE by default
#' @param gzip Compress the file after saving? FALSE by default
#' @param separator Field separator, such as "," for csv
#' @param ... Pass any other argument to the kollapse() function used for file name.
#' @export
#' @examples YourDataFrameWithRowAndColumnNames = cbind("A" = rnorm(100), "B" = rpois(100, 8))
#' rownames(YourDataFrameWithRowAndColumnNames) = letters[1:NROW(YourDataFrameWithRowAndColumnNames)]
#' write.simple.tsv(YourDataFrameWithRowAndColumnNames)

write.simple.tsv <- function(input_df, separator = "\t", extension = 'tsv', ManualName = "", o = FALSE,
                             gzip = FALSE, ...  ) {
  if (separator %in% c(',', ';')) extension <- 'csv'
  fname = kollapse (..., print = FALSE); if (nchar (fname) < 2 ) { fname = substitute(input_df) }

  if (nchar(ManualName)) {FnP = kollapse(ManualName)
  } else { FnP = ww.FnP_parser (fname, extension) }
  utils::write.table (input_df, file = FnP, sep = separator, row.names = TRUE,
                      col.names = NA, quote = FALSE  )
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
#' @title write.simple.xlsx
#' @description Write out a list of matrices/ data frames WITH ROW- AND COLUMN-
#'   NAMES to a file with as an Excel (.xslx) file. Your output filename will be
#'   either the variable's name. The output file will be located in "OutDir"
#'   specified by you at the beginning of the script, or under your current
#'   working directory. You can pass the PATH and VARIABLE separately (in
#'   order), they will be concatenated to the filename.
#' @param named_list A list of data frames to write out
#' @param ManualName A string for a manually defined filename. Default: ''
#' @param o Set to TRUE to open file after writing out using 'system(open ...)' on OS X., Default: FALSE
#' @param ... Multiple simple variables to parse.
#' @param TabColor Tab Color in Excel, Default: 'darkgoldenrod1'
#' @param Creator Creator, Default: 'Vertesy'
#' @param HeaderCex Header color, Default: 12
#' @param HeaderLineColor Header line color, Default: 'darkolivegreen3'
#' @param HeaderCharStyle Header character style, Default: c("bold", "italic", "underline")[1]
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # write.simple.xlsx(my.list.of.data.frames)
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{write.xlsx}}
#' @export
#' @importFrom openxlsx write.xlsx
write.simple.xlsx <- function(named_list, ManualName = "", o = FALSE,  ..., TabColor = "darkgoldenrod1", Creator = "Vertesy",
                              HeaderCex = 12, HeaderLineColor = "darkolivegreen3", HeaderCharStyle = c("bold", "italic", "underline")[1]  ) {
  Stringendo::irequire(openxlsx)
  fname = if (nchar(ManualName) < 2 ) { fname = substitute(named_list) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, "xlsx") }

  hs <- createStyle(textDecoration = HeaderCharStyle, fontSize = HeaderCex, fgFill = HeaderLineColor)
  setwd(OutDir)
  openxlsx::write.xlsx(named_list, file = ppp(fname,"xlsx"), rowNames = TRUE, firstRow = TRUE, firstCol = TRUE, colWidths = "auto"
                       , headerStyle = hs, tabColour = TabColor, creator = Creator) #

  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun



# _________________________________________________________________________________________________
#' @title write.simple.append
#' @description Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of
#'  the same number of columns. Your output filename will be either the
#'  variable's name. The output file will be located in "OutDir" specified by
#'  you at the beginning of the script, or under your current working directory.
#'  You can pass the PATH and VARIABLE separately (in order), they will be
#'  concatenated to the filename.
#' @param input_df Data frame to write out.
#' @param extension File extension to add, Default: 'tsv'
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
write.simple.append <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ... ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_df) }
  if (nchar(ManualName)) { FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE  )
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun
