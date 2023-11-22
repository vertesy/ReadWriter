


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
# #' @importFrom gdata read.xls sheetNames
read.simple.xls <- function(pfn = kollapse(...), row_namePos = NULL, ..., header_ = TRUE, WhichSheets) {

  .Deprecated("read.simple.xlsx")

  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  if (grepl("^~/", pfn)) {
    iprint("You cannot use the ~/ in the file path! It is replaced by '/Users/abel.vertesy/'.")
    pfn = gsub(pattern = "^~/", replacement = "/Users/abel.vertesy/", x = pfn)
  } else {print(pfn)}

  # merge path and filename
  TheSheetNames = gdata::sheetNames(pfn, verbose = FALSE);
  NrSheets = length(TheSheetNames)
  iprint(NrSheets, "sheets in the file.")
  # ExpData = CodeAndRoll2::list.fromNames(TheSheetNames)
  ExpData = as.list(TheSheetNames)
  names(ExpData) = TheSheetNames

  RangeOfSheets = if (missing(WhichSheets)) 1:NrSheets else WhichSheets
  for (i in RangeOfSheets ) {
    iprint("sheet", i)
    # ExpData[[i]] = gdata::read.xls(pfn, sheet = i, row.names = row_namePos, header = header_)
  } #for
  lapply(ExpData, function(x) print(dimnames(x)) )
  return(ExpData);
}



# _________________________________________________________________________________________________
#' @title convert.tsv.data
#' @description Fix NA issue in dataframes imported by the new read.simple.tsv.
#' Set na_rep to NA if you want to keep NA-s
#' @param df_by_read.simple.tsv Data frame (e.g. by read.simple.tsv).
#' @param digitz Number of digits when rounding up, Default: 2
#' @param na_rep Replace NA?, Default: 0
#' @seealso
#'  \code{\link[gtools]{na.replace}}
#' @importFrom gtools na.replace
convert.tsv.data <- function(df_by_read.simple.tsv, digitz = 2, na_rep = 0 ) {

  .Deprecated(msg = 'This function is phased out.')

  DAT = data.matrix(df_by_read.simple.tsv)
  SNA = sum(is.na(DAT))
  try(iprint("Replaced NA values:", SNA, "or", percentage_formatter(SNA/length(DAT))), silent = TRUE)
  gtools::na.replace(round(DAT, digits = digitz), replace = na_rep)
}



# _________________________________________________________________________________________________


