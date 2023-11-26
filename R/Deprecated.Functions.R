


# _________________________________________________________________________________________________




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
# #' @importFrom gdata read.xls sheetNames
read.simple.xls <- function(pfn = kollapse(...), row_namePos = NULL, ..., header_ = TRUE, WhichSheets) {

  .Deprecated("read.simple.xlsx")

  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  if (grepl("^~/", pfn)) {
    iprint("You cannot use the ~/ in the file path! It is replaced by '~/'.")
    pfn = gsub(pattern = "^~/", replacement = "~/", x = pfn)
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


#'
#' # _________________________________________________________________________________________________
#' #' @title write.simple.xlsx.old
#' #' @description Write out a list of matrices/ data frames WITH ROW- AND COLUMN-
#' #'   NAMES to a file with as an Excel (.xslx) file. Your output filename will be
#' #'   either the variable's name. The output file will be located in "OutDir"
#' #'   specified by you at the beginning of the script, or under your current
#' #'   working directory. You can pass the PATH and VARIABLE separately (in
#' #'   order), they will be concatenated to the filename.
#' #' @param named_list A list of data frames to write out
#' #' @param suffix A suffix added to the filename, Default: NULL
#' #' @param fname A string for a manually defined filename. Default: substitute(named_list)
#' #' @param o Set to TRUE to open file after writing out using 'system(open ...)' on OS X., Default: FALSE
#' #' @param TabColor Tab Color in Excel, Default: 'darkgoldenrod1'
#' #' @param Creator Creator, Default: ''
#' #' @param HeaderCex Header color, Default: 12
#' #' @param HeaderLineColor Header line color, Default: 'darkolivegreen3'
#' #' @param HeaderCharStyle Header character style, Default: c("bold", "italic", "underline")[1]
#' #' @param row_names Have rownames? Default: TRUE
#' #' @param ... Multiple simple variables to parse.
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  # write.simple.xlsx(my.list.of.data.frames)
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[openxlsx]{write.xlsx}}
#' #' @export
#' #' @importFrom openxlsx write.xlsx createStyle
#'
#' write.simple.xlsx.old <- function(named_list
#'                                   , filename = substitute(named_list)
#'                                   , suffix = NULL
#'                                   , o = FALSE
#'                                   , TabColor = "darkgoldenrod1", HeaderLineColor = "darkolivegreen3"
#'                                   , HeaderCex = 12, Creator = ""
#'                                   , HeaderCharStyle = c("bold", "italic", "underline")[1]
#'                                   , row_names = TRUE, ...) {
#'
#'   warning("Switched using from openxlsx to readxl package 2023.11.22")
#'
#'   fname <- Stringendo::sppp(filename, suffix)
#'   if ( !('list' %in% class(named_list))  ) named_list <- list(named_list) # convert to a list if needed
#'
#'   if (nchar(fname) > 100) fname <- kpp('_Output', idate())
#'   FnP <- kpp(kpps(getwd(), fname), "xlsx")
#'
#'   hs <- openxlsx::createStyle(textDecoration = HeaderCharStyle, fontSize = HeaderCex
#'                               , fgFill = HeaderLineColor)
#'
#'   if (row_names) {
#'     FUNX <- function(x)  rownames_to_column(as.data.frame(x), var = "genes")
#'     named_list <- lapply(named_list, FUNX)
#'     # named_list <- rownames_to_column(as.data.frame(named_list), var = "genes")
#'   }
#'   print(named_list)
#'   print(rownames(named_list))
#'
#'   openxlsx::write.xlsx(x = named_list, file = FnP, rowNames = FALSE
#'                        , firstRow = TRUE
#'                        , firstCol = TRUE
#'                        , colWidths = "auto"
#'                        , headerStyle = hs, tabColour = TabColor, creator = Creator)
#'   if (o) { system(paste0("open ", fix_special_characters_bash(FnP)), wait = FALSE) }
#' } # fun
#'



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


