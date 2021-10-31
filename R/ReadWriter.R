######################################################################
# ReadWriter.R
######################################################################
source('~/GitHub/Packages/ReadWriter/R/ReadWriter.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T)

## File handling, export, import [read & write] -------------------------------------------------------------------------------------------------

### Aux -------------------------------------------------------------------------------------------------

jjpegA4 <- function(filename, r = 225, q = 90) { # Setup an A4 size jpeg
  jpeg(file = filename,width = wA4, height = hA4, units = 'in', quality = q,res = r)
}

extPDF <- function(vec) ppp(vec, "pdf") # add pdf as extension to a file name

extPNG <- function(vec) ppp(vec, "png") # add png as extension to a file name


FirstCol2RowNames <- function(Tibble, rownamecol = 1, make_names = FALSE) { # Set First Col to Row Names
  Tibble = as.data.frame(Tibble)
  NN = Tibble[[rownamecol]]
  rownames(Tibble) = if (make_names) make.names(NN, unique = TRUE) else NN
  return(Tibble[, -rownamecol, drop = F])
}


sourcePartial <- function(fn,startTag = '#1', endTag = '#/1') { # Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file
  lines <- scan(fn, what = character(), sep = "\n", quiet = TRUE)
  st <- grep(startTag,lines)
  en <- grep(endTag,lines)
  tc <- textConnection(lines[(st + 1):(en - 1)])
  source(tc)
  close(tc)
}


### Reading files in -------------------------------------------------------------------------------------------------
read.simple.vec <- function(...) { # Read each line of a file to an element of a vector (read in new-line separated values, no header!).
  pfn = kollapse(...) # merge path and filename
  read_in = as.vector(unlist(read.table( pfn , stringsAsFactors = FALSE, sep = "\n" )) )
  iprint(length(read_in), "elements")
  return(read_in);
}

read.simple <- function(...) { # It is essentially read.table() with file/path parsing.
  pfn = kollapse(...) # merge path and filename
  read_in = read.table( pfn , stringsAsFactors = FALSE)
  return(read_in)
}

read.simple_char_list <- function(...) { # Read in a file.
  pfn = kollapse(...) # merge path and filename
  read_in = unlist(read.table( pfn , stringsAsFactors = FALSE ) )
  iprint("New variable head: ", what(read_in))
  return(read_in)
}

read.simple.table <- function(..., colnames = TRUE, coltypes = NULL) { # Read in a file. default: header defines colnames, no rownames. For rownames give the col nr. with rownames, eg. 1 The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  # read_in = read.table( pfn , stringsAsFactors = FALSE, sep = "\t", header = colnames )
  read_in = readr::read_tsv( pfn, col_names = colnames, col_types = coltypes )
  iprint("New variable dim: ", dim(read_in))
  read_in = as.data.frame(gtools::na.replace(data.matrix(read_in), replace = 0))
  return(read_in)
}


read.simple.tsv <- function(..., sep_ = "\t", colnames = TRUE, wRownames = TRUE, coltypes = NULL, NaReplace = TRUE) { # Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = , sep_, row.names = 1, header = TRUE )
  read_in = suppressWarnings(readr::read_tsv( pfn, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}


read.simple.csv <- function(...,  colnames = TRUE, coltypes = NULL, wRownames = TRUE, NaReplace = TRUE, nmax = Inf) { # Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_csv( pfn, col_names = colnames, col_types = coltypes, n_max = nmax ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}

read.simple.ssv <- function(..., sep_ = " ", colnames = TRUE, wRownames = TRUE, NaReplace = TRUE, coltypes = NULL) { # Space separeted values. Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_delim( pfn, delim = sep_, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}


read.simple.tsv.named.vector <- function(...) { # Read in a file with excel style named vectors, names in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = sep_, row.names = 1, header = TRUE )
  read_in = readr::read_tsv( pfn )
  vect = read_in[[2]]
  names(vect) = read_in[[1]]
  iprint("New vectors length is: ", length(vect))
  return(vect)
}

convert.tsv.data <- function(df_by_read.simple.tsv = x, digitz = 2, na_rep = 0 ) { # Fix NA issue in dataframes imported by the new read.simple.tsv. Set na_rep to NA if you want to keep NA-s
  DAT = data.matrix(df_by_read.simple.tsv)
  SNA = sum(is.na(DAT))
  try(iprint("Replaced NA values:", SNA, "or", percentage_formatter(SNA/length(DAT))), silent = TRUE)
  gtools::na.replace(round(DAT, digits = digitz), replace = na_rep)
}



read.simple.xls <- function(pfn = kollapse(...), row_namePos = NULL, ..., header_ = TRUE, WhichSheets) { # Read multi-sheet excel files. row_namePos = NULL for automatic names Look into: http://readxl.tidyverse.org/.
  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  if (grepl("^~/", pfn)) {
    iprint("You cannot use the ~/ in the file path! It is replaced by '/Users/abel.vertesy/'.")
    pfn = gsub(pattern = "^~/", replacement = "/Users/abel.vertesy/", x = pfn)
  } else {print(pfn)}

  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  # merge path and filename
  TheSheetNames = sheetNames(pfn, verbose = FALSE);
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

write.simple <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ...  ) { # Write out a matrix-like R-object to a file with as tab separated values (.tsv). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP = ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
  iprint("Length: ", length(input_df))
} # fun

write.simple.vec <- function(input_vec, extension = 'vec', ManualName = "", o = FALSE, ... ) { # Write out a vector-like R-object to a file with as newline separated values (.vec). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_vec, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE  )
  iprint("Length: ", length(input_vec))
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun

write.simple.xlsx <- function(named_list, ManualName = "", o = FALSE,  ..., TabColor = "darkgoldenrod1", Creator = "Vertesy",# Write out a list of matrices/ data frames WITH ROW- AND COLUMN- NAMES to a file with as an Excel (.xslx) file. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
                              HeaderCex = 12, HeaderLineColor = "darkolivegreen3", HeaderCharStyle = c("bold", "italic", "underline")[1]  ) {
  irequire(openxlsx)
  fname = if (nchar(ManualName) < 2 ) { fname = substitute(named_list) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, "xlsx") }

  hs <- createStyle(textDecoration = HeaderCharStyle, fontSize = HeaderCex, fgFill = HeaderLineColor)
  setwd(OutDir)
  openxlsx::write.xlsx(named_list, file = ppp(fname,"xlsx"), rowNames = TRUE, firstRow = TRUE, firstCol = TRUE, colWidths = "auto"
                       , headerStyle = hs, tabColour = TabColor, creator = Creator) #

  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun


write.simple.append <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ... ) { # Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of the same number of columns. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_df) }
  if (nchar(ManualName)) { FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE  )
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun

