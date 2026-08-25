## List of Functions in ReadWriter.R (22) 
Updated: 2026/08/25 17:02
- #### 1 `column.2.row.names()`
Convert a Column to Row Names in a Tibble or DataFrame. Converts the first column (or a specified column) of a data frame or tibble into row names.  This function differs from `tibble::column_to_rownames` in that it takes column names or indices,  offers the option to sanitize row names using `make.names`, and provides a warning if there are  duplicated values in the row name column. 

- #### 2 `FirstCol2RowNames()`
FirstCol2RowNames. Set first column to row names.

- #### 3 `FirstCol2RowNames.as.df()`
FirstCol2RowNames.as.df. Set first column to row names.

- #### 4 `construct.file.path()`
Construct File Path. Constructs a complete file path using either provided manual file name and directory    or defaults to processing a given filename and using the current working directory.    At least one of `filename` or `manual_file_name` must be supplied. 

- #### 5 `read.simple.vec()`
read.simple.vec. Read each line of a file to an element of a vector (read in newline-separated values, no header!).

- #### 6 `read.simple()`
read.simple. Essentially `read.table()` with file/path parsing.

- #### 7 `read.simple_char_list()`
read.simple_char_list. Read in a file.

- #### 8 `read.simple.table()`
read.simple.table. Read a file. Default: header defines column names, no row names.  For row names give the column number with row names, e.g., 1. The header should start  with a TAB; the first column name should be empty.

- #### 9 `read.simple.tsv()`
read.simple.tsv. Read in a file with Excel-style data: row names in column 1,  headers shifted. The header should start with a TAB; the first column name  should be empty.

- #### 10 `read.simple.csv()`
read.simple.csv. Read in a file with Excel-style data: row names in column 1,  headers shifted. The header should start with a TAB; the first column name  should be empty.

- #### 11 `read.simple.csv.named.vector()`
read.simple.csv.named.vector. Read in a data frame (CSV), extract a value and a name column, and convert them  to a named vector. By default, it assumes the names are in the first column and the values in the second.  For Excel-style named vectors, names are in column 1 and headers are shifted.  The header should start with a TAB; the first column name should be empty.

- #### 12 `read.simple.ssv()`
read.simple.ssv. Space separated values. Read in a file with Excel-style data:  row names in column 1, headers shifted. The header should start with a  TAB; the first column name should be empty.

- #### 13 `read.simple.tsv.named.vector()`
read.simple.tsv.named.vector. Read in a file with Excel-style named vectors, names in column 1,  headers shifted. The header should start with a TAB; the first column name  should be empty.

- #### 14 `read.simple.xlsx()`
Read a multi-sheet XLSX easily. Reads specified sheets from an XLSX file into a list of data frames.               It allows customization of column names, row names, and trimming of whitespace. 

- #### 15 `write.simplest()`
Append or write a vector to a standard file, one element per line.. Alternative to the clipboard. This function takes a vector and appends it  to a specified file. 

- #### 16 `write.simple()`
Write Simple. Writes a matrix-like R object (e.g., a data frame) to a file as tab-separated    values (.tsv). The output filename can be auto-generated from the variable's name or manually    specified. The file is saved in the specified output directory or the current working directory.    The path and variable name can be passed separately and will be concatenated to form the filename.

- #### 17 `write.simple.vec()`
Write Simple Vector. Writes a vector-like R object to a file as newline-separated values (.vec).    The output filename can be auto-generated from the variable's name or manually specified. The file    is saved in the specified output directory or the current working directory. The path and variable    name can be passed separately and will be concatenated to form the filename.

- #### 18 `write.simple.tsv()`
write.simple.tsv. Write out a matrix-like R object with row and column names to a file as tab-separated  values (.tsv). The output filename will be either the variable's name or the one you provide. The output  file will be located in the directory specified at the beginning of the script or in your current  working directory. You can pass the path and variable separately (in order); they will be concatenated  to the filename. If `col.names = NA` and `row.names = TRUE`, a blank column name is added,  which is the convention used for CSV files to be read by spreadsheets.  It can also write CSV files if you set the separator to ',' or ';'.

- #### 19 `write.simple.append()`
Write Simple Append. Appends a data frame without row names to an existing .tsv file with the same number    of columns. The output filename is auto-generated from the variable's name or manually specified.    The file is saved in the specified output directory or the current working directory. The path and    variable name can be passed separately and will be concatenated to form the filename.

- #### 20 `    assignRownames()`
Write Simple XLSX. Write out a list of matrices or data frames with row and column names    to an Excel (.xlsx) file. The output filename is generated based on the provided parameters    and stored in the specified output directory or the current working directory.    The function offers various styling and formatting options for the Excel file. 

- #### 21 `  collapse_row()`
as.simple.md.table. Convert a data.frame / matrix-like object to minimal GitHub-flavored Markdown  table lines (header, separator, body). Pure formatter; does not write to disk. 

- #### 22 `write.simple.md.table()`
write.simple.md.table. Write an R data.frame / matrix-like object to disk as a minimal GitHub-flavored  Markdown table (.md). 

