# ReadWriter ![status: active](https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/active.svg)
Functions to read and write table or text files conveniently. 
Complements the new [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2). 


## News

- The underlying `gdata` removed `read.xls`, and this is resolved in `v1.0.0` by using `openxlsx`.
- As of 11/2023 you may need `install_github(repo = "vertesy/ReadWriter@main")` instead of `install_github(repo = "vertesy/ReadWriter")` to install the package on some platforms.

<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

```R
# install.packages("devtools"); # If you don't have it.
require("devtools")
devtools::install_github(repo = "vertesy/Stringendo", ref = "main", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter", ref = "main")

"As of 11/2023 you may need:"
devtools::install_github(repo = "vertesy/ReadWriter@main", ref = "main")
```

...then simply load the package:

```R
require("ReadWriter")
```

Alternatively, you simply source it from the web. 
*This way function help will not work, and you will have no local copy of the code on your hard drive.*

```r
source("https://raw.githubusercontent.com/vertesy/ReadWriter/main/R/ReadWriter.R")
```

<br><br>

### Troubleshooting

*If you encounter a **bug**, something doesn't work or unclear, please let me know by raising an issue on [ReadWriter](https://github.com/vertesy/ReadWriter/issues) – Please check if it has been asked.*

## List of Functions in ReadWriter.R (20) 
Updated: 2024/10/24 13:48

- #### 1 `column.2.row.names()`
Convert a Column to Row Names in a Tibble or DataFrame. Converts the first column (or a specified column) of a dataframe or tibble into row names.  This function differs from `tibble::column_to_rownames` in that it takes column names or inices and  it offers the option to sanitize row names using `make.names`, provides a warning if there are  duplicated values in the row name column 

- #### 2 `FirstCol2RowNames()`
FirstCol2RowNames. Set First Col to Row Names

- #### 3 `FirstCol2RowNames.as.df()`
FirstCol2RowNames.as.df. Set First Col to Row Names

- #### 4 `construct.file.path()`
Construct File Path. Constructs a complete file path using either provided manual file name and directory    or defaults to processing a given filename and using the current working directory. 

- #### 5 `read.simple.vec()`
read.simple.vec. Read each line of a file to an element of a vector (read in new-line separated values, no header!).

- #### 6 `read.simple()`
read.simple. It is essentially read.table() with file/path parsing.

- #### 7 `read.simple_char_list()`
read.simple_char_list. Read in a file.

- #### 8 `read.simple.table()`
read.simple.table. Read in a file. default: header defines colnames, no rownames.  For rownames give the col nr. with rownames, eg. 1 The header should start  with a TAB / First column name should be empty.

- #### 9 `read.simple.tsv()`
read.simple.tsv. Read in a file with excel style data: rownames in col1,  headers SHIFTED. The header should start with a TAB / First column name  should be empty.

- #### 10 `read.simple.csv()`
read.simple.csv. Read in a file with excel style data: rownames in col1,  headers SHIFTED. The header should start with a TAB / First column name  should be empty.

- #### 11 `read.simple.csv.named.vector()`
read.simple.csv.named.vector. Read in a data frame (csv), and extact a value and a name column, and convert them  to a named vector. By default, it assumes the names in the first column and the values  excel style named vectors, names in col1,  headers SHIFTED. The header should start with a TAB / First column name  should be empty.

- #### 12 `read.simple.ssv()`
read.simple.ssv. Space separeted values. Read in a file with excel style data:  rownames in col1, headers SHIFTED. The header should start with a  TAB / First column name should be empty.

- #### 13 `read.simple.tsv.named.vector()`
read.simple.tsv.named.vector. Read in a file with excel style named vectors, names in col1,  headers SHIFTED. The header should start with a TAB / First column name  should be empty.

- #### 14 `read.simple.xlsx()`
Read a multi-sheet XLSX easily. Reads specified sheets from an XLSX file into a list of data frames.               It allows customization of column names, row names, and trimming of white spaces. 

- #### 15 `write.simplest()`
Append or write a vector to standard file, one element per line.. Alternative to clipboard. This function takes a vector and appends it  to a specified file. 

- #### 16 `write.simple()`
Write Simple. Writes a matrix-like R object (e.g., a data frame) to a file as tab-separated    values (.tsv). The output filename can be auto-generated from the variable's name or manually    specified. The file is saved in the specified output directory or the current working directory.    The path and variable name can be passed separately and will be concatenated to form the filename.

- #### 17 `write.simple.vec()`
Write Simple Vector. Writes a vector-like R object to a file as newline separated values (.vec).    The output filename can be auto-generated from the variable's name or manually specified. The file    is saved in the specified output directory or the current working directory. The path and variable    name can be passed separately and will be concatenated to form the filename.

- #### 18 `write.simple.tsv()`
write.simple.tsv. Write out a matrix-like R-object WITH ROW- AND COLUMN- NAMES to a file with as tab separated  values (.tsv). Your output filename will be either the variable's name. The output file will be  located in "OutDir" specified by you at the beginning of the script, or under your current  working directory. You can pass the PATH and VARIABLE separately (in order), they will be  concatenated to the filename. If col.names = NA and row.names = TRUE a blank column name is added,  which is the convention used for CSV files to be read by spreadsheets.

- #### 19 `write.simple.append()`
Write Simple Append. Appends a data frame without row names to an existing .tsv file with the same number    of columns. The output filename is auto-generated from the variable's name or manually specified.    The file is saved in the specified output directory or the current working directory. The path and    variable name can be passed separately and will be concatenated to form the filename.

- #### 20 `    assignRownames()`
Write Simple XLSX. Write out a list of matrices or data frames with row and column names    to an Excel (.xlsx) file. The output filename is generated based on the provided parameters    and stored in the specified output directory or the current working directory.    The function offers various styling and formatting options for the Excel file. 

