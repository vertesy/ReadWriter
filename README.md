# ReadWriter
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
devtools::install_github(repo = "vertesy/Stringendo", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter")

"As of 11/2023 you may need:"
devtools::install_github(repo = "vertesy/ReadWriter@main")
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

## List of Functions (16) 
Updated: 2023/11/24 16:40

- #### 1 `column.2.row.names()`
Convert a Column to Row Names in a Tibble or DataFrame. Converts the first column (or a specified column) of a dataframe or tibble into row names.  This function differs from `tibble::column_to_rownames` in that it takes column names or inices and  it offers the option to sanitize row names using `make.names`, provides a warning if there are  duplicated values in the row name column 

- #### 2 `FirstCol2RowNames()`
FirstCol2RowNames. Set First Col to Row Names

- #### 3 `FirstCol2RowNames.as.df()`
FirstCol2RowNames.as.df. Set First Col to Row Names

- #### 4 `read.simple.vec()`
read.simple.vec. Read each line of a file to an element of a vector (read in new-line separated values, no header!).

- #### 5 `read.simple()`
read.simple. It is essentially read.table() with file/path parsing.

- #### 6 `read.simple_char_list()`
read.simple_char_list. Read in a file.

- #### 7 `read.simple.table()`
read.simple.table. Read in a file. default: header defines colnames, no rownames.  For rownames give the col nr. with rownames, eg. 1 The header should start  with a TAB / First column name should be empty.

- #### 8 `read.simple.tsv()`
read.simple.tsv. Read in a file with excel style data: rownames in col1,  headers SHIFTED. The header should start with a TAB / First column name  should be empty.

- #### 9 `read.simple.csv()`
read.simple.csv. Read in a file with excel style data: rownames in col1,  headers SHIFTED. The header should start with a TAB / First column name  should be empty.

- #### 10 `read.simple.ssv()`
read.simple.ssv. Space separeted values. Read in a file with excel style data:  rownames in col1, headers SHIFTED. The header should start with a  TAB / First column name should be empty.

- #### 11 `read.simple.tsv.named.vector()`
read.simple.tsv.named.vector. Read in a file with excel style named vectors, names in col1,  headers SHIFTED. The header should start with a TAB / First column name  should be empty.

- #### 12 `read.simple.xlsx()`
Read a multi-sheet XLSX easily. Reads specified sheets from an XLSX file into a list of data frames.               It allows customization of column names, row names, and trimming of white spaces. 

- #### 13 `write.simple()`
write.simple. Write out a matrix-like R-object to a file with as tab separated    values (.tsv). Your output filename will be either the variable's name. The    output file will be located in "OutDir" specified by you at the beginning    of the script, or under your current working directory. You can pass the    PATH and VARIABLE separately (in order), they will be concatenated to the    filename.

- #### 14 `write.simple.vec()`
write.simple.vec. Write out a vector-like R-object to a file with as newline    separated values (.vec). Your output filename will be either the variable's    name. The output file will be located in "OutDir" specified by you at the    beginning of the script, or under your current working directory. You can    pass the PATH and VARIABLE separately (in order), they will be concatenated    to the filename.

- #### 15 `write.simple.tsv()`
write.simple.tsv. Write out a matrix-like R-object WITH ROW- AND COLUMN- NAMES to a file with as tab separated  values (.tsv). Your output filename will be either the variable's name. The output file will be  located in "OutDir" specified by you at the beginning of the script, or under your current  working directory. You can pass the PATH and VARIABLE separately (in order), they will be  concatenated to the filename.

- #### 16 `write.simple.append()`
write.simple.append. Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of   the same number of columns. Your output filename will be either the   variable's name. The output file will be located in "OutDir" specified by   you at the beginning of the script, or under your current working directory.   You can pass the PATH and VARIABLE separately (in order), they will be   concatenated to the filename.



  
