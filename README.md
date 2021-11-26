# ReadWriter
Functions to read and write files conveniently. 
Complements the new [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2). Many functionalities were part of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll).



<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

```R
# install.packages("devtools"); # If you don't have it.
require("devtools")
devtools::install_github(repo = "vertesy/Stringendo", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter")
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



## List of functions

- #### FirstCol2RowNames 
Set First Col to Row Names

- #### read.simple.vec 
Read each line of a file to an element of a vector (read in new-line separated values, no header!).

- #### read.simple 
It is essentially read.table() with file/path parsing.

- #### read.simple_char_list 
Read in a file.

- #### read.simple.table 
Read in a file. default: header defines colnames, no rownames. For rownames give the col nr. with rownames, eg. 1 The header should start with a TAB / First column name should be empty.

- #### read.simple.tsv 
Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

- #### read.simple.csv 
Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

- #### read.simple.ssv 
Space separeted values. Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

- #### read.simple.tsv.named.vector 
Read in a file with excel style named vectors, names in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

- #### convert.tsv.data 
Fix NA issue in dataframes imported by the new read.simple.tsv. Set na_rep to NA if you want to keep NA-s

- #### read.simple.xls 
Read multi-sheet excel files. row_namePos = NULL for automatic names Look into: http://readxl.tidyverse.org/.

- #### write.simple 
Write out a matrix-like R-object to a file with as tab separated values (.tsv). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

- #### write.simple.vec 
Write out a vector-like R-object to a file with as newline separated values (.vec). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

- #### write.simple.xlsx 
Write out a list of matrices/ data frames WITH ROW- AND COLUMN- NAMES to a file with as an Excel (.xslx) file. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

- #### write.simple.append 
Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of the same number of columns. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
