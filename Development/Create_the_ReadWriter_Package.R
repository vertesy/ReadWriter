######################################################################################################
# Create_the_ReadWriter_Package.R
######################################################################################################
# source("/Users/abel.vertesy/GitHub/Packages/ReadWriter/Development/Create_the_ReadWriter_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)

# Functions ------------------------
# install_version("devtools", version = "2.0.2", repos = "http://cran.at.r-project.org") # install.packages("devtools")
# require("devtools")
# require("roxygen2")
# require("stringr")

# # devtools::install_github(repo = "vertesy/CodeAndRoll2")
# require('CodeAndRoll2')
# require('Stringendo')


# Setup ------------------------
package.name <- 	"ReadWriter"
package.version <- "0.3.3"
setwd("~/GitHub/Packages/")

RepositoryDir <- paste0("~/GitHub/Packages/", package.name, "/")
fname <-	paste0(package.name, ".R")
package.FnP <-		paste0(RepositoryDir, "R/", fname)

BackupDir <- "~/GitHub/Packages/ReadWriter/Development/"
dir.create(BackupDir)

DESCRIPTION <- list("Title" = "ReadWriter "
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "a.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "ReadWriter is a set of R functions to read and write files conveniently. Complements CodeAndRoll2."
    , "License" = "GPL-3 + file LICENSE"
    , "Version" = package.version
    , "Packaged" =  Sys.time()
    # , "Repository" =  "CRAN"
    , "Depends" =  "Stringendo"
    , "Imports" = "base, gtools, gdata, openxlsx, readr,  utils" # CodeAndRoll2,
    # , "Suggests" = ""
    , "BugReports"= "https://github.com/vertesy/ReadWriter/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
    getwd()
    try(file.remove(c("DESCRIPTION","NAMESPACE", "ReadWriter.Rproj")))
    usethis::create_package(path = RepositoryDir, fields = DESCRIPTION, open = F)
}


# go and write fun's ------------------------------------------------------------------------
# file.edit(package.FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(package.FnP)

# replace output files ------------------------------------------------
BackupOldFile <-	(paste0(BackupDir, "Development", ".bac"))
AnnotatedFile <-	(paste0(BackupDir, "Development", ".annot.R"))
file.copy(from = package.FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = package.FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(package.FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
devtools::document()
warnings()

{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T
                  , "^version: v.+", paste0("version: v", package.version))
}

# Install your package ------------------------------------------------
# # setwd(RepositoryDir)
devtools::install(RepositoryDir, upgrade = F)

# require("ReadWriter")
# # remove.packages("ReadWriter")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()


# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/ReadWriter")

# require("ReadWriter")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("ReadWriter")

check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
#
#
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("ReadWriter", "foo")
# # desc$get(ReadWriter)
#
#
# system("cd ~/GitHub/ReadWriter/; ls -a; open .Rbuildignore")
#
# Check package dependencies ------------------------------------------------
depFile = paste0(RepositoryDir, 'Development/Dependencies.R')

(f.deps <- NCmisc::list.functions.in.file(filename = package.FnP))
# clipr::write_clip(f.deps)

sink(file = depFile); print(f.deps); sink()
p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
write(x = p.deps, file = depFile, append = T)
p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
(p.dep.new <- sort(union( p.deps, p.dep.declared)))
# clipr::write_clip(p.dep.new)


if (F) {
  "check dependency on gdata package"
  require('gdata'); (fs.gdata <- ls("package:gdata"))
  intersect(f.deps[[1]], fs.gdata)

  require('Stringendo'); (fs.Stringendo <- ls("package:Stringendo"))
  intersect(f.deps[[1]], fs.Stringendo)
  setdiff(f.deps[[1]], c(fs.Stringendo, fs.gdata))

}
