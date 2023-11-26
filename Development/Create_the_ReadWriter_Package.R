######################################################################################################
# Create_the_ReadWriter_Package.R
######################################################################################################
# source("~/GitHub/Packages/ReadWriter/Development/Create_the_ReadWriter_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)

# Functions ------------------------
# require("devtools")


# Setup ------------------------
package.name <- 	"ReadWriter"
package.version <- "1.5.2"
setwd("~/GitHub/Packages/")

RepositoryDir <- paste0("~/GitHub/Packages/", package.name, "/")
fname <-	paste0(package.name, ".R")
package.FnP <-		paste0(RepositoryDir, "R/", fname)

BackupDir <- "~/GitHub/Packages/ReadWriter/Development/"
dir.create(BackupDir)

DESCRIPTION <- list("Title" = "ReadWriter "
    , "Author" = person(given = "Abel", family = "Vertesy", email = "av@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "av@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "ReadWriter is a set of R functions to read and write files conveniently. Complements CodeAndRoll2."
    , "License" = "GPL-3 + file LICENSE"
    , "Version" = package.version
    , "Packaged" =  Sys.time()
    , "Depends" =  "Stringendo (>= 0.5.0)"
    , "Remotes" =  "github::vertesy/Stringendo" # https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html
    # https://stackoverflow.com/questions/72908510/r-package-how-to-specify-a-dependency-version-that-is-only-available-on-github
    , "Imports" = "openxlsx, gtools, readr, utils"
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
install(RepositoryDir, upgrade = F)

# Test if you can install from github ------------------------------------------------
pak::pkg_install("vertesy/ReadWriter")
# unload("CodeAndRoll2")
# require("CodeAndRoll2")
# # remove.packages("CodeAndRoll2")

# dev branch
# "devtools::install_github('vertesy/ReadWriter@read_excel', upgrade = F)"


# Check CRAN ------------------------------------------------
check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("CodeAndRoll2", "foo")
# # desc$get(CodeAndRoll2)
# system("cd ~/GitHub/CodeAndRoll2/; ls -a; open .Rbuildignore")


# Check package dependencies ------------------------------------------------
{
  depFile = paste0(RepositoryDir, 'Development/Dependencies.R')

  (f.deps <- NCmisc::list.functions.in.file(filename = package.FnP))
  # clipr::write_clip(f.deps)

  sink(file = depFile); print(f.deps); sink()
  p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
  write(x = p.deps, file = depFile, append = T)
  p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
  (p.dep.new <- sort(union( p.deps, p.dep.declared)))
  # clipr::write_clip(p.dep.new)
}

# Package styling, and visualization ------------------------------------------------
{
  styler::style_pkg(RepositoryDir)
  # styler::style_file("~/GitHub/Packages/CodeAndRoll2/Development/02.Compile.the.CodeAndRoll2.package.R")

  {
    # Exploring the Structure and Dependencies of my R Package:
    "works on an installed package!"
    pkgnet_result <- pkgnet::CreatePackageReport(package.name)
    fun_graph <- pkgnet_result$FunctionReporter$pkg_graph$"igraph"

    # devtools::load_all('~/GitHub/Packages/PackageTools/R/DependencyTools.R')
    convert_igraph_to_mermaid(graph = fun_graph, openMermaid = T, copy_to_clipboard = T)
  }

  if (F) {
    # Add @importFrom statements
    (FNP <- package.FnP)
    PackageTools::add_importFrom_statements(FNP, exclude_packages = "")
    add_importFrom_statements(FNP, exclude_packages = "")
  }
}


if (F) {
  "check dependency on gdata package"
  require('gdata'); (fs.gdata <- ls("package:gdata"))
  intersect(f.deps[[1]], fs.gdata)

  require('Stringendo'); (fs.Stringendo <- ls("package:Stringendo"))
  intersect(f.deps[[1]], fs.Stringendo)
  setdiff(f.deps[[1]], c(fs.Stringendo, fs.gdata))

}
