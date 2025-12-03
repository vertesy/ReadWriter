# Configuration for the Package
# file.edit("~/GitHub/Packages/ReadWriter/Development/config.R")

DESCRIPTION <- list(
  package.name = "ReadWriter",
  version = "1.6.6",
  title = "ReadWriter",
  description = "ReadWriter is a set of R functions to read and write files conveniently. Complements CodeAndRoll2.",

  author.given = "Abel",
  author.family = "Vertesy",
  author.email = "av@imba.oeaw.ac.at",
  github.user = "vertesy",
  license = "GPL-3 + file LICENSE",
  depends = "Stringendo (>= 0.5.0)",
  remotes =  "github::vertesy/Stringendo",
  imports = "qs, openxlsx, gtools, readr",
  suggests = ""
)

