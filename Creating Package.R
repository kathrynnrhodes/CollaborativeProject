#loading necessary packages
library(devtools)
library(roxygen2)
library(withr)
library(tidyverse)
library(usethis)

has_devel()
#spit back you rsystem is ready to build packages!
usethis::create_package("~/Development/Repos/AnalyzeR")

install.packages("/Users/meganhall/Development/Repos/AnalyzeR_0.0.0.9000.tar.gz", repos = NULL, type = "source")
library(AnalyzeR)
