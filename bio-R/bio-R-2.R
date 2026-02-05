# BioR — 2nd Class setup (run once)

pkgs <- c(
  "tidyverse",   # dplyr, tidyr, ggplot2, readr, purrr, tibble, stringr, forcats
  "janitor",     # clean_names(), tabyl()
  "lubridate",   # dates
  "skimr",       # quick data QA
  "here",         # project-safe file paths
  "readxl",      # Excel import
  "conflicted"   # clearer function conflicts
)


pkgs <- c(pkgs)

# Install missing packages
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]

# conditional statement for installing package found to be false in to_install
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}


# Load packages 
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(here)
library(readxl)
library(conflicted)
