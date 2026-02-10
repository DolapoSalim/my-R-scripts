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

setwd("C:/Users/dolap/OneDrive/Documents/DOLAPO/data-analysis/bio_r_course/bio-R/Field_Challenge_easy")
cat("here is the directory:", getwd())

biomass_wide <- read.csv("biomass_wide.csv", header = TRUE)
site_meta <- read.csv("site_meta.csv", header = TRUE)

?pivot_longer
#Wide -> long: pivot_longer() day_0/day_7/day_14 into columns day (numeric) and biomass


wide <- biomass_wide %>%
  pivot_longer(cols = starts_with("day_"),
               names_to = "day",
               values_to = "biomass") %>%

mutate(day=readr::parse_number(day))

?as.numeric

# Parse IDs: split sample_id into site, treatment, rep using separate().
wide <- wide %>%
  separate(sample_id,
           into = c("site", "treatment", "rep"),
           sep = "_"
           )

?separate()
?left_join


#  Join metadata: add location with left_join by = 'site'

site_meta <- site_meta %>%
  pivot_longer(cols = starts_with("day_"),
               names_to = "day",
               values_to = "biomas") %>%
  mutate(day=readr::parse_number(day))



site_meta_long <- site_meta %>%
  separate(sample_id,
           into = c("site", "treatment", "rep"),
           sep = "_"
  )



wide <- wide %>%
  left_join(site_meta_long, by = "site")
View(wide)

