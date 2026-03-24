getwd()


library(conflicted)
library(tidyverse)
library(ggpubr)
library(readxl)
library(lubridate)
library(broom)
conflicts_prefer(dplyr::filter)



#Setup data directory
data_dir <- file.path(getwd(), "final_exam")
setwd(data_dir)
output_dir <- file.path(data_dir, "outputs")
plot_dir <- file.path(output_dir, "plots")
dir.create(plot_dir, showWarnings = F, recursive = T)


getwd()


# load data and inspect
biomass_wide <- read.csv("biomass_wide.csv")
drone_ndvi <- read.csv("drone_ndvi.csv")
logger_raw <- read.csv("logger_raw.csv")
site_meta <- read.csv("site_meta.csv")
View(biomass_wide)



# HELPER FUNCTIONS

# To fix the column
column_fix <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace("-", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("-+", "_") %>%
    str_replace_all(" +", "_")
}


# Normalize plot IDs: P1 → P01, P2 → P02
normalize_plot_id <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace("^P([0-9])$", "P0\\1")
}

# 'ND' strings in biomass columns; numeric parsing require
parse_column <- function(x) { 
  as.character(x) %>%
  str_trim() %>%
  readr::parse_number()
}

# Remove % from canopy values
parse_pct <- function(x) {
  as.numeric(gsub("%", "", as.character(x)))
}

# Parse dates (handles both YYYY-MM-DD and DD/MM/YYYY)
parse_date_robust <- function(x) {
  parse_date_time(x, orders = c("Y-m-d", "d/m/Y"), tz = "UTC")
}

# Parse timestamps (handles mixed formats with/without seconds)
parse_ts_robust <- function(x) {
  parse_date_time(x, orders = c("Y-m-d H:M:S", "Y-m-d H:M", "d/m/Y H:M"), tz = "UTC")
}



### BIOMASS DATA
biomass_long <- biomass_wide %>%
    mutate(
    # clean columns
    site = column_fix(site),
    treat = column_fix(treat),
    plot = normalize_plot_id(plot),
    rep = column_fix(rep)
  ) %>%
  
  # Wide  to long
  pivot_longer(
    starts_with("day_"),
    names_to = "day",
    values_to = "biomass_old"
  ) %>%
  
  # Parse and create flags
  mutate(
    # Extract numeric day value
    day = parse_number(day),
    # Parse biomass robustly
    biomass = parse_column(biomass_old),
    # QC flag for non-numeric values in biomass_raw
    qc_non_numeric_biomass = !is.na(biomass_old) & is.na(biomass),
    # Reconstruct clean sample_id
    sample_id = paste(site, treat, plot, rep, sep = "_")
  ) %>%
  select(site, treat, plot, rep, day, biomass_old, biomass, 
         qc_non_numeric_biomass, sample_id)

# Check for duplicate biomass records (same site/treat/plot/rep/day)
biomass_duplicate <- biomass_long %>% 
  group_by(site, treat, plot, rep, day) %>%
  filter(n() > 1)

if (nrow(biomass_duplicates) > 0) {
  message("Warning: Found ", nrow(biomass_duplicate), " duplicates")
}

# Remove duplicates (keep first, flag with QC)
biomass_long_duplicate <- biomass_long %>%
  group_by(site, treat, plot, rep, day) %>%
  mutate(qc_biomass_duplicate = n() > 1) %>%
  slice(1) %>%
  ungroup()

# Create clean version with only essential columns
biomass_long_clean <- biomass_long_duplicate %>%
  select(site, treat, plot, rep, day, biomass)


nrow(biomass_long_clean)


## SITE DATA

site_meta_clean <- site_meta %>%
  mutate(
    site = column_fix(site),
    plot = normalize_plot_id(plot),
    canopy_pct = parse_pct(canopy_cover_pct),
    qc_canopy_high = !is.na(canopy_pct) & canopy_pct > 100,
  ) %>%
  
  group_by(site, plot) %>%
    # QC aggregation
    mutate(
      qc_meta_duplicate = n() > 2,
    )


?any
message("Site metadata records: ", nrow(site_meta_clean))

  