# BioR — SUPER Challenge Solution (Intertidal integration)
# Folder: /mnt/data/BioR_SuperChallenge_Intertidal_20260209_200030

library(tidyverse)
library(readxl)
library(lubridate)
library(broom)

data_dir <- "Lesson_3/Capstone challanges/Hard_intertidal_challange/data"
out_dir  <- file.path(data_dir, "outputs")
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)


# ----------------------------
# Helpers
# ----------------------------
std_id <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("-", "_") %>%
    str_to_upper() %>%
    str_replace_all("_+", "_") %>%
    str_replace("_R0([0-9])$", "_R\1")
}

parse_biomass <- function(x) {
  x2 <- as.character(x) %>% str_trim()
  x2[x2 %in% c("", "ND", "DEAD", "dead", "Dead")] <- NA
  readr::parse_number(x2)
}

parse_ts <- function(x) {
  parse_date_time(x, orders = c("Y-m-d H:M", "d/m/Y H:M", "b d Y H:M"), tz = "Europe/Rome")
}

# ----------------------------
# A) Biomass wide -> long + parse IDs
# ----------------------------
biomass_wide <- readr::read_csv(file.path(data_dir, "biomass_wide.csv"), show_col_types = FALSE) %>%
  mutate(sample_id_raw = sample_id,
         sample_id = std_id(sample_id))

biomass_long <- biomass_wide %>%
  # 1) Replace the literal string "ND" anywhere in the data frame with NA.
  #    Useful when biomass is recorded as "ND" (not detected / not determined).
  #    NOTE: this replaces "ND" in all columns, including sample_id if it ever contained "ND".
  replace(. == "ND", NA) %>%
  
  # 2) Coerce all day_* columns to numeric.
  #    This is needed because Excel/CSV imports often read these columns as character
  #    (especially if they include ND, blanks, or other text).
  #    NOTE: as.numeric("dead") becomes NA silently; that's OK if you flag later.
  mutate(across(starts_with("day_"), as.numeric)) %>%
  
  # 3) Wide -> long: turn day_0, day_7, day_14 into one "day" column + one value column.
  #    After this step, each row is: one sample_id at one day.
  pivot_longer(
    starts_with("day_"),
    names_to  = "day",
    values_to = "biomass_raw"
  ) %>%
  
  # 4) Clean/parse columns:
  mutate(
    # Convert "day_14" -> 14 (numeric), so you can plot/model day as a number.
    day = readr::parse_number(day),
    
    # Convert biomass_raw into a clean numeric biomass.
    # parse_biomass() is your robust helper (e.g., handles "dead", blanks, etc.).
    biomass = parse_biomass(biomass_raw),
    
    # QC flag: TRUE when something was present in biomass_raw but became NA after parsing.
    # This catches cases like "dead", "ND", "", or other non-numeric strings.
    qc_non_numeric_biomass = !is.na(biomass_raw) & is.na(biomass)
  ) %>%
  
  # 5) Split sample_id into metadata columns.
  #    Example: "SITE1_CANOPY_R1" -> site="SITE1", habitat="CANOPY", rep="R1"
  #    remove = FALSE keeps the original sample_id column.
  separate(
    sample_id,
    into = c("site", "habitat", "rep"),
    sep = "_",
    remove = FALSE
  )

# ----------------------------
# B) Quadrat metadata
# ----------------------------

meta_raw <- read_xlsx("Lesson_3/Capstone challanges/Hard_intertidal_challange/data/quadrat_meta.xlsx")
meta_clean <- meta_raw %>%
  mutate(
    # Convert canopy_cover_pct to a clean numeric value.
    # Works even if the Excel column contains strings like "45%" or " 45 % ".
    canopy_cover_pct = readr::parse_number(as.character(canopy_cover_pct)),
    
    # QC flag: canopy cover is a percentage, so values > 100 are impossible.
    # We keep this as a flag rather than “fixing” it silently.
    qc_canopy_impossible = !is.na(canopy_cover_pct) & canopy_cover_pct > 100
  ) %>%
  # If the Excel sheet has duplicated sample_id rows, we want ONE row per sample_id.
  group_by(sample_id) %>%
  summarise(
    # Deduplication strategy:
    # pick the first NON-missing value for each field.
    # - stats::na.omit(x) removes NA values
    # - dplyr::first(...) takes the first remaining value
    # If all values are NA, na.omit() is empty and first() returns NA.
    shore_height_cm  = dplyr::first(stats::na.omit(shore_height_cm)),
    canopy_cover_pct = dplyr::first(stats::na.omit(canopy_cover_pct)),
    substrate        = dplyr::first(stats::na.omit(substrate)),
    collector        = dplyr::first(stats::na.omit(collector)),
    
    # Collapse QC info across duplicates:
    # if ANY row for this sample_id had an impossible canopy value, keep TRUE.
    qc_canopy_impossible = any(qc_canopy_impossible, na.rm = TRUE),
    
    # Flag whether this sample_id appeared more than once in the metadata file.
    qc_meta_duplicate    = n() > 1,
    
    # Ungroup at the end (avoids accidental grouped behaviour downstream).
    .groups = "drop"
  )


# ----------------------------
# C) Microclimate aggregation
# ----------------------------
# ------------------------------------------------------------
# Microclimate pipeline: map loggers -> parse timestamps -> bin to sampling days
# Goal: get mean temperature and mean humidity per site × habitat × day
# ------------------------------------------------------------

logger_map <- readr::read_csv(file.path(data_dir, "logger_map.csv"), show_col_types = FALSE) %>%
  # Clean logger_id so it matches across files:
  # e.g., trim spaces, fix case, replace '-' with '_', etc. (depends on your std_id()).
  mutate(logger_id = std_id(logger_id))

logger_raw <- readr::read_csv(file.path(data_dir, "logger_raw.csv"), show_col_types = FALSE) %>%
  mutate(
    # Standardize logger_id in the raw logger file too (critical for joins).
    logger_id = std_id(logger_id),
    
    # Parse timestamps robustly (multiple formats).
    # parse_ts() should return POSIXct; non-parsable timestamps become NA.
    timestamp = parse_ts(timestamp)
  ) %>%
  # Drop rows where timestamp could not be parsed (otherwise day-binning fails).
  dplyr::filter(!is.na(timestamp))

# Join mapping info (site, habitat, rep) onto every logger record.
# left_join keeps all logger_raw rows even if some logger_id are missing in logger_map (they become NA).
logs <- logger_raw %>% left_join(logger_map, by = "logger_id")

# Define the campaign "day 0" as the earliest timestamp in the logger dataset.
# NOTE: This assumes all loggers were deployed at about the same time.
start_date <- as.Date(min(logs$timestamp, na.rm = TRUE))

logs_day <- logs %>%
  mutate(
    # Convert each timestamp to a day index relative to start_date:
    # day 0 = start_date, day 7 = one week later, etc.
    day = as.integer(as.Date(timestamp) - start_date)
  ) %>%
  # Keep only the sampling days that match your biomass measurements.
  # This aligns high-frequency logger data to discrete field sampling time points.
  dplyr::filter(day %in% c(0, 7, 14))

micro_day <- logs_day %>%
  # Aggregate to the same grain as your biomass dataset (site × habitat × day)
  group_by(site, habitat, day) %>%
  summarise(
    # Mean microclimate on that day (across loggers/records in that group)
    mean_temp = mean(temp_c, na.rm = TRUE),
    mean_rh   = mean(rh_pct, na.rm = TRUE),
    
    # Number of logger records used in the mean (useful QC: low n_log = poor coverage)
    n_log     = n(),
    
    # Remove grouping to avoid surprises later
    .groups = "drop"
  )

#  Note: if logger_id has trailing spaces or different case, the join fails silently and you’ll get NA site/habitat. That’s why std_id() is essential before left_join().
# ----------------------------
# D) Microbiome proxies
# ----------------------------
asv <- readr::read_csv(file.path(data_dir, "asv_table.csv"), show_col_types = FALSE) %>%
  mutate(sample_id = std_id(sample_id))

asv_combined <- asv %>%
  group_by(sample_id, asv_id) %>%
  summarise(reads = sum(reads, na.rm = TRUE), .groups = "drop")

microbiome_proxy <- asv_combined %>%
  group_by(sample_id) %>%
  summarise(
    library_size = sum(reads, na.rm = TRUE),
    richness = n_distinct(asv_id[reads > 0]),
    qc_low_depth = library_size < 1000,
    .groups = "drop"
  )

# ----------------------------
# E) Join all + complete()
# ----------------------------
dat <- biomass_long %>%
  left_join(meta_clean, by = "sample_id") %>%
  left_join(micro_day, by = c("site", "habitat", "day")) %>%
  left_join(microbiome_proxy, by = "sample_id")

dat_complete <- dat %>%
  complete(site, habitat, rep, day = c(0, 7, 14)) %>%
  mutate(missing_biomass_flag = is.na(biomass))

# Join diagnostics example:
# inner_join keeps only rows that match micro_day; left_join keeps all biomass rows.
n_left  <- nrow(biomass_long %>% left_join(micro_day, by = c("site","habitat","day")))
n_inner <- nrow(biomass_long %>% inner_join(micro_day, by = c("site","habitat","day")))
message("Rows after left_join:  ", n_left)
message("Rows after inner_join: ", n_inner)

# QC report per sample_id
qc_report <- dat %>%
  group_by(sample_id) %>%
  summarise(
    site = first(site),
    habitat = first(habitat),
    qc_non_numeric_biomass = any(qc_non_numeric_biomass, na.rm = TRUE),
    qc_canopy_impossible   = any(qc_canopy_impossible, na.rm = TRUE),
    qc_meta_duplicate      = any(qc_meta_duplicate, na.rm = TRUE),
    qc_low_depth           = any(qc_low_depth, na.rm = TRUE),
    qc_missing_microclimate = all(is.na(mean_temp)) & all(is.na(mean_rh)),
    .groups = "drop"
  )

# ----------------------------
# G) Hybrid models per site (biomass ~ mean_temp)
# ----------------------------
models <- dat %>%
  dplyr::filter(!is.na(biomass), !is.na(mean_temp)) %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    fit = purrr::map(data, ~ lm(biomass ~ mean_temp, data = .x)),
    stats = purrr::map(fit, broom::glance),
    slope = purrr::map_dbl(fit, ~ unname(coef(.x)[["mean_temp"]])),
    intercept = purrr::map_dbl(fit, ~ unname(coef(.x)[["(Intercept)"]])),
    r2 = map_dbl(stats, ~ .x$r.squared),
    n = map_int(stats, ~ as.integer(.x$nobs))
  )

site_models <- models %>% select(site, n, intercept, slope, r2)

# ----------------------------
# Plots
# ----------------------------
p1 <- dat %>%
  ggplot(aes(x = day, y = biomass, group = rep)) +
  geom_line(alpha = 0.5, na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_grid(site ~ habitat) +
  labs(x = "Day", y = "Biomass") +
  theme_classic()

# ggsave(file.path(plot_dir, "01_biomass_timeseries.png"), p1, width = 9, height = 6, dpi = 300)

p2 <- dat %>%
  ggplot(aes(x = mean_temp, y = biomass)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  facet_wrap(~ site) +
  labs(x = "Mean temperature (C)", y = "Biomass") +
  theme_classic()
# 
# ggsave(file.path(plot_dir, "02_biomass_vs_temp.png"), p2, width = 9, height = 4.5, dpi = 300)

qc_long <- qc_report %>%
  pivot_longer(starts_with("qc_"), names_to = "flag", values_to = "failed") %>%
  group_by(flag) %>%
  summarise(pct_failed = mean(failed, na.rm = TRUE) * 100, .groups = "drop")

p3 <- ggplot(qc_long, aes(x = flag, y = pct_failed)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "% samples flagged", title = "QC flags") +
  theme_classic()

# ggsave(file.path(plot_dir, "03_qc_flags.png"), p3, width = 9, height = 4.5, dpi = 300)

# ----------------------------
# Export
# ----------------------------
readr::write_csv(dat_complete, file.path(out_dir, "clean_master.csv"))
readr::write_csv(qc_report, file.path(out_dir, "qc_report.csv"))
readr::write_csv(site_models, file.path(out_dir, "site_models.csv"))


