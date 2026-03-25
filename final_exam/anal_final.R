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


# load data
biomass_wide <- read.csv("biomass_wide.csv")
drone_ndvi <- read.csv("drone_ndvi.csv")
logger_raw <- read.csv("logger_raw.csv")
site_meta <- read.csv("site_meta.csv")

# Inspect data
View(biomass_wide)
summary(biomass_wide)


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

# Parse dates
parse_date_robust <- function(x) {
  parse_date_time(x, orders = c("Y-m-d", "d/m/Y"), tz = "UTC")
}

# Parse timestamps, handles mixed formats with/without seconds)
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
    day = parse_number(day),
    biomass = parse_column(biomass_old),
    qc_non_numeric_biomass = !is.na(biomass_old) & is.na(biomass),
    # Reconstruct clean sample_id
    sample_id = paste(site, treat, plot, rep, sep = "_")
  ) %>%
  select(site, treat, plot, rep, day, biomass_old, biomass, 
         qc_non_numeric_biomass, sample_id)

summary(biomass_long)

# Check for duplicate biomass records (same site/treat/plot/rep/day)
biomass_duplicate <- biomass_long %>% 
    group_by(site, treat, plot, rep, day) %>%
    filter(n() > 1)

if (nrow(biomass_duplicate) > 0) {
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

#quick inspect
nrow(biomass_long_clean)



## SITE DATA
site_meta_clean <- site_meta %>%
  mutate(
    site = column_fix(site),
    plot = normalize_plot_id(plot),
    canopy_cover_value = parse_pct(canopy_cover_pct),
    qc_canopy_height = !is.na(canopy_cover_value) & canopy_cover_value > 100,
  ) %>%
  group_by(site, plot) %>%
    # QC aggregation
  mutate(qc_meta_duplicate = n() > 1) %>%
  ungroup()


site_meta_clean_filtered <- site_meta_clean %>%
  group_by(site, plot) %>%
  slice(1) %>%
  ungroup()
  
message("Site metadata records: ", nrow(site_meta_clean))
message("Filtered site metadata: ", nrow(site_meta_clean_filtered))



## LOGGER_RAW DATA
logger_raw_clean <- logger_raw %>%
  mutate(
    site = column_fix(site),
    timestamp = parse_ts_robust(timestamp),
    qc_timestamp_fail = is.na(timestamp)
  ) %>%
  filter(!is.na(timestamp))

# Aggregate to site × day (mean across all readings)
logger_day <- logger_raw_clean %>%
  group_by(site, day) %>%
  summarise(
    mean_temp_c = mean(temp_c, na.rm = TRUE),
    mean_humidity_pct = mean(humidity, na.rm = TRUE),
    
    # QC: number of logger readings on this day
    n_logger_records = n(),
    qc_low_logger_coverage = n_logger_records < 10,
  ) %>%
  ungroup()

message("Logger days: ", nrow(logger_day))
logger_new_data <- logger_day


## DRONE NDVI DATA
drone_ndvi_clean <- drone_ndvi %>%
  mutate(
    site = column_fix(site),
    plot = normalize_plot_id(plot),
    flight_id = column_fix(flight_id),
    date_parsed = parse_date_robust(date),
    qc_date_fail = is.na(date_parsed)
  ) %>%
  filter(!is.na(date_parsed))

# QC: NDVI should be between -1 and 1
drone_ndvi_clean <- drone_ndvi_clean %>%
  mutate(
    qc_ndvi_out_of_range = !is.na(ndvi) & (ndvi < -1 | ndvi > 1),
    ndvi_clean = if_else(qc_ndvi_out_of_range, NA_real_, ndvi)
  )

# Check for duplicates
drone_duplicate <- drone_ndvi_clean %>%
  group_by(site, plot, date_parsed) %>%
  filter(n() > 1)

if (nrow(drone_duplicate) > 0) {
  message("Note: Found ", nrow(drone_duplicate), " duplicate drone records (taking mean)")
}

# remove dupklicate take mean NDVI per site/plot/date
drone_day <- drone_ndvi_clean %>%
  group_by(site, plot, date_parsed) %>%
  summarise(
    ndvi = mean(ndvi_clean, na.rm = TRUE),
    n_drone_records = n(),
    qc_drone_duplicate = n() > 1,
    qc_ndvi_out_of_range = any(qc_ndvi_out_of_range, na.rm = TRUE),
  ) %>%
  rename(date = date_parsed) %>%
  ungroup()

message("Drone readings: ", nrow(drone_day))


## JOIN ALL DATA
# Converted drone dates to day index
# Then joined biomass × logger × drone all on site + plot + day
# This allows comparing biomass, temperature, and NDVI on the same day


# Get logger start date to convert drone dates to day index
logger_start_date <- logger_raw_clean %>%
  mutate(date = as.Date(timestamp)) %>%
  pull(date) %>%
  min()


# Add day column to drone_day for joining
drone_day_with_day <- drone_day %>%
  mutate(
    day = as.integer(as.Date(date) - logger_start_date)
  )

# Main analysis table: biomass as anchor
dat <- biomass_long_clean %>%
  # Join site metadata (on site + plot)
  left_join(
    site_meta_clean_filtered %>%
      select(site, plot, shore_height_cm, canopy_cover_value, 
             substrate, collector, qc_canopy_height, qc_meta_duplicate),
    by = c("site", "plot")
  ) %>%
  
  # Join logger data (on site + day)
  left_join(
    logger_day,
    by = c("site", "day")
  ) %>%
  
  # Join drone data (on site + plot + day)
  left_join(
    drone_day_with_day %>%
      select(site, plot, day, date, ndvi, n_drone_records, qc_drone_duplicate, qc_ndvi_out_of_range),
    by = c("site", "plot", "day")
  )

message("Joined records: ", nrow(dat))


## QUALITY CONTROL REPORT

# Overall QC summary
qc_summary <- dat %>%
  pivot_longer(starts_with("qc_"), names_to = "flag", values_to = "failed") %>%
  group_by(flag) %>%
  summarise(
    n_failed = sum(failed, na.rm = TRUE),
    pct_failed = round(mean(failed, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_failed))

print(qc_summary)

# Join diagnostics
join_summary <- dat %>%
  summarise(
    total_records = n(),
    with_biomass = sum(!is.na(biomass)),
    with_logger = sum(!is.na(mean_temp_c)),
    with_drone = sum(!is.na(ndvi)),
    all_three = sum(!is.na(biomass) & !is.na(mean_temp_c) & !is.na(ndvi))
  )
print(join_summary)


## MODELLING/ANALYSIS

# Model 1: Treatment Effect Only (Biomass ~ Treatment)
models_treat_only <- dat %>%
  filter(!is.na(biomass)) %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(biomass ~ treat, data = .x)),
    glance_out = map(fit, broom::glance),
    coef_treat = map_dbl(fit, ~ coef(.x)["treatHEAT"]),
    r2 = map_dbl(glance_out, ~ .x$r.squared),
    p_value = map_dbl(glance_out, ~ .x$p.value),
    n_obs = map_int(glance_out, ~ as.integer(.x$nobs))
  ) %>%
  select(site, n_obs, coef_treat, r2, p_value) %>%
  ungroup()

# Model 2: Temperature Effect (Biomass ~ Mean Temperature)
models_temp <- dat %>%
  filter(!is.na(biomass), !is.na(mean_temp_c)) %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(biomass ~ mean_temp_c, data = .x)),
    glance_out = map(fit, broom::glance),
    slope = map_dbl(fit, ~ coef(.x)["mean_temp_c"]),
    intercept = map_dbl(fit, ~ coef(.x)["(Intercept)"]),
    r2 = map_dbl(glance_out, ~ .x$r.squared),
    p_value = map_dbl(glance_out, ~ .x$p.value),
    n_obs = map_int(glance_out, ~ as.integer(.x$nobs))
  ) %>%
  select(site, n_obs, slope, intercept, r2, p_value) %>%
  ungroup()


# Model 3: NDVI Effect (Biomass ~ NDVI)
models_ndvi <- dat %>%
  filter(!is.na(biomass), !is.na(ndvi)) %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(biomass ~ ndvi, data = .x)),
    glance_out = map(fit, broom::glance),
    slope = map_dbl(fit, ~ coef(.x)["ndvi"]),
    intercept = map_dbl(fit, ~ coef(.x)["(Intercept)"]),
    r2 = map_dbl(glance_out, ~ .x$r.squared),
    p_value = map_dbl(glance_out, ~ .x$p.value),
    n_obs = map_int(glance_out, ~ as.integer(.x$nobs))
  ) %>%
  select(site, n_obs, slope, intercept, r2, p_value) %>%
  ungroup()

# Model 4: Combined Model (Biomass ~ Temperature + NDVI + Treatment)
models_combined <- dat %>%
  filter(!is.na(biomass), !is.na(mean_temp_c), !is.na(ndvi)) %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(biomass ~ mean_temp_c + ndvi + treat, data = .x)),
    glance_out = map(fit, broom::glance),
    coef_treat = map_dbl(fit, ~ coef(.x)["treatHEAT"]),
    r2 = map_dbl(glance_out, ~ .x$r.squared),
    p_value = map_dbl(glance_out, ~ .x$p.value),
    n_obs = map_int(glance_out, ~ as.integer(.x$nobs))
  ) %>%
  select(site, n_obs, coef_treat, r2, p_value) %>%
  ungroup()


# Model Comparison Summary
comparison <- data.frame(
  site = c(models_treat_only$site, models_temp$site, models_ndvi$site, models_combined$site),
  model = c(rep("Treatment", nrow(models_treat_only)), 
            rep("Temperature", nrow(models_temp)),
            rep("NDVI", nrow(models_ndvi)),
            rep("Combined", nrow(models_combined))),
  r2 = c(models_treat_only$r2, models_temp$r2, models_ndvi$r2, models_combined$r2)
)


# PLOTS
# color palettes
color_treat <- c("CTRL" = "#2E86AB", "HEAT" = "#A23B72")
color_site <- c("SITE1" = "#06A77D", "SITE2" = "#D62828")

# Plot 1: Biomass response over time (aggregated by site + treatment)
p1 <- dat %>%
  group_by(site, treat, day) %>%
  summarise(mean_biomass = mean(biomass, na.rm = TRUE),
            se_biomass = sd(biomass, na.rm = TRUE) / sqrt(n()),
            .groups = "drop") %>%
  ggplot(aes(x = day, y = mean_biomass, color = treat, fill = treat)) +
  geom_line(size = 1.2) +
  geom_point(size = 4, shape = 21, color = "black", stroke = 1) +
  geom_errorbar(aes(ymin = mean_biomass - se_biomass, 
                    ymax = mean_biomass + se_biomass), 
                width = 1, size = 0.8, alpha = 0.6) +
  facet_wrap(~ site, scales = "free_y") +
  scale_color_manual(values = color_treat, name = "Treatment") +
  scale_fill_manual(values = color_treat, name = "Treatment") +
  labs(x = "Day", y = "Biomass (mean ± SE)", title = "Biomass Response Over Time") +
  theme_minimal()

ggsave(file.path(plot_dir, "01_biomass_timeseries.png"), p1, 
       width = 10, height = 5, dpi = 300)


# Plot 2: Biomass vs Temperature (separated by treatment)
p2 <- dat %>%
  ggplot(aes(x = mean_temp_c, y = biomass, color = treat, fill = treat)) +
  geom_point(alpha = 0.5, size = 3, shape = 21, color = "black", stroke = 0.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1.2) +
  facet_grid(treat ~ site) +
  scale_color_manual(values = color_treat, name = "Treatment") +
  scale_fill_manual(values = color_treat, name = "Treatment") +
  labs(x = "Mean Temperature (°C)", y = "Biomass", 
       title = "Biomass vs Daily Mean Temperature") +
  theme_minimal()

ggsave(file.path(plot_dir, "02_biomass_vs_temperature.png"), p2, 
       width = 11, height = 6, dpi = 300)


# Plot 3: Biomass vs NDVI (separated by treatment)
p3 <- dat %>%
  ggplot(aes(x = ndvi, y = biomass, color = treat, fill = treat)) +
  geom_point(alpha = 0.5, size = 3, shape = 21, color = "black", stroke = 0.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1.2) +
  facet_grid(treat ~ site) +
  scale_color_manual(values = color_treat, name = "Treatment") +
  scale_fill_manual(values = color_treat, name = "Treatment") +
  labs(x = "NDVI", y = "Biomass", 
       title = "Biomass vs Drone NDVI") +
  theme_minimal()

ggsave(file.path(plot_dir, "03_biomass_vs_ndvi.png"), p3, 
       width = 11, height = 6, dpi = 300)


## EXPORT
write_csv(dat, file.path(output_dir, "clean_master.csv"))
write_csv(models_treat_only, file.path(output_dir, "01_model_treatment.csv"))
write_csv(models_temp, file.path(output_dir, "02_model_temperature.csv"))
write_csv(models_ndvi, file.path(output_dir, "03_model_ndvi.csv"))
write_csv(models_combined, file.path(output_dir, "04_model_combined.csv"))
write_csv(comparison, file.path(output_dir, "05_model_comparison.csv"))


# Key findings:
# Temperature + NDVI + Treatment together explain 60-69% of biomass variation
# SITE2 responds stronger to the combined model (r² = 0.686 vs 0.598)
# Treatment matters — the heat treatment affects biomass significantly
# Synergy effect — together they're much better than individually
# variation may come from other factors (canopy cover, substrate, humidity, etc.).

