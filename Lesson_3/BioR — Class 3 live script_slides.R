# ============================================================
# BioR — Class 3 live script (on slides)
# Topics: dplyr basics, summarise by group, joins, quick ggplot
# ============================================================

# Read the dataset from a fixed path.


# Option A:# setwd() is used here only for teaching purposes.
# In real projects, prefer an RStudio Project + here::here() for portable paths. Here a useful link that explain the here package (https://here.r-lib.org/)
# It really comes in handy for developing portable projects 
# Tip: for teaching/reproducibility, prefer here::here("Class_3", "BioR_Class3_dataset.csv")
# setwd("PATH/TO/YOUR/PROJECT/FOLDER")
dat <- read.csv("BioR_Class3_dataset.csv", header = TRUE)
# Option B: use file.choose() (easy, but NOT recommended for reproducible scripts)
# dat <- read.csv(file.choose(), header = TRUE)
# ------------------------------------------------------


# Load ggplot2 (the main plotting package in the grammar of graphics).
library(ggplot2)

# ============================================================
# Example 1: "Full plot object" (build once, print/save later)
# ============================================================

# 1) Start with ggplot(data, aes(...)):
#    - aes() defines the mapping from data columns to plot aesthetics (x, y, color, etc.)
#    - Here: temperature on x, biomass on y, treatment mapped to color (creates a legend)
p <- ggplot(dat, aes(x = temperature, y = biomass, color = treatment)) +
  # Add points for each observation; alpha makes points semi-transparent (reduces overplotting)
  geom_point(alpha = 0.7) +
  # Add a linear regression line per group (because color is mapped to treatment)
  # se = TRUE adds confidence interval around the line
  geom_smooth(method = "lm", se = TRUE) +
  # Split into panels by site (small multiples).
  facet_wrap(~ site, scales = "free_x") +
  # Axis labels (and units)
  labs(x = "Temperature (°C)", y = "Biomass (g m^-2)") +
  # Clean minimal theme
  theme_classic()

# 2) Print the plot
p

# ============================================================
# Quick examples of common plot types
# ============================================================

# Scatter plot: relationship between two continuous variables, colored by treatment
ggplot(dat, aes(temperature, biomass, color = treatment)) +
  geom_point()

# Boxplot: distribution of biomass by treatment (good alternative to barplots)
ggplot(dat, aes(treatment, biomass)) +
  geom_boxplot()

# Time series:
# IMPORTANT: group = rep connects points *within replicate*, not across replicates.
ggplot(dat, aes(day, biomass, color = treatment, group = rep)) +
  geom_line() +
  geom_point()

# ============================================================
# "Mapping" vs "Setting" aesthetics
# ============================================================

# MAPPING (inside aes): uses a variable -> legend is created
ggplot(dat, aes(temperature, biomass, color = treatment)) +
  geom_point(size = 2, alpha = 0.7)

# SETTING (outside aes): constant value -> no legend
ggplot(dat, aes(temperature, biomass)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7)

# ============================================================
# Facets example (one panel per site)
# ============================================================

ggplot(dat, aes(day, biomass, color = treatment)) +
  geom_point(size = 1.5) +
  # lm here fits biomass ~ day (per treatment, within each facet)
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ site, nrow = 1) +
  labs(x = "Day", y = "Biomass") +
  theme_bw()

# ============================================================
# Linear trend per treatment (biomass ~ temperature)
# ============================================================

ggplot(dat, aes(temperature, biomass, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE)

# ============================================================
# Mean ± SE by day
# ============================================================

# stat_summary computes summaries on the fly:
# - fun = mean draws mean line/points
# - fun.data = mean_se draws mean ± standard error
ggplot(dat, aes(day, biomass, color = treatment)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)

# ============================================================
# Transformations + labels
# ============================================================

ggplot(dat, aes(temperature, biomass, color = treatment)) +
  geom_point(alpha = 0.7) +
  # Log-transform the y-axis (good if biomass spans orders of magnitude)
  scale_y_continuous(trans = "log10") +
  labs(
    title = "Biomass vs temperature",
    x = "Temperature (°C)",
    y = "Biomass (g m^-2, log10)",
    color = "Treatment"
  ) +
  theme_minimal()

# ============================================================
# Saving plots (PNG and PDF)
# ============================================================

p <- ggplot(dat, aes(temperature, biomass, color = treatment)) +
  geom_point() +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")

# Tip: ensure the folder exists before saving
# dir.create("outputs", showWarnings = FALSE)

# Save bitmap (good for slides)
ggsave("outputs/fig_biomass_temp.png", p, width = 6, height = 4, dpi = 300)

# Save vector (best for papers; stays sharp when zooming)
ggsave("outputs/fig_biomass_temp.pdf", p, width = 6, height = 4)

# ============================================================
# Useful add-on packages for ggplot workflows
# ============================================================

# 1) ggrepel: labels that don't overlap (sites/species/outliers)
library(ggrepel)
p <- ggplot(dat, aes(temperature, biomass, label = sample_id)) +
  geom_point() +
  geom_text_repel(max.overlaps = 20)
p

# 2) patchwork: combine multiple ggplots (multi-panel figures)
library(patchwork)
p1 <- ggplot(dat, aes(temperature, biomass)) + geom_point()
p2 <- ggplot(dat, aes(treatment, biomass)) + geom_boxplot()
p1 | p2

# 3) viridis: colorblind-friendly palettes
library(viridis)
ggplot(dat, aes(temperature, biomass, color = site)) +
  geom_point() +
  scale_color_viridis_d()

# 4) scales: control axis labels (rounding, scientific notation, etc.)
library(scales)
ggplot(dat, aes(day, biomass)) +
  geom_point() +
  scale_y_continuous(labels = label_number(accuracy = 0.1))

# ============================================================
# ggbeeswarm: show replicates without overlap (great alternative to jitter)
# ============================================================

library(ggbeeswarm)
ggplot(dat, aes(treatment, biomass)) +
  geom_boxplot(outlier.shape = NA) +
  geom_beeswarm()

# Pitfall 1: bar plots hide replicate variation
# Prefer: points + box/violin/beeswarm
ggplot(dat, aes(treatment, biomass)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0)

# Pitfall 2: overplotting (points overlap too much)
# Solutions: alpha, jitter, beeswarm
ggplot(dat, aes(temperature, biomass)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.6)
