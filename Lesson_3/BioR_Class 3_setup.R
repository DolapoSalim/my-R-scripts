# BioR — Class 3 (ggplot2) setup
# Installs missing packages, then loads the ones we'll use.

pkgs_plot <- c(
  "scales",
  "readxl",
  "ggrepel",     # non-overlapping labels
  "patchwork",   # combine plots
  "RColorBrewer",
  "viridis",    # colorblind-friendly palettes
  "cowplot",     # alternative plot combining
  "broom",       # tidy model outputs
  "ggpubr",      # quick stat annotations
  "rstatix",     # helper stats
  "ggridges",    # ridge plots
  "ggdist"       # distribution plots
)


# Install missing packages
to_install <- pkgs_plot[!pkgs_plot %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

# Load core
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(ggrepel)
library(patchwork)
library(RColorBrewer)
library(viridis)
library(conflicted)
library(here)


dat <- read.csv("lesson_3/data/BioR_Class3_dataset.csv", header = TRUE)
p <- ggplot(data = dat, aes(x = temperature, y = biomass, color=treatment)) +
  geom_point() +
  labs(title = "Biomass vs Temperature by Treatment", x = "Temperature (°C)", y = "Biomass (g/m²)") +
  # facet_wrap(~ site, ncol = 2, scales = "fixed") +
  geom_smooth(method = "gam", se = TRUE) +
  theme_bw()

#?geom_smooth
#?facet_wrap

p <- ggplot(data = dat, aes(x = day, y = biomass, colour = treatment)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point()

p


p <- ggplot(data = dat, aes(x = day, y = biomass, color = treatment)) +
  #geom_smooth(method = "lm", se = TRUE)
  geom_point(size = 4, alpha=0.5) +
  scale_y_continuous(trans = "log10") +
  labs(x = "Day", y = "Biomass (g/m²)") +
  theme_classic() +
  theme(legend.position = "top")

p




p1 <- ggplot(data = dat, aes(x = treatment, y = biomass, color = treatment)) +
  #geom_smooth(method = "lm", se = TRUE)
  geom_boxplot() +
  labs(x = "Day", y = "Biomass (g/m²)") +
  theme_classic() +
  theme(legend.position = "top")

p1

#Saving with ggplot
ggsave(filename = "Lesson_3/outputs/biomass_timeseries.png", plot = p2, width = 6, height = 4, dpi = 600)


#using patchwork to combine plots
comb_p <- (p|p1)/(p|p1)
comb_p

#Saving with ggplot
ggsave(filename = "Lesson_3/outputs/biomass_timeseries2.png", plot =comb_p, width = 6, height = 4, dpi = 600)


