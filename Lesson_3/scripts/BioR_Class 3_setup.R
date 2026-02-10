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
