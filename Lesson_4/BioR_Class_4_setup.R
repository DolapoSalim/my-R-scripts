# ============================================================
# BioR — Class 4 setup (Beluga + Interactive plots + Shiny)
# Installs missing packages, then loads them.
# ============================================================


pkgs <- c(
  "ggthemes",
  "scales",
  "patchwork",
  "mapdata",
  "png",
  "grid",
  "rgbif",
  "CoordinateCleaner",
  "terra",
  "broom",
  "plotly",
  "htmlwidgets",
  "shiny"
)


# Install missing packages
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

# Load packages (grouped, so students see what is for what)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

library(ggthemes)
library(ggrepel)
library(viridis)
library(scales)
library(patchwork)

library(mapdata)
library(png)
library(grid)

library(rgbif)
library(CoordinateCleaner)
library(terra)

library(broom)

library(plotly)
library(htmlwidgets)

library(shiny)

