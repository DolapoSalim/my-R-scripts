# BioR — Hard Challenge 
library(tidyverse)
library(readxl)
library(lubridate)
library(broom)

data_dir <- "/mnt/data/BioR_SuperChallenge_Intertidal_20260209_200030"
out_dir  <- file.path(data_dir, "outputs")
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)



