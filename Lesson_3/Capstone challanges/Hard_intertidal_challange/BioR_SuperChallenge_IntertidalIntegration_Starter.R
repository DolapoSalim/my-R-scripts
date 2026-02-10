# BioR — Hard Challenge 
library(tidyverse)
library(readxl)
library(lubridate)
library(broom)
library(RColorBrewer)

data_dir <- "Lesson_3/Capstone challanges/Hard_intertidal_challange/data"
out_dir  <- file.path(data_dir, "outputs")
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)


#Input all data
asv_data <- read.csv(file.path(data_dir, "asv_table.csv"), header = TRUE)
biomass_data <- read.csv(file.path(data_dir, "biomass_wide.csv"), header = TRUE)
loggerMap_data <- read.csv(file.path(data_dir, "logger_map.csv"), header = TRUE)
loggerRaw_data <- read.csv(file.path(data_dir, "logger_raw.csv"), header = TRUE)
quadratMeta_data <- read_xlsx(file.path(data_dir, "quadrat_meta.xlsx"), sheet = 1)

#Clean Data
day_7 <- biomass_data$day_7


#replace empty cell and ND with mean value of day_7
day_7[is.na(day_7) | day_7 == ""] <- mean(as.numeric(day_7), na.rm = TRUE)
day_7[day_7 == "ND"] <- mean(as.numeric(day_7), na.rm = TRUE)
day_7 <- round(as.numeric(day_7), 2)
biomass_data$day_7 <- day_7

#Change data types of day columns to numeric
biomass_data <- biomass_data %>%
  mutate(across(starts_with("day_"), ~ as.numeric(.)))
str(biomass_data)

#Pivot data longer and checking sample_id seperator
biomass_data_clean <- biomass_data %>%
  pivot_longer(cols = starts_with("day_"), names_to = "day", values_to = "biomass") %>%
  #Separate sample_id into site, treatment, rep, and separator is "_" and "-"
  separate(sample_id, into = c("site", "habitat", "rep"), sep = "[-_]", remove = FALSE) %>%
  mutate(day=readr::parse_number(day))
View(biomass_data_clean)


#change all biomass$habitat to uppercase
biomass_data_clean$habitat <- toupper(biomass_data_clean$habitat)
View(biomass_data_clean)
biomass_data_clean$site <- toupper(biomass_data_clean$site)
View(biomass_data_clean)

#change all biomass$rep cell with R01 to R1, R02 to R2, R03 to R3, R04 to R4, and R05 to R5
biomass_data_clean$rep <- gsub("R0", "R", biomass_data_clean$rep)
View(biomass_data_clean)

# create qc_non_numeric_biomass flag
biomass_data_clean <- biomass_data_clean %>%
  mutate(qc_non_numeric_biomass = ifelse(is.na(biomass) | biomass == "" | biomass == "ND", TRUE, FALSE))
View(biomass_data_clean)

?mutate
