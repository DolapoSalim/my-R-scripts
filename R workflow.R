# Set the working directory
setwd("C:/Users/dolap/OneDrive/Documents/DOLAPO/SCHOOL/THESIS/DATA")
# Load necessary library
library(ggplot2)
# Read the dataset from the CSV file
data <- read.csv("C:/Users/dolap/OneDrive/Documents/DOLAPO/SCHOOL/THESIS/DATA/Thesis Data Five Species.csv")

head(data)
install.packages('reshape2')
library(reshape2)

install.packages("multcomp")
library(multcomp)


install.packages("tidyverse")

library(tidyverse)

colnames(data) <- tolower(colnames(data))

# Reshape the data to long format, separating smr by temperature
data_long <- data %>%
    pivot_longer(cols = c(afdw, bw, length, smr),
                 names_to = "Variable",
                 values_to = "Measurement")

#Reshape 'species'
data$species <- as.factor(data$species)



# Separate the 'smr' measurements by temperature
data_long <- data_long %>%
    mutate(Variable = ifelse(Variable == "smr", paste(Variable, temp, sep = "_"), Variable))

# Preview the reshaped data
head(data_long)

# Create a summary data frame containing minimum and maximum values for each group
summary_data <- data_long %>%
    group_by(species, Variable) %>%
    summarise(min = min(Measurement),
              max = max(Measurement))

# Create the boxplot with facet_wrap, including separating smr by temperature
ggplot(data_long, aes(x = species, y = Measurement, fill = species)) +
    geom_boxplot() +
    facet_wrap(~ Variable, scales = "free_y", labeller = labeller(Variable = c(
        afdw = "AFDW (mg)",
        smr_18 = "SMR at 18°C",
        smr_25 = "SMR at 25°C",
        bw = "BW (mg)",
        length = "Length (mm)"
    ))) +
    theme_minimal() +
    labs(title = "Boxplots of Various Measurements for Five Species",
         x = "Species",
         y = "Measurements") +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels by 45 degrees
    scale_fill_manual(values = c("blue", "red", "green", "purple", "orange")) + # Specify colors for each species
    guides(fill = guide_legend(title = "Species")) # Renaming the legend title


# Calculate summary statistics for SMR at 18°C for each species
summary_smr_18 <- data_long %>%
    filter(Variable == "smr_18") %>%
    group_by(species) %>%
    summarise(min = min(Measurement, na.rm = TRUE),
              max = max(Measurement, na.rm = TRUE),
              median = median(Measurement, na.rm = TRUE),
              mean = mean(Measurement, na.rm = TRUE),
              q1 = quantile(Measurement, 0.25, na.rm = TRUE),
              q3 = quantile(Measurement, 0.75, na.rm = TRUE))

# Calculate summary statistics for SMR at 25°C for each species
summary_smr_25 <- data_long %>%
    filter(Variable == "smr_25") %>%
    group_by(species) %>%
    summarise(min = min(Measurement, na.rm = TRUE),
              max = max(Measurement, na.rm = TRUE),
              median = median(Measurement, na.rm = TRUE),
              mean = mean(Measurement, na.rm = TRUE),
              q1 = quantile(Measurement, 0.25, na.rm = TRUE),
              q3 = quantile(Measurement, 0.75, na.rm = TRUE))

# Print the summaries
summary_smr_18
summary_smr_25

write_csv(summary_smr_18, file = 'smr_at_18.csv')
write_csv(summary_smr_25, file = 'smr_at_25.csv')

colnames(data) <- tolower(colnames(data))

# Summarize selected columns for each species
summary_data <- data %>%
    group_by(species) %>%
    summarise(
        min_afdw = min(afdw, na.rm = TRUE),
        max_afdw = max(afdw, na.rm = TRUE),
        mean_afdw = mean(afdw, na.rm = TRUE),
        median_afdw = median(afdw, na.rm = TRUE),
        min_bw = min(bw, na.rm = TRUE),
        max_bw = max(bw, na.rm = TRUE),
        mean_bw = mean(bw, na.rm = TRUE),
        median_bw = median(bw, na.rm = TRUE),
        min_length = min(length, na.rm = TRUE),
        max_length = max(length, na.rm = TRUE),
        mean_length = mean(length, na.rm = TRUE),
        median_length = median(length, na.rm = TRUE)
    )

# View the summary data
summary_data

write_csv(summary_data, file = 'data_summary_for_all_species.csv')


# Filter data for AFDW and SMR at both temperatures
data_both <- data %>%
    filter(temp %in% c("18", "25")) %>%
    select(species, temp, afdw, smr)

# Convert 'temp' to factor
data_both$temp <- factor(data_both$temp)

# Create the plot with different colors for each temperature
plot_both <- ggplot(data_both, aes(x = afdw, y = smr, color = temp)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, aes(group = interaction(species, temp)),
                formula = y ~ x) +
    facet_wrap(~ species, scales = "free_y") +
    labs(title = "Linear Regression of AFDW vs. SMR",
         x = "AFDW", y = "SMR") +
    scale_color_manual(values = c("blue", "red"),
                       breaks = c("18", "25"),
                       labels = c("18C", "25C"))  # Set colors and labels for each temperature

# Print the plot
print(plot_both)




# Function to perform ANOVA and Tukey test for a given variable
perform_anova_tukey <- function(data_long, variable_name) {
    aov_result <- aov(Measurement ~ species, data = data_long %>% filter(Variable == variable_name))
    anova_summary <- summary(aov_result)

    tukey_result <- glht(aov_result, linfct = mcp(species = "Tukey"))
    tukey_summary <- summary(tukey_result)

    list(anova_summary = anova_summary, tukey_summary = tukey_summary)
}

# Variables to analyze
variables_to_analyze <- c("afdw", "smr_18", "smr_25", "bw", "length")

# Perform the analysis for each variable and store the results
analysis_results <- lapply(variables_to_analyze, function(var) {
    perform_anova_tukey(data_long, var)
})

# Naming the results
names(analysis_results) <- variables_to_analyze

# Print the results
analysis_results

# Save the ANOVA and Tukey test results to files
for (var in variables_to_analyze) {
    write_csv(as.data.frame(analysis_results[[var]]$anova_summary), file = paste0(var, "_anova_summary.csv"))
    write_csv(as.data.frame(analysis_results[[var]]$tukey_summary), file = paste0(var, "_tukey_summary.csv"))
}

