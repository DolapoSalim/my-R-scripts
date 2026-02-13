# BioR — Solution: Enzyme kinetics (Michaelis–Menten) challenge

library(tidyverse)
raw<-read.csv("~/Documents/BioR/2025_2026/PhD/Class_3/MM_kinetic_challange/raw_kinetics.csv", header = TRUE)
# A) Missingness
sum(is.na(raw$rate))

# B) Summary mean ± SD + n
kinetics_summary <- raw %>%
  group_by(variant, inhibitor, conc_uM) %>%
  summarise(
    n = sum(!is.na(rate)),
    mean_rate = mean(rate, na.rm = TRUE),
    sd_rate   = se.fun(rate, na.rm = TRUE), # even better! replace with the SD with standard error run: se.fun<-function(x) sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)]))
    .groups = "drop"
  )

# write.csv(kinetics_summary, "kinetics_summary.csv", row.names = FALSE)

# C) Outliers (IQR within each group)
with_thresholds <- raw %>%
  group_by(variant, inhibitor, conc_uM) %>%
  mutate(
    q1  = quantile(rate, 0.25, na.rm = TRUE),
    q3  = quantile(rate, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    outlier_flag = !is.na(rate) & (rate < lower | rate > upper)
  ) %>%
  ungroup()

kinetics_outliers <- with_thresholds %>%
  dplyr::filter(outlier_flag) %>%
  select(sample_id, variant, inhibitor, conc_uM, rate, lower, upper)

# write.csv(kinetics_outliers, "kinetics_outliers.csv", row.names = FALSE)

# D/E) Plot: raw points + mean ± SD
p <- ggplot() +
  geom_point(
    data = with_thresholds,
    aes(x = conc_uM, y = rate, colour = inhibitor),
    alpha = 0.55, na.rm = TRUE
  ) +
  # optional highlight of outliers
  geom_point(
    data = with_thresholds %>% dplyr::filter(outlier_flag),
    aes(x = conc_uM, y = rate),
    shape = 21, size = 3, stroke = 1, fill = NA
  ) +
  geom_line(
    data = kinetics_summary,
    aes(x = conc_uM, y = mean_rate, colour = inhibitor, group = inhibitor),
    linewidth = 1
  ) +
  geom_point(
    data = kinetics_summary,
    aes(x = conc_uM, y = mean_rate, colour = inhibitor),
    size = 2
  ) +
  geom_errorbar(
    data = kinetics_summary,
    aes(x = conc_uM, ymin = mean_rate - sd_rate, ymax = mean_rate + sd_rate, colour = inhibitor),
    width = 3, alpha = 0.7
  ) +
  facet_grid(variant ~ inhibitor) +
  labs(
    x = "Substrate concentration [µM]",
    y = "Rate [nmol/min]",
    title = "Enzyme kinetics (raw replicates + mean ± SD)"
  ) +
  theme_classic()

# ggsave("kinetics_plot.png", plot = p, width = 10, height = 6, dpi = 300)
p
# Bonus: approximate Vmax and Km (no fitting)
approx_params <- kinetics_summary %>%
  group_by(variant, inhibitor) %>%
  summarise(
    Vmax_approx = max(mean_rate, na.rm = TRUE),
    halfmax = Vmax_approx / 2,
    Km_approx = conc_uM[which.min(abs(mean_rate - halfmax))],
    .groups = "drop"
  )

approx_params
