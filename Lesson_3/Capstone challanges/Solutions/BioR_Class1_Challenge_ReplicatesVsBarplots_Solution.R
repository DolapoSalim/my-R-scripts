# BioR — Challenge Solution: Replicates vs Barplots (dplyr + ggplot2)

library(tidyverse)

# --------------------
# 1) Generate data
# --------------------
sites <- paste0("Site", 1:3)
treatments <- c("Control", "Warming")
reps <- paste0("R", 1:8)

dat <- tidyr::expand_grid(
  site = sites,
  treatment = treatments,
  rep = reps
) %>%
  mutate(
    site_effect = case_when(
      site == "Site1" ~ 0,
      site == "Site2" ~ 5,
      TRUE            ~ -3
    ),
    treat_effect = if_else(treatment == "Warming", 8, 0),
    length_mm = 40 + site_effect + treat_effect + rnorm(n(), sd = 6)
  ) %>%
  select(site, treatment, rep, length_mm)

# Add missing values
dat$length_mm[sample(1:nrow(dat), 3)] <- NA

# Inspect
glimpse(dat)
sum(is.na(dat$length_mm))

# --------------------
# 2) Summaries (mean/sd/se/n) by treatment x site
# --------------------
summary_tbl <- dat %>%
  group_by(treatment, site) %>%
  summarise(
    n = sum(!is.na(length_mm)),
    mean_length = mean(length_mm, na.rm = TRUE),
    sd_length   = sd(length_mm, na.rm = TRUE),
    se_length   = sd_length / sqrt(n),
    .groups = "drop"
  )

# write.csv(summary_tbl, "summary_table.csv", row.names = FALSE)

# --------------------
# 3) Plot 1 (bad on purpose): barplot of means only
# --------------------
p1 <- ggplot(summary_tbl, aes(x = treatment, y = mean_length)) +
  geom_col() +
  facet_wrap(~ site) +
  labs(
    x = "Treatment",
    y = "Length [mm]",
    title = "BAD (on purpose): Barplot of means hides replicates"
  ) +
  theme_classic()

# ggsave("fig1_bad.png", plot = p1, width = 8, height = 4, dpi = 300)

# --------------------
# 4) Plot 2 (better): raw replicates + mean ± SE
# --------------------
p2 <- ggplot(dat, aes(x = treatment, y = length_mm)) +
  geom_jitter(width = 0.12, alpha = 0.6, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 3, na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, na.rm = TRUE) +
  facet_wrap(~ site) +
  labs(
    x = "Treatment",
    y = "Length [mm]",
    title = "BETTER: show raw replicates + mean ± SE"
  ) +
  theme_classic()

# ggsave("fig2_good.png", plot = p2, width = 8, height = 4, dpi = 300)

# --------------------
# 5) Comment (why dotplots can mislead)
# --------------------
# Barplots show only the mean and hide the distribution (spread, outliers) and sample size.
# Two treatments can have the same mean but very different variability.
# Showing raw points + mean ± SE makes biological variability visible.
