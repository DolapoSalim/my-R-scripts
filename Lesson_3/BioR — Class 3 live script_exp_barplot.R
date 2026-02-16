# ============================================================
# BioR — Class 3 live script (barplot from experiments)
# Topics: dplyr basics, summarise by group, joins, quick ggplot
# ============================================================

# --- Packages (install once, then just library()) ---
# install.packages(c("tidyverse", "RColorBrewer", "lubridate", "janitor", "here"))
library(tidyverse)
library(RColorBrewer)

# Option A:# setwd() is used here only for teaching purposes.
# In real projects, prefer an RStudio Project + here::here() for portable paths. Here a useful link that explain the here package (https://here.r-lib.org/)
# It really comes in handy for developing portable projects 
# setwd("PATH/TO/YOUR/PROJECT/FOLDER")
dat.exp <- read.csv("dat.exp.csv", header = TRUE)
# Option B: use file.choose() (easy, but NOT recommended for reproducible scripts)
# dat.exp <- read.csv(file.choose(), header = TRUE)
# ------------------------------------------------------------
# 1) dplyr essentials: rename / filter / mutate / distinct
# ------------------------------------------------------------

# rename columns (example)
dat.exp.rn <- dat.exp %>%
  rename(Quadrato = plot, Treat = treat)

# filter: subset rows based on conditions
dat.exp %>%
  dplyr::filter(treat == "control", site == 1)

dat.exp %>%
  dplyr::filter(length > 10)

# mutate: create new columns
dat.exp %>%
  mutate(area = length * width)

# distinct: keep one row per plot (example)
dat.exp %>%
  distinct(plot, .keep_all = TRUE)

# ------------------------------------------------------------
# 2) Grouped summaries + SE function
# ------------------------------------------------------------

se_fun <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  sd(x) / sqrt(length(x))
}

#Aggregating by treatment (no sites)
dat.exp.agg.treat <- dat.exp %>%
  dplyr::filter(!is.na(length)) %>%
  group_by(treat) %>%
  summarise(
    mean_l = mean(length),
    se     = se_fun(length),
    n      = n(),
    .groups = "drop"
  )




# Make sure site is a factor (needed for consistent dodging)
dat.exp.site <- dat.exp.site %>% mutate(site = factor(site))
dat.exp.agg.site <- dat.exp.agg.site %>% mutate(site = factor(site))

plot_exp.treat <- ggplot(dat.exp.agg.treat, aes(x = treat, y = mean_l, fill = treat, colour = treat)) +
  # Bars = group means
  geom_col(alpha = 0.5) +
  
  # Error bars = mean ± SE
  geom_errorbar(aes(ymin = mean_l, ymax = mean_l + se),# Here I plot only the upper error bar (mean +SE) (ymax=mean_l+se) if you want mean ± SE set also ymin= mean_l-se
                width = 0.25) +
  
  # Raw data = jittered points, dodged by site (so they sit over the right bar)
  geom_point(
    data = dat.exp %>% dplyr::filter(!is.na(length)),# here I used filter function of dplyr to filter out NA values of "length" variable. We can use also dplyr inside of ggplot
    aes(x = treat, y = length),
    position = position_jitterdodge(
      dodge.width = 0.9,   #Good practice: define a single dodge width and reuse it everywhere. Use a single 'dodge_width' (e.g., dodge_width <- 0.9) object so bars, error bars, and raw points always align.
      jitter.width = 0.12, #horizontal jitter within each site/treat group. This prevents points overlapping.
      jitter.height = 0
    ),
    color="grey30",
    alpha = 0.6,
    size = 1.8
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  labs(y = "Length (mm) [Mean +SE, n = 4 or 3]", x = "Treatment") +
  theme_classic()

plot_exp.treat

# The warming treatment shows higher dispersion than the control and artifact control
# (procedural control: the warming device without heating).
# This suggests the warming effect may not be consistent across sites.
# Therefore, it’s worth plotting the data by site (e.g., facet by site) to inspect site-specific responses.


dat.exp.agg.site <- dat.exp %>%
  dplyr::filter(!is.na(length)) %>%
  group_by(site, treat) %>%
  summarise(
    mean_l = mean(length),
    se     = se_fun(length),
    n      = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3) ggplot
# ------------------------------------------------------------


# Make sure site is a factor (needed for consistent dodging)
dat.exp.agg.site <- dat.exp.agg.site %>% mutate(site = factor(site))

plot_exp.site <- ggplot(dat.exp.agg.site, aes(x = treat, y = mean_l, fill = treat, colour = treat, group=site)) +
  # Bars = group means
  geom_col(position = position_dodge(width = 0.9),
           alpha = 0.5) +
  
  # Error bars = mean ± SE
  geom_errorbar(aes(ymin = mean_l, ymax = mean_l + se),# Here I plot only the upper error bar (mean +SE) (ymax=mean_l+se) if you want mean ± SE set also ymin= mean_l-se
                position = position_dodge(width = 0.9),
                width = 0.25) +
  
  # Raw data = jittered points, dodged by site (so they sit over the right bar)
  geom_point(
    data = dat.exp %>% dplyr::filter(!is.na(length)),# here I used filter function of dplyr to filter out NA values of "length" variable. We can use also dplyr inside of ggplot
    aes(x = treat, y = length),
    position = position_jitterdodge(
      dodge.width = 0.9,   #Good practice: define a single dodge width and reuse it everywhere. Use a single 'dodge_width' (e.g., dodge_width <- 0.9) object so bars, error bars, and raw points always align.
      jitter.width = 0.12, #horizontal jitter within each site/treat group. This prevents points overlapping.
      jitter.height = 0
    ),
    color="grey30",
    alpha = 0.6,
    size = 1.8
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  labs(y = "Length (mm) [Mean +SE, n = 4 or 3]", x = "Treatment") +
  theme_classic()

plot_exp.site

# From the plot, length in the warming treatment varies across sites.
# This suggests the warming effect is not consistent among sites (site-specific responses).
# In a formal analysis (e.g., two-way ANOVA), this pattern would likely appear as a significant
# Treatment × Site interaction.



# Optional alternative: show replicates (if you have raw replicate rows)
ggplot(dat.exp, aes(treat, length, colour = factor(site))) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.3)) +
  theme_classic()

# ------------------------------------------------------------
# 4) Another dataset example: Acacia survey (source: https://datacarpentry.github.io/semester-biology/materials/ggplot/)
# ------------------------------------------------------------

dat.exp.graz <- read.csv("ACACIA_DREPANOLOBIUM_SURVEY.txt", sep = "\t", na.strings = c("dead"))

ggplot(dat.exp.graz, aes(x = CIRC, y = HEIGHT)) +
  geom_point(alpha = 0.5) +
  labs(x = "Circumference [cm]", y = "Height [m]", title = "Acacia Survey at UHURU") +
  theme_classic() +
  scale_y_log10() +
  scale_x_log10()

ggplot(dat.exp.graz, aes(x = CIRC, y = HEIGHT, colour = TREATMENT)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ TREATMENT) +
  labs(x = "Circumference [cm]", y = "Height [m]", title = "Acacia Survey at UHURU") +
  theme_classic() +
  scale_y_log10() +
  scale_x_log10()

ggplot(dat.exp.graz, aes(x = CIRC)) +
  geom_histogram(bins = 15)

