# Load an .RData file (can contain one or more objects).
# After this, use ls() to see what objects were loaded.
getwd()
load(file = "Lesson_4/beluga_data.rdata")
ls()

# ------------------------------------------------------------
# Libraries / packages
# ------------------------------------------------------------


# Vector of packages we want for this workflow
R_pack.vec <- c(
  "tidyverse",     # data wrangling + ggplot2 + purrr
  "ggthemes",      # extra ggplot themes (incl. map styles)
  "ggalt",         # some map/projection helpers (can be version-sensitive)
  "ggrepel",       # non-overlapping labels
  "viridis",       # colorblind-friendly palettes
  "broom",         # turn model outputs into tidy tables
  "treemapify",    # treemap / area-based plots
  "wesanderson",   # fun palettes (optional)
  "rgbif",         # GBIF occurrence downloads
  "mapdata"        # map outlines
)

# Install only the missing packages
R_pack.ins.vec <- R_pack.vec[!(R_pack.vec %in% installed.packages()[, "Package"])]
if (length(R_pack.ins.vec)) install.packages(R_pack.ins.vec)

# Load packages
library(tidyverse)
library(ggthemes)   # for ggplot themes (incl. mapping theme)
library(ggalt)
library(ggrepel)    # for text labels that don't overlap
library(viridis)    # good palettes
library(broom)      # tidy() and glance() for models
library(treemapify) # treemaps/area plots
library(wesanderson)
library(rgbif)
library(mapdata)
library(terra)
library(CoordinateCleaner)




# ------------------------------------------------------------
# Optional: Download occurrences from GBIF (it could take timef)
# ------------------------------------------------------------
beluga <- occ_search(scientificName = "Delphinapterus leucas",
                     limit = 20000, hasCoordinate = TRUE, return = "data")

# seaotter <- occ_search(scientificName = "Enhydra lutris",
#                        limit = 20000, hasCoordinate = TRUE, return = "data")






beluga_simp <- beluga %>%
  # Keep only the key columns needed for mapping / basic summaries.
  # - key: unique record identifier (GBIF occurrence key)
  # - name: scientific name
  # - decimalLatitude / decimalLongitude: coordinates
  # - year: observation year
  # - individualCount: reported number of individuals (often missing/NA)
  # - country: country of occurrence
  dplyr::select(key, name, decimalLatitude, decimalLongitude,
                year, individualCount, country)

# ------------------------------------------------------------
# Data formatting & manipulation
# ------------------------------------------------------------

# If beluga is a GBIF object, beluga$data contains occurrences.
# This line is ok only if beluga exists and has $data.
beluga_df <- as.data.frame(beluga)

# Simplify occurrence data frame:
# Select only a few columns we care about (key, species name, coords, year, count, country)
# NOTE: this should be beluga$data %>% select(...), not beluga %>% select(...),
# unless beluga itself is already a data frame.
beluga_simp <- beluga$data %>%
  dplyr::select(key, name, decimalLatitude, decimalLongitude,
                year, individualCount, country)

# ------------------------------------------------------------
# Format population change data (wide -> long)
# ------------------------------------------------------------
# Here you reference beluga_PLD and beluga.pop <- beluga_PLD %>% ...
# but the next line uses beluga.pop <- beluga_PLD %>% ...
# and earlier you had beluga_LPD from sea_ott_LPD.
#
# Make sure the object name is consistent:
# - either beluga_PLD is your population dataset
# - or beluga_LPD is your population dataset

beluga_pop <- beluga_PLD %>%   # <-- check this object exists (maybe beluga_LPD?)
  dplyr::filter(Genus == "Delphinapterus" & Species == "leucas") %>%
  # Convert wide year columns (e.g., X1950...X2020) to long format
  pivot_longer(
    names(beluga_PLD)[34:104],
    names_to = "year",
    values_to = "abundance"
  ) %>%
  # Drop "NULL" strings (note: NA is different from "NULL")
  dplyr::filter(abundance != "NULL") %>%
  mutate(abundance = as.numeric(abundance)) %>%
  group_by(ID) %>%
  # Scale population within each ID to 0–1 so trends are comparable
  mutate(scalepop = (abundance - min(abundance)) / (max(abundance) - min(abundance))) %>%
  # Keep only populations with enough time points
  dplyr::filter(length(unique(year)) > 4) %>%
  ungroup()


View(beluga_pop)
# This prints structure of sea otter population object,
# but you created beluga.pop above. Likely meant:
str(beluga_pop)

# Convert year column from strings like "X2010" -> 2010
beluga_pop$year <- parse_number(beluga_pop$year)

# ------------------------------------------------------------
# Fit one linear model per population and extract slopes
# ------------------------------------------------------------

beluga_slopes <- beluga_pop %>%
  # Group by population identity + coordinates
  group_by(Location, Latitude, Longitude, ID) %>%

  # Fit a model per group: scaled abundance ~ year
  # # NOTE: do() works, but nest()/map() is more modern tidyverse style.
  # do(mod = lm(scalepop ~ year, data = .)) %>%
  # ungroup() %>%
  nest() %>%                              # one row per population, data = tibble of its time series
  mutate(
    mod   = purrr::map(data, ~ lm(scalepop ~ year, data = .x)),  # fit one model per population
    coef  = purrr::map(mod, tidy)                               # tidy coefficients per model
  ) %>%
  select(ID, Location, Latitude, Longitude, coef) %>%
  unnest(coef) %>%                           # expand coefficient table into rows
  mutate(
    Latitude  = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )


# For analysis and plotting, we often need the intercept and slopes to be columns, not rows
# Format data frame with model outputs
beluga_slopes <- beluga_slopes %>%
  dplyr::select(Location, Latitude, 
                Longitude, ID, term, estimate) %>%  
  # Select the columns we need
  spread(term, estimate) %>%  # spread() is the opposite of gather()
  ungroup()




# Create a map of beluga occurrences and print it immediately
# (the parentheses + "<-" assign the plot to beluga.map AND display it)
(beluga_map <- ggplot(beluga_simp, aes(x = decimalLongitude, y = decimalLatitude)) +
    
    # Add a high-resolution world outline (from mapdata)
    # ylim limits latitude to the Arctic/temperate region where belugas occur
    borders(
      "worldHires",  #worldHires 
      ylim   = c(40, 100),
      colour = "gray40",
      fill   = "gray40",
      size   = 0.3
    ) +
    
    # Apply a clean map theme (from ggthemes)
    theme_map() +
    
    # Plot occurrence points (each row = one observation)
    # alpha controls transparency: 0 = fully transparent, 1 = fully opaque
    geom_point(alpha = 0.5, size = 0.5, colour = "aquamarine3")
)

ggplotly(beluga_map, tooltip = "all")

# Save the beluga map to a PDF file.
# - dpi is mainly relevant for raster outputs (png/jpg), but harmless for pdf
# - useDingbats=FALSE avoids font-symbol issues in some PDF viewers/journals
ggsave(
  plot = beluga_map,                  # <- save the plot object you created
  filename = "beluga_map.pdf",
  dpi = 1200,
  useDingbats = FALSE,
  width = 18, height = 11, units = "cm"
)





# set CRS if missing (WGS84 lon/lat)
if (is.na(terra::crs(seas_ref)) || terra::crs(seas_ref) == "") {
  terra::crs(seas_ref) <- "EPSG:4326"
}
beluga.clean <- clean_coordinates(
  beluga_simp,
  lon = "decimalLongitude", lat = "decimalLatitude",
  species = "name",
  tests = c("outliers", "seas", "zeros"),
  outliers_method = "distance",
  outliers_td = 5000,
  value = "clean"
)


nrow(beluga.clean)
nrow(beluga_simp)
# Map of cleaned beluga occurrences + population trend locations
sea_otter.map.LPI <- ggplot(
  beluga.clean,                               # base data: cleaned occurrence records
  aes(x = decimalLongitude, y = decimalLatitude)  # map lon/lat to x/y
) +
  # Add a world outline (base R maps). ylim focuses on northern latitudes.
  borders(
    "world",
    ylim = c(40, 100),
    colour = "gray40",
    fill = "gray40",
    size = 0.3
  ) +
  # Apply a clean map theme (from ggthemes)
  theme_map() +
  # Plot beluga occurrence points (many points, so use transparency)
  geom_point(alpha = 0.3, size = 2, colour = "aquamarine3") +
  # Overlay population-trend locations (from beluga.slopes)
  # IMPORTANT: this layer uses a different dataset, so we specify data=...
  # and map its coordinate columns to x/y.
  geom_point(
    data = beluga.slopes,
    aes(x = Longitude, y = Latitude),
    size = 4,
    colour = "tan1"
  )

# Print the plot
sea_otter.map.LPI




# Subset the GBIF occurrence data to records from Greenland only
greenland <- dplyr::filter(beluga_df, country == "Greenland")

# Keep only coordinates and remove duplicated coordinate pairs
# (many occurrences can have identical lon/lat, e.g., repeated observations at the same location)
greenland <- dplyr::select(greenland, decimalLongitude, decimalLatitude) %>%
  distinct()

# View the unique Greenland coordinates (interactive viewer in RStudio)
View(greenland)

# After inspection, you decide one point is suspicious (far east outlier)
# Longitude = -46, Latitude = 65
# (Often these are digitisation / rounding / swapped-coordinate artifacts.)

# Identify which rows in beluga.clean match exactly those coordinates
# NOTE: exact == comparisons on floats can be risky if values are not exactly stored.
which(beluga.clean$decimalLongitude == -46 & beluga.clean$decimalLatitude == 65)
# -> row numbers: 6879 6880 6881 6882 6955

# Remove those rows by index (base R approach)
# beluga.clean[-c(...), ] means: keep all rows EXCEPT those indices
beluga.base <- beluga.clean[-c(6879, 6880, 6881, 6882, 6955), ]

# Alternative (tidyverse): filter them out
# IMPORTANT: to remove exactly that point, you want to drop rows where BOTH lon AND lat match.
# The clean logical form is: !(lon == -46 & lat == 65)
beluga.pipe <- beluga.clean %>%
  dplyr::filter(!(decimalLongitude == -46 & decimalLatitude == 65))

# Check that both methods produce the same result:
# anti_join returns rows that are in beluga.base but not in beluga.pipe.
# If the output is empty, the data frames are identical (same rows).
anti_join(beluga.base, beluga.pipe)

# Inspect the first rows
head(beluga.base)

# Map beluga occurrences (after removing the outlier point) and overlay population trend locations
(beluga.map.LPI <- ggplot(beluga.base, aes(x = decimalLongitude, y = decimalLatitude)) +
    borders("world", ylim = c(40, 100), colour = "gray40", fill = "gray40", size = 0.3) +
    theme_map() +
    geom_point(alpha = 0.3, size = 2, colour = "aquamarine3") +
    # Overlay population-change locations (from beluga.slopes)
    geom_point(data = beluga.slopes, aes(x = Longitude, y = Latitude),
               size = 4, colour = "tan1")
)



# Inspect the location names (useful for spotting inconsistent spelling/encoding)
beluga.slopes$Location

# ------------------------------------------------------------
# Make site/location names consistent (important for plotting and grouping)
# ------------------------------------------------------------
# Sometimes text is imported with weird encodings (e.g., Québec can appear as garbled characters).
# Here we recode a few known variants into a single standard name.

beluga.slopes$Location <- recode(
  beluga.slopes$Location,
  "Cook Inlet stock, Alaska" = "Cook Inlet stock"
)

beluga.slopes$Location <- recode(
  beluga.slopes$Location,
  "Eastern Hudson Bay, Qu?\xabbec" = "Eastern Hudson Bay"
)

beluga.slopes$Location <- recode(
  beluga.slopes$Location,
  "St. Lawrence estuary population" = "St. Lawrence Estuary"
)

beluga.slopes$Location <- recode(
  beluga.slopes$Location,
  "St. Lawrence Estuary population" = "St. Lawrence Estuary"
)

beluga.slopes$Location <- recode(
  beluga.slopes$Location,
  "St. Lawrence estuary, Canada" = "St. Lawrence Estuary"
)

# Check that the names are now consistent
print(beluga.slopes$Location)

# Teaching note:
# Even if you filtered to beluga records, sometimes the original object contains extra attributes
# or unused levels—this is usually not a problem as long as your filtering is correct.

# ------------------------------------------------------------
# Load packages to add an image/icon onto a ggplot
# ------------------------------------------------------------
packs <- c("png", "grid")
lapply(packs, require, character.only = TRUE)

# Load a PNG icon from file and convert it into a grob (graphical object)
icon <- readPNG("beluga_icon.png")
icon <- rasterGrob(icon, interpolate = TRUE)

# ------------------------------------------------------------
# Final map: occurrences + labelled population locations + icon
# ------------------------------------------------------------
beluga.map.final <- ggplot(beluga.base, aes(x = decimalLongitude, y = decimalLatitude)) +
  # World outline (focus on northern latitudes)
  borders("world", ylim = c(40, 100), colour = "gray40", fill = "gray40", size = 0.3) +
  theme_map() +
  # Plot occurrence points (many points -> use transparency)
  geom_point(alpha = 0.3, size = 2, colour = "aquamarine3") +
  
  # Add text labels for a subset of populations (first 3 rows here)
  # We subset because one site may have multiple population time series,
  # but we only want to label the location once.
  geom_label_repel(
    data = beluga.slopes[1:3, ],
    aes(x = Longitude, y = Latitude, label = Location),
    box.padding = 1,
    size = 5,
    nudge_x = 1,
    
    # Nudge labels up or down depending on population ID
    # If ID == 13273 -> move label upward, else move downward
    nudge_y = ifelse(beluga.slopes[1:3, ]$ID == 13273, 4, -4),
    
    min.segment.length = 0,
    inherit.aes = FALSE
  ) +
  
  # Add the "pin" marker for population locations:
  # We fake a pin by overlaying a circle (point) + a triangle (shape 25).
  geom_point(
    data = beluga.slopes,
    aes(x = Longitude, y = Latitude + 0.6),
    size = 4, colour = "tan1",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = beluga.slopes,
    aes(x = Longitude, y = Latitude - 0.3),
    size = 3, fill = "tan1", colour = "tan1", shape = 25,
    inherit.aes = FALSE
  ) +
  
  # Add a beluga icon to the plot at a fixed location (a decorative annotation)
  annotation_custom(icon, xmin = -200, xmax = -110, ymin = 15, ymax = 35) +
  labs(title = "a. Beluga GBIF occurrences") +
  theme(plot.title = element_text(size = 20))

# ------------------------------------------------------------
# Number of occurrence records through time
# ------------------------------------------------------------
# Count records per year (tally() is like count())
# Filter out missing years
yearly.obs <- beluga.clean %>%
  group_by(year) %>%
  tally() %>%
  ungroup() %>%
  filter(!is.na(year))

# Plot time series of occurrence counts
occurrences <- ggplot(yearly.obs, aes(x = year, y = n)) +
  geom_line(colour = "aquamarine3", linewidth = 1) +
  # Fill the area under the curve (visual emphasis)
  geom_area(fill = "aquamarine3") +
  labs(
    x = NULL,                               # remove x-axis label
    y = "Number of occurrences\n",
    title = "b. GBIF occurrences\n"
  ) +
  theme_classic()

# ------------------------------------------------------------
# Population trends (abundance through time + linear model fit)
# ------------------------------------------------------------

# Hudson Bay population (ID 13273)
beluga1 <- filter(beluga.pop, ID == "13273")

# NOTE: This line likely contains a bug:
# beluga.slopes$year probably does not exist as a slope output.
# You probably want the slope estimate (e.g., from beluga.slopes where term == "year")
# or beluga.slopes$slope / beluga.slopes$estimate depending on your object structure.
print(beluga.slopes$year[beluga.slopes$ID == "13273"])

hudson.bay <- ggplot(beluga1, aes(x = year, y = abundance)) +
  # Points with outline + fill
  geom_point(shape = 21, fill = "aquamarine3", size = 4) +
  # Linear model fit with confidence interval
  geom_smooth(method = "lm", colour = "aquamarine3", fill = "aquamarine3", alpha = 0.4) +
  labs(x = "", y = "Individuals\n", title = "c. Eastern Hudson Bay\n") +
  theme_classic()

# Cook Inlet stock population (ID 2191)
beluga2 <- filter(beluga.pop, ID == "2191")

cook.inlet <- ggplot(beluga2, aes(x = year, y = abundance)) +
  geom_point(shape = 21, fill = "aquamarine3", size = 4) +
  geom_smooth(method = "lm", colour = "aquamarine3", fill = "aquamarine3", alpha = 0.4) +
  labs(x = "", y = "", title = "d. Cook Inlet stock\n") +
  theme_classic()

# St. Lawrence estuary has three time series (IDs 1950, 4557, 4558)
beluga3 <- filter(beluga.pop, ID %in% c("1950", "4557", "4558"))

# Inspect abundances (debug/quick check)
beluga3$abundance

st.lawrence.est <- ggplot(beluga3, aes(x = year, y = abundance, shape = as.factor(ID))) +
  geom_point(fill = "aquamarine3", size = 4) +
  # Define shapes for the three IDs (to visually separate time series)
  scale_shape_manual(values = c(21, 23, 24)) +
  geom_smooth(method = "lm", colour = "aquamarine3", fill = "aquamarine3", alpha = 0.4) +
  labs(x = "", y = "", title = "e. St. Lawrence estuary\n") +
  theme_classic() +
  guides(shape = "none")   # hide legend for shapes (optional)



library(patchwork)

# Row 1: map + occurrences (map wider)
row1 <- beluga.map.final + occurrences +
  plot_layout(ncol = 2, widths = c(1.96, 1.04))

# Row 2: three population plots (first slightly wider for y-axis label)
row2 <- hudson.bay + cook.inlet + st.lawrence.est +
  plot_layout(ncol = 3, widths = c(1.1, 1, 1))

# Full panel: stack rows (give row2 a bit more height)
beluga.panel <- row1 / row2 +
  plot_layout(heights = c(0.9, 1.1))

beluga.panel
