
load (file = "Lesson_4/beluga_data.rdata")
ls()

beluga <- occ_search(scientificName = "Delphinapterus leucas", hasCoordinate = TRUE, limit = 100, return = "data")
head(beluga$data)


# Clean the data based on needed variables
beluga_sim <- beluga$data %>%
 dplyr::select(key, name, decimalLatitude, decimalLongitude, year, individualCount, country)
# Clean the data based on needed variables

View(beluga_PLD)


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


# Convert year column from strings like "X2010" -> 2010
beluga_pop$year <- parse_number(beluga_pop$year)

beluga_simp <- beluga$data %>%
  dplyr::select(key, name, decimalLatitude, decimalLongitude,
                year, individualCount, country)


icon <-readPNG("Lesson_4/beluga_icon.png")

#rasterize the image
icon <- rasterGrob(icon, interpolate = TRUE)


beluga_map <- ggplot(beluga_simp, aes(x = decimalLongitude, y = decimalLatitude)) +
    borders(
      "worldHires",  #worldHires 
      ylim   = c(40, 100),
      colour = "gray40",
      fill   = "gray40",
      size   = 0.3
    ) +
    theme_map() +
    annotation_custom(icon, xmin = -200, xmax = -110, ymin = 15, ymax = 35) +
    geom_point(alpha = 0.5, size = 0.5, colour = "aquamarine3")

ggplotly(beluga_map, tooltip = "all")

?ggplotly


