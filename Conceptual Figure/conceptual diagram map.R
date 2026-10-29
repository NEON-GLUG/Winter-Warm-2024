



#### map without icons ####

# install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "remotes"))
# remotes::install_github("ropensci/rnaturalearthhires")  # needed for state/province lines

library(tidyverse)
library(sf)
library(rnaturalearth)

# --- Bounding box of the study area ---
bbox <- list(xmin = -95, xmax = -80, ymin = 40, ymax = 50)
pad  <- 0.2

# --- Settings ---
dot_size <- 6   # size of the black dots

# --- Load data ---
sites_all <- read.csv("./formatted data/ENSO warm winters study sites.csv") |>
  rename_with(\(x) str_to_lower(str_trim(x))) |>
  mutate(
    group = str_to_lower(str_trim(group)),
    group = recode(group, "aquatic bigoechemistry" = "aquatic biogeochemistry"),
    long  = -abs(long)
  )

# Sites without coordinates can't be plotted; list them so nothing vanishes silently
sites_all |> filter(is.na(lat) | is.na(long)) |> select(group, site) |> print()
sites <- sites_all |> drop_na(lat, long)

# --- Base map layers ---
states    <- ne_states(country = c("United States of America", "Canada"),
                       returnclass = "sf")
countries <- ne_countries(scale = "medium", returnclass = "sf")
lakes     <- ne_download(scale = 10, type = "lakes", category = "physical",
                         returnclass = "sf")

water_col <- "#d6e6f4"

bbox_sf <- st_bbox(c(xmin = bbox$xmin, xmax = bbox$xmax,
                     ymin = bbox$ymin, ymax = bbox$ymax),
                   crs = st_crs(countries))
countries_in <- suppressWarnings(st_crop(st_make_valid(countries), bbox_sf))

# --- Plot ---
p <- ggplot() +
  geom_sf(data = countries, fill = "gray85", color = "black", linewidth = 0.4) +
  geom_sf(data = countries_in, fill = "#DDF3DC", color = NA) +
  geom_sf(data = states, fill = NA, color = "gray50", linewidth = 1) +
  geom_sf(data = lakes, fill = water_col, color = "#6a9ac4", linewidth = 0.3) +
  annotate("rect",
           xmin = bbox$xmin, xmax = bbox$xmax,
           ymin = bbox$ymin, ymax = bbox$ymax,
           fill = NA, color = "black", linewidth = 1.1, linetype = "dashed") +
  # black dot at every site location
  geom_point(data = distinct(sites, long, lat),
             aes(x = long, y = lat),
             shape = 21, fill = "black", color = "white",
             size = dot_size, stroke = 0.5) +
  coord_sf(xlim = c(bbox$xmin - pad, bbox$xmax + pad),
           ylim = c(bbox$ymin - pad, bbox$ymax + pad),
           expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.background = element_rect(fill = water_col),
        panel.grid = element_line(color = "gray60", linewidth = 0.2, linetype = "dotted"),
        axis.text = element_text(size = 16))

p
ggsave("enso_study_sites_map.png", p, width = 9, height = 7, dpi = 600)










