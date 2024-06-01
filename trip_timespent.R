# Calculate time spent inside and outside the TFMP

rm(list = ls())

# Load required packages
source("./scripts/load_packages_for_tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/utils.R")
# library(basf)
library(sp)
library(raster)
library(readxl)

# Load data
locs <- load_latest_rds(filename = "all_species_tracks.rds") %>% 
  dplyr::select(-trip_max_distance)

metadata <- read_excel("./tfmp_deployment_metadata_base_copy.xlsx", sheet = "deployment summary")

# Add colony location to locs
locs <- locs %>% 
  left_join(metadata %>% dplyr::select(id, colony_lon, colony_lat)) %>% 
  # Remove trip_type == "long" trips
  filter(!trip_type %in% "long") %>% 
  # Calculate distance to colony
  mutate(dist_to_col = track_distance_to(lon, lat, colony_lon, colony_lat)) %>% 
  # Create new ID for trip object
  mutate(id_clean = janitor::make_clean_names(id, allow_dupes = T))

# set projections
# TFMP: -43.79534447981563, 145.728070274819
proj_laea <- "+proj=laea +lat_0=-43.795 +lon_0=145.728 +datum=WGS84 +ellps=WGS84 +no_defs"
proj_base <- "+proj=longlat +datum=WGS84"

locs_filtered <- locs %>% 
  filter(dist_to_col >= 25000)

# plot_locs_sf(loc_sf, buffer = 0) + 
#   geom_point(data = loc_sf, aes(x = colony_lon, y = colony_lat), color = "red") +
#   facet_wrap(species~id)
# 
# loc_sf %>% 
#   ggplot(aes(x = date, y = dist_to_col)) +
#   geom_path(linewidth = .5) +
#   facet_wrap(species~id, scales = "free")


# Transform to Lambert Azimuthal Equal-Area projection
# loc_sf <- st_transform(loc_sf, crs = proj_laea)

# create trip object and run a speed filter (just in case)
tr <- trip(locs_filtered %>% dplyr::select(lon, lat, date, id_clean))

# set up the grid in kms
grid <- makeGridTopology(tr, cellsize = c(.25, .25)) 

tripGrid(tr, grid = grid) %>% plot()

# calc time spent in each seal for each trip 
# (mikes clever method)
split_trip <- split(tr, tr[[getTORnames(tr)[2]]])
split_grid <- lapply(split_trip, function(x) raster(tripGrid(x, grid = grid)))
time_per_cell <- stack(split_grid)/3600 # make into hours 

# Remove NA cells
time_per_cell <- calc(time_per_cell, function(x) ifelse(x > 0, x, NA_real_))

plot(time_per_cell)

library(stars)
stars_object <- st_as_stars(time_per_cell)

# Convert stars object to sf object with polygons
time_per_cell_sf <- st_as_sf(stars_object, as_points = F, merge = F, long = T)
time_per_cell_sf <- time_per_cell_sf %>% 
  mutate(id_clean = band, time = a16) %>% 
  dplyr::select(id_clean, time) %>% 
  group_by(id_clean) %>% 
  mutate(time_perc = time/sum(time, na.rm = T))

# Add metadata to time_per_cell_sf
m <- locs %>% 
  group_by(id_clean, id, species, age_group, breeding_stage) %>% 
  summarise()

# Replace NA in breeding_stage with ''
m$breeding_stage[is.na(m$breeding_stage)] <-''
 
time_per_cell_sf <- time_per_cell_sf %>%
  left_join(m)


# transform crs to base
# time_per_cell_sf <- st_transform(time_per_cell_sf, crs = proj_base)

tpc_split <- time_per_cell_sf %>% 
  group_by(species, breeding_stage, id_clean) %>% 
  group_split()

oz <- ne_countries(scale= 10,country = "Australia", returnclass = "sf")

buffer <- 0
bbox <- st_bbox(tpc_split[[1]])
xlim <- c(bbox$xmin - buffer, bbox$xmax + buffer)
ylim <- c(bbox$ymin - buffer, bbox$ymax + buffer)



plot_list <- purrr::map(tpc_split, function(x) {
  ggplot() +
    geom_sf(aes(fill = time), colour = NA, data = x) + 
    geom_sf(data = se_network, color = 'firebrick1', fill = NA, lwd = .5) + 
    geom_sf(data = oz, fill = NA, colour = "black") +
    scale_fill_viridis_c(na.value = 'transparent', name = 'time (h)') +
    labs(caption = paste(x$species[1], x$age_group, x$breeding_stage, x$id[1])) +
    ggpubr::clean_theme() + 
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), grid) +
    coord_sf(xlim = xlim, ylim = ylim) 
})


plot_list[[10]]

p <- ggpubr::ggarrange(plotlist = plot_list, align = "hv")
# Save 
save_plot_results(p, "time_per_cell.png", width = 30, height = 20, bg = 'white')



# Time spent pooled -------------------------------------------------------
# Identify the high-use areas for each animal and add them to the map
# high-use = top 75% quantile

time_per_cell_sf <- time_per_cell_sf %>% 
  group_by(id) %>% 
  mutate(q = quantile(time, .95, na.rm = T))  %>%
  mutate(is_core = ifelse(time > q, T, F))
  

time_per_cell_sf %>% 
  filter(is_core) %>%
  ggplot() + 
  geom_sf(aes(fill = is_core), color = NA, fill = 'blue') +
  geom_sf(data = se_network, color = 'firebrick1', fill = NA, lwd = .5) + 
  geom_sf(data = oz, fill = NA, colour = "black") + 
  get_coord_lims(time_per_cell_sf)

# Measure overlap of core areas
# Calculate the number of core locations that fall 

# Calculate the number of core locations that fall within each grid cell
core_sf <- time_per_cell_sf %>% 
  filter(is_core) 

grid <- st_make_grid(core_sf, cellsize = c(.25, .25), square = TRUE) %>%
  st_as_sf() %>%
  mutate(cell = 1:nrow(.))

# Perform intersection
intersection <- st_intersection(grid, core_sf)

# Count overlaps
overlap_counts <- intersection %>%
  group_by(cell) %>%
  summarize(overlap_count = n())

overlap_counts

p2 <- overlap_counts %>% 
  ggplot() + 
  geom_sf(data = oz, fill = NA, colour = "black") +
  geom_sf(data = se_network, colour = 'firebrick1', fill = "firebrick1", linewidth = .1, alpha = .1) +
  mytheme + 
  geom_sf(aes(fill = overlap_count), color = NA) + 
  scale_fill_viridis(name = "overlap count") + 
  get_coord_lims(time_per_cell_sf) + 
  labs(caption = "Number of core locations (> 0.95 quantile) in each grid cell")

save_plot_results(p2, "core_area_overlap.png", width = 8, height = 8, bg = 'white')
