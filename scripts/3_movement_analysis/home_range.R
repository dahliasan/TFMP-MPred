# Home range estimation
rm(list = ls())

# Load required packages
source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/utils.R")

library(sf)
library(adehabitatHR)
library(sp)

# Load data
tracks <- load_latest_rds("./all_species_tracks.rds", "./data/exports/") %>% 
  dplyr::select(-trip_max_distance)

# Count the number of locations for each individual
num_locations <- table(tracks$id)

# Get the IDs of individuals with 10 or more locations
ids_10plus <- names(num_locations[num_locations >= 10])

# Convert tracks to a spatial object
tracks_sp <- tracks %>% filter(id %in% ids_10plus)
coordinates(tracks_sp) <- ~lon+lat

head(as.data.frame(tracks_sp))

# Calculate the kernel density estimation for each individual
ud <- kernelUD(tracks_sp[,1])

## The UD of the four animals
image(ud)

## Calculation of the 95 percent home range

for(i in seq_along(ud)) {
  print(i)
  ver <- getverticeshr(ud[[i]], 95)
}





# Plot KDE ----------------------------------------------------------------


# Calculate the contour lines for each individual
contour_list <- lapply(kde_list, function(x) getverticeshr(x, percent = 95))

# Convert the contour lines to sf objects
contour_sf_list <- lapply(contour_list, st_as_sf)

# Combine the sf objects into one data frame
contour_sf <- do.call(rbind, contour_sf_list)

# Add an 'id' column to match the 'id' column in the tracks data
contour_sf$id <- rep(names(contour_sf_list), sapply(contour_sf_list, nrow))

# Plot the contour lines
ggplot() +
  geom_sf(data = contour_sf, aes(fill = id), color = NA) +
  geom_sf(data = tracks_utm, aes(color = id), size = 0.5) +
  theme_minimal() +
  labs(title = "Habitat Utility Density by Individual", x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom")

