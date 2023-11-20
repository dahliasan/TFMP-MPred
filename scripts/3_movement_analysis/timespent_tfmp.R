# Calculate time spent inside and outside the TFMP

rm(list = ls())

# Load required packages
source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/utils.R")
library(sf)


# Load data
tracks <- load_latest_rds("./all_species_tracks.rds", "./data/exports/") %>% 
  dplyr::select(-trip_max_distance)

# Convert tracks to a spatial object
tracks_sf <- st_as_sf(tracks, coords = c("lon", "lat"), crs = 4326)

# Transform the coordinate system of tracks to match that of tfmp
tracks_sf <- st_transform(tracks_sf, st_crs(tfmp)$epsg)

# Calculate the intersection of the tracks and tfmp
tracks_tfmp <- st_join(tracks_sf, tfmp, join = st_intersects)

# Calculate the time spent inside and outside the tfmp for each species
time_spent <- tracks_tfmp %>%
  as_tibble() %>% 
  arrange(id, species, date) %>% 
  group_by(id, species) %>% 
  mutate(in_tfmp = ifelse(is.na(OBJECTID), "outside", "inside"),
         dur = difftime(lead(date), date, units = "hours") %>% as.numeric()) %>%
  group_by(id, species, in_tfmp) %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    time_spent_h = sum(dur, na.rm = TRUE)
    )%>% 
  pivot_wider(names_from = in_tfmp, values_from = time_spent_h, values_fill = 0) %>% 
  group_by(id, species) %>% 
  summarise(min_date = min(min_date), 
            max_date = max(max_date), 
            inside = max(inside),
            outside = max(outside), 
            prop_inside = inside / (inside + outside))

time_spent


# Print the result
print(time_spent)

time_spent%>% 
  group_by(species) %>% 
  summarise(prop_inside_mean = mean(prop_inside),
            prop_inside_sd = sd(prop_inside),
            prop_inside_min = min(prop_inside),
            prop_inside_max = max(prop_inside))


p1 <- time_spent %>% 
  mutate(group = paste(month(min_date), species, sep = "_")) %>% 
  ggplot(aes(x = min_date, y = prop_inside, color = species, group = group)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.3) + 
  theme_light() + 
  labs(y = "Proportion Time Inside", x = "Start Date", title = "Proportion Time Spent in TFMP")
  
p1
# save_plot_results(p1, "proportion_time_in_tfmp_species.png")

# Add a new variable in_tfmp in tracks_tfmp dataframe to differentiate inside and outside
tracks_tfmp$in_tfmp <- ifelse(is.na(tracks_tfmp$OBJECTID), "Outside", "Inside")

# Plot the tfmp polygons
ggplot() +
  geom_sf(data = tfmp, fill = "lightblue", color = "darkblue") +
  geom_sf(data = tracks_tfmp, aes(color = in_tfmp), size = 0.5) +
  theme_minimal() +
  labs(title = "Animal Tracks within TFMP Areas", x = "Longitude", y = "Latitude") +
  
  theme(legend.position = "bottom")
