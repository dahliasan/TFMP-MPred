
# Plot all species map

source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/ids.R")
source("./scripts/utils.R")


# Specify the directory containing the .rds files
dir_path <- "./data/exports/animotum ssm fits/2023-05-16/"

# List all .rds files in the directory
file_list <- list.files(path = dir_path, pattern = "*.rds", full.names = TRUE)

ssms <- purrr::map(file_list, function(file) {
  ssm <- read_rds(file)
  ssm <- grab(ssm, what = "predicted")

  # detect species
  if(str_detect(file, "adult")) {
    specie <- "adult female"
  }  else if(str_detect(file, "juv")) {
    specie <- "juvenile"
  } else if(str_detect(file, "albatross")) {
    specie <- "shy albatross"
  } else if(str_detect(file, "stsw")) {
    specie <- "short-tailed shearwater"
  }

  # detect fur seals species
  if(specie == "adult female" | specie == "juvenile") {
    # adult females detected
    ssm <- ssm %>%
      mutate(file = file) %>%
      dplyr::select(id, date, lon, lat)

    id <- furseals %>%
      filter(age_group == specie) %>%
      dplyr::select(id, species, age_group)

    ssm <- ssm %>% left_join(id)
    
    return(ssm)
  }

  ssm <- ssm %>%
    mutate(file = file, species = specie, age_group = "adult") %>%
    dplyr::select(id, date, lon, lat, species, age_group)

  return(ssm)
}) %>% 
  bind_rows()



# Load shearwater ---------------------------------------------------------

load("./data/exports/cleaned/stsw_gps_ecotone.RData")
load("./data/exports/cleaned/stsw_gpsc.RData")


shear <- stsw_gps %>%
  ungroup() %>%
  select(id, date, lat, lon, trip_max_distance) %>% 
  mutate(species = "short-tailed shearwater", 
         age_group = "adult", 
         trip_type = ifelse(
           trip_max_distance < 500000,
           "short",
           "long")
         )
  

shear_cat <- stsw_gpsc %>%
  ungroup() %>%
  select(id, date, lat, lon) %>% 
  mutate(species = "short-tailed shearwater", 
         age_group = "adult")


# Plot all species --------------------------------------------------------

all <- ssms %>% 
  bind_rows(shear) %>% 
  bind_rows(shear_cat)

all %>% group_by(species, age_group) %>% 
  summarise(n_individuals = unique(id) %>% length())

plot_data <- all %>% 
  filter(trip_type != "long" | is.na(trip_type))

p <- plot_tracks(plot_data, tfmp, facet = FALSE)

p1 <- p  +
  geom_path(data = plot_data, 
            aes(x = lon, y = lat, group = id, color = species), 
            lwd = .75,
            na.rm = TRUE) +
  publication_theme() +
  scale_color_brewer(na.value = NA, type = 'div', palette = 4) + 
  labs(x = "lon", y = "lat", title = "All Species")

save_plot_results(p1, "all species.png", width = 12, height = 12)


p1 <- p  +
  geom_path(data = plot_data, 
            aes(x = lon, y = lat, group = id), 
            lwd = .75,
            na.rm = TRUE, color = 'orangered') +
  publication_theme() +
  facet_wrap(~species) + 
  # scale_color_brewer(na.value = NA, type = 'div', palette = 4) + 
  labs(x = "lon", y = "lat", title = "All Species")

save_plot_results(p1, "all species_species.png", width = 12, height = 12)


p2 <-  p  +
  geom_path(data = plot_data, 
            aes(x = lon, y = lat, group = id), 
            lwd = .75,
            na.rm = TRUE,
            color = 'red') +
  facet_wrap(species~id) + 
  publication_theme() +
  scale_color_manual(na.value = NA) + 
  labs(x = "lon", y = "lat", title = "All Species", 
       caption = "STSW antarctic trips excluded. AUSFS = Australian fur seals; LNFS = long-nosed fur seals; TFMP = blue area")
  

save_plot_results(p2, "all species_individual.png", width = 20, height = 12) 

# Limit map around TFMP
# Calculate the bounding box of your tfmp object
bbox <- st_bbox(tfmp)

p1 <- p  +
  geom_path(data = plot_data, 
            aes(x = lon, y = lat, group = id), 
            lwd = .75,
            na.rm = TRUE,
            color = 'orangered') +
  publication_theme() +
  facet_wrap(~species) + 
  labs(x = "lon", y = "lat", title = "All Species (zoomed in to TFMP)") +
  coord_sf(xlim = c(bbox["xmin"] - 0.5, bbox["xmax"] + 0.5), 
           ylim = c(bbox["ymin"] - 0.5, bbox["ymax"] + 0.5)) # limit the map to the bounding box

save_plot_results(p1, "all species_tfmp.png", width = 10, height = 15)
 
