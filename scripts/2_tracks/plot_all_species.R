
# Plot all species map

source("./scripts/load_packages_for_tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/ids.R")
source("./scripts/utils.R")

locs <- load_latest_rds(filename = "all_species_tracks.rds")


# Create breeding stage grouping ------------------------------------------
# For seals, we want to group by age_group, for birds we want to group by breeding_stage

locs <- locs %>% 
  mutate(facet_group = case_when(
    species %in% c("short-tailed shearwater", "shy albatross") ~ paste(species, "-", breeding_stage),
    species %in% c("LNFS", "AUSFS") ~ paste("fur seals -", age_group)
  ))


# Plot all species --------------------------------------------------------

locs_local <- locs %>% filter(!trip_type %in% "long")

locs_local_sf <- locs_local %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)

plot_data_split <- locs_local_sf %>% group_by(facet_group) %>% group_split()

plot_list <- plot_data_split %>% 
  purrr::map(
    function(x) {
      p <- x %>% 
        ggplot() + 
        geom_sf(data = se_network, colour = 'firebrick1', fill = "firebrick1", linewidth = .1, alpha = .1) +
        geom_sf(data = oz) + 
        geom_path(aes(x = lon, y = lat, group = id, color = species), 
                  lwd = .5,
                  na.rm = TRUE) +
        get_coord_lims(locs_local_sf) +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          axis.title = element_blank(),
          # set background colour
          panel.background = element_rect(fill = "#fefefe"),
        ) + 
        scale_colour_manual(values = c("black", "dodgerblue3")) +
        facet_wrap(~facet_group) + 
        labs(caption = paste0("N=", n_distinct(x$id))) 
      
      # if facet_group does not contain fur seals remove legend
      if (!grepl("fur seals", unique(x$facet_group))) {
        p <- p + theme(legend.position = "none")
      }
      
      return(p)
    }
  )

plot_list[[2]]

p1 <- ggpubr::ggarrange(plotlist = plot_list, align = "hv", ncol = 2, nrow = 3)

save_plot_results(p1, "all species.png", width = 8, height = 10, bg = "white")


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
 


# Plot for BLS8 Poster ----------------------------------------------------




p <- plot_tracks(plot_data, facet = FALSE, show_bathy_contour = FALSE, x_buffer=2, y_buffer = 2)

p1 <- p + 
  geom_path(data = plot_data,
            aes(x = lon, y = lat, group = id), color = "#DF2935") +
  geom_sf(data = tfmp, fill = NA, color = '#F5F3BB', alpha = 1, lwd=.5) + 
  publication_theme() + 
  facet_wrap(~species, ncol=4) 

p1 <- add_map_limits(p1, plot_data, type="lims", x_buffer=2, y_buffer=2)

p1 <- p1 + 
  # remove legends
  theme(legend.position = "none")  +
  labs(x = "Longitude", y = "Latitude", title = "All Species", caption = "STSW antarctic trips excluded")


p1

save_plot_results(p1, "all species (for bls8 poster).png", width = 12, height = 12)
