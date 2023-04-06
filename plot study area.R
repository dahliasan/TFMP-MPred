source('load packages__tracks.R')


# Get Tasmania shapefile from the 'rnaturalearth' package
tasmania <- ne_states(country = "australia", returnclass = "sf") %>%
  dplyr::filter(name == "Tasmania")

# Load shapefile data for the Tasman Fracture
shp_file <- list.files('./data', recursive = T, pattern='SouthEastAMPNetwork\\.shp$', full.names = T)
se_network <- st_read(shp_file)
tfmp <- se_network %>% dplyr::filter(RESNAME == 'Tasman Fracture')

# Define padding values
x_padding <- 0.5
y_padding <- 0.5

# Get bounding box of TFMP and add padding
bbox <- st_bbox(tfmp)
xmin <- bbox[1] - x_padding
xmax <- bbox[3] + x_padding
ymin <- bbox[2] - y_padding
ymax <- bbox[4] + y_padding
bbox <- st_bbox(c(xmin, xmax, ymax, ymin))
                
# Set the CRS for the bounding box and create an sfc object
st_crs(bbox) <- st_crs(tasmania)
bbox_sfc <- st_as_sfc(bbox)
st_crs(bbox_sfc) <- st_crs(tasmania)

# Define palette
paletteer_d("ggthemes::Classic_Color_Blind")

# Create a map of Tasmania with the boxed area
p1 <- ggplot() +
  geom_sf(data = tasmania, fill = "#CFCFCFFF", color = "black", lwd=.25) +
  geom_sf(data = bbox_sfc, fill = NA, color = "#C85200FF", lwd=1) +
  theme_minimal()

# Create a map of the boxed area with Tasman Fracture boundaries
p2 <- ggplot() +
  geom_sf(data = tasmania, fill = "#CFCFCFFF", color = "black", lwd=.25) +
  geom_sf(data = bbox_sfc, fill = NA, color = "#C85200FF", lwd=1) +
  geom_sf(data = tfmp, aes(fill = RESNAME), color = "#C85200FF", lwd=0.5) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  scale_fill_manual(name="", values = "#FFBC79FF", labels = "TFMP") +
  theme_minimal()

# Combine p1 and p2 side by side
combined_plot <- p1 + p2 + plot_layout(ncol = 2)


# Save plot as PNG file
ggsave("tfmp_study_area.png", plot = combined_plot, units='in',  width = 9, height = 9,   dpi = 300)
