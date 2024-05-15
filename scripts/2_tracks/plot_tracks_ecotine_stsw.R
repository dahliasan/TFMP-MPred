# Process and plot ecotone shearwater tracks
source("./scripts/1_clean/clean tracks__stsw__ecotone.R")


# plot lat vs date of tracks by id
gps_clean %>% ggplot(aes(x = date, y = lat, color = tag_id)) +
  geom_point() +
  labs(x = "Date", y = "Latitude")

# plot distance from maatsuyker vs date of tracks by id
gps_clean %>% ggplot(aes(
  x = date,
  y = distance_from_maatsuyker,
  color = tag_id
)) +
  geom_point() +
  labs(x = "Date", y = "Distance from Maatsuyker (m)") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(id ~ tag_id, scales = "free") +
  lims(y = c(0, 60000))


# plot speed vs distance from maatsuyker
gps_clean %>% ggplot(aes(
  x = distance_from_maatsuyker,
  y = travel_speed_km_h,
  color = tag_id
)) +
  geom_path() +
  labs(x = "Distance from Maatsuyker (m)", y = "Speed (km/h)") +
  # use integers as breaks
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(id ~ tag_id)


# histogram of trip max distance
gps_clean %>%
  ggplot(aes(x = trip_max_distance)) +
  geom_histogram() +
  labs(x = "Trip max distance (m)", y = "Count") +
  # pretty x breaks
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


# classify short and long trips in new column
gps_clean <- gps_clean %>%
  mutate(
    trip_type = ifelse(
      trip_max_distance < 500000,
      "short",
      "long"
    )
  )


# Short trips -------------------------------------------------------------

# plot short trip tracks
gps_filtered_ids <- gps_clean %>% filter(trip_type == "short")

d <- gps_filtered_ids

p <- plot_tracks(d, tfmp)
p1 <- p +
  geom_path(
    data = d,
    aes(x = lon, y = lat, color = travel_speed_km_h),
    lwd = .75
  ) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
    guide = guide_colorbar(title = "Speed (km/h)")
  ) +
  publication_theme() +
  facet_wrap(id ~ tag_id) +
  labs(
    x = "lon", y = "lat", title = "stsw - short trips",
    subtitle = paste("last tx date", last_date(d))
  )
p1

p <- plot_tracks(d, tfmp, facet = FALSE)
p2 <- p +
  geom_path(
    data = d,
    aes(x = lon, y = lat, color = travel_speed_km_h),
    lwd = .75
  ) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
                          guide = guide_colorbar(title = "Speed (km/h)")
  ) +
  publication_theme() +
  labs(
    x = "lon", y = "lat", title = "stsw - short trips",
    subtitle = paste("last tx date", last_date(d))
  )

p2

save_plot_results(p1, "shearwater_ecotone_1_individual.png")
save_plot_results(p2, "shearwater_ecotone_1.png")


# Long trips --------------------------------------------------------------

# plot long trip tracks
gps_filtered_ids <- gps_clean %>% filter(trip_type == "long")
d <- gps_filtered_ids
p <- plot_tracks(d, tfmp)
p2 <- p +
  geom_path(
    data = d,
    aes(x = lon, y = lat, color = travel_speed_km_h),
    lwd = .75
  ) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
    guide = guide_colorbar(title = "Speed (km/h)")
  ) +
  publication_theme() +
  facet_wrap(id ~ tag_id) +
  labs(
    x = "lon", y = "lat", title = "stsw - long trips",
    subtitle = paste("last tx date", last_date(d))
  )
p2

p <- plot_tracks(d, tfmp, facet=FALSE)
p3 <- p +
  geom_path(
    data = d,
    aes(x = lon, y = lat, color = travel_speed_km_h),
    lwd = .75
  ) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
                          guide = guide_colorbar(title = "Speed (km/h)")
  ) +
  publication_theme() +
  labs(
    x = "lon", y = "lat", title = "stsw - long trips",
    subtitle = paste("last tx date", last_date(d))
  )
p2

save_plot_results(p2, "shearwater_ecotone_2_individual.png")
save_plot_results(p3, "shearwater_ecotone_2.png")

# # combine tracks and save
# combined_plot <- p1 + p2 + plot_layout(ncol = 2, widths = c(1, 2))
#
# # Save plot as PNG file
# save_plot_results(combined_plot,
#   "shearwater_ecotone_all.png",
#   width = 16, height = 9
# )

anim <- p1 + 
  labs(subtitle = "Date: {frame_along}") +
  transition_reveal(date)
animate(anim,  width = 700, height = 700)
save_animation_results('stsw-short-trips.gif')

anim <- p2 + 
  labs(subtitle = "Date: {frame_along}") +
  transition_reveal(date)
animate(anim,  width = 700, height = 700)
save_animation_results('stsw-long-trips.gif')
