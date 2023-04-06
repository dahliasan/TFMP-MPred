# Process and plot ecotone shearwater tracks
source("load packages__tracks.R")
source("tfmp_functions.R")
source("utils.R")


# set the working directory to the folder containing the data files
dir <- "./data/shearwaters/ecotone"

# get deployment info
file <- list.files(path = dir, pattern = "^URIA.*\\.csv$", full.names = TRUE)
deploy <- read_csv(file, skip = 2) %>%
  janitor::clean_names() %>%
  mutate(date = dmy(date), tag_id = str_replace(tag_id, "-", ""))

# create a vector of file names in the folder
files <- list.files(path = dir, pattern = "^SHE.*\\.csv$", full.names = TRUE)

# read in and combine all files into one data frame
gps <- files %>%
  purrr::map(~ read_delim(.x)) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  # clean the data
  mutate(date = ymd_hms(date_2)) %>%
  rename(
    lat = latitude,
    lon = longitude,
    tag_id = logger_id
  )


# select columns where not all values are NA
gps <- gps %>%
  select_if(~ !all(is.na(.))) %>%
  select(-c(year, month, day, hour, minute, second, date_2))

# Process data by matching ids to deployment dates
gps_clean <- process_ecotone_data(deploy, gps)
gps_clean <- gps_clean %>% filter(!is.na(lat))

# plot lat vs date of tracks by id
gps_clean %>% ggplot(aes(x = date, y = lat, color = tag_id)) +
  geom_point() +
  labs(x = "Date", y = "Latitude")

# save coordinates of maatsuyker -43.6679° S, 146.3122° E
maatsuyker <- c(-43.6679, 146.3122)

# calculate distance from maatsuyker (in metres)
gps_clean <- gps_clean %>%
  mutate(
    distance_from_maatsuyker =
      track_distance_to(lon, lat, maatsuyker[2], maatsuyker[1])
  )

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




# add new column for when bird is at the colony (within 1km) and not flying
gps_clean <- gps_clean %>%
  mutate(
    at_colony = ifelse(
      distance_from_maatsuyker < 10000,
      TRUE,
      FALSE
    )
  )

# create trip id column by cumulatively summing at_colony by id
gps_clean <- gps_clean %>%
  group_by(id) %>%
  mutate(trip_id = cumsum(at_colony))

gps_clean %>%
  select(
    id,
    tag_id,
    date,
    at_colony,
    trip_id,
    distance_from_maatsuyker,
    travel_speed_km_h
  ) %>%
  View()


table(gps_clean$tag_id, gps_clean$trip_id)

# remove trip ids with less than 10 locations and restart sequence from 1
gps_clean <- gps_clean %>%
  group_by(id, trip_id) %>%
  filter(n() > 10) %>%
  ungroup() %>%
  group_by(id, tag_id) %>%
  mutate(trip_id = cumsum(at_colony))


# calcule trip max distance
gps_clean <- gps_clean %>%
  group_by(id, trip_id) %>%
  mutate(trip_max_distance = max(distance_from_maatsuyker))

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

# plot short trip tracks
gps_filtered_ids <- gps_clean %>% filter(trip_type == "short")

d <- gps_filtered_ids
d
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
  labs(x = "lon", y = "lat")
p1
save_plot_results(p1, "shearwater_ecotone_1.png")

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
  labs(x = "lon", y = "lat")
p2
save_plot_results(p2, "shearwater_ecotone_2.png")

# combine tracks and save
combined_plot <- p1 + p2 + plot_layout(ncol = 2, widths = c(1, 2))

# Save plot as PNG file
save_plot_results(combined_plot,
  "shearwater_ecotone_all.png",
  width = 16, height = 9
)
