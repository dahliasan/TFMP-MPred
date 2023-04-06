source("load packages__tracks.R")
source("tfmp_functions.R")
source("utils.R")


# Load wildlife computers data
d <- readWcLocationsCsv("./data/albatross", "-Locations\\.csv")

# Clean data from WC portal
d1 <- cleanWcLocationsData(d)

# Get actual deployment start and end dates
deploy <- shy_deployment %>%
  filter(logger_type == "Telonics Sat Tag") %>%
  select(ptt_id, deployment_date, retrieval_date) %>%
  rename(id = ptt_id) %>%
  mutate(
    deployment_date = force_tz(deployment_date, tzone = "Australia/Sydney"),
    retrieval_date = force_tz(retrieval_date, tzone = "Australia/Sydney"),
    id = id %>% as.character()
  )

d1 <- left_join(d1, deploy)
d1 <- d1 %>% filter(date >= deployment_date)

# Extract basic trip metrics
metric <- d1 %>%
  group_by(id) %>%
  mutate(
    distance = track_distance(lon, lat),
    angle = track_angle(lon, lat),
    turn = track_turn(lon, lat),
    bearing = track_bearing(lon, lat),
    duration = track_time(date),
    speed = track_speed(lon, lat, date)
  ) %>%
  select(id, date, lat, lon, lc, distance, angle, turn, bearing, duration, speed)

speeds <- metric$speed[metric$speed %>% is.finite()]

# Filter and smooth tracks
# Prepare data for ssm
d1 <- d1 %>% filter(!is.na(lc))

fit <- fit_ssm(d1,
  time.step = 6,
  vmax = 32,
  control = ssm_control(verbose = 0)
)

d1_ssm <- grab(fit, what = "predicted")

# Calculate speed
d1_ssm <- d1_ssm %>%
  group_by(id) %>%
  mutate(speed = track_speed(lon, lat, date))

# Plot ssm tracks

# Overlay tfmp
p <- plot_tracks(d1_ssm, tfmp)

# dark mode
# lab_dates <- pretty(d1_ssm$date)
#
# p +
#   scale_fill_viridis_c(option = "inferno")+
#   scale_color_viridis_c(option = "inferno", breaks = as.numeric(lab_dates), labels = format(lab_dates, format='%d %b'))+
#   theme(panel.background = element_rect(fill = "#C5CBD3"),
#         panel.grid = element_blank()) +
#   labs(title = 'Shy Albatross from Mewstone (deployed Dec 2022)')

p1 <- p +
  geom_path(data = d1_ssm, aes(x = lon, y = lat, color = speed), lwd = .5) +
  scale_color_viridis(option = "inferno", guide = guide_colorbar(title = "Speed (m/s)")) +
  # scale_color_paletteer_c("ggthemes::Orange-Blue Diverging", guide = guide_colorbar(title = "Speed (m/s)")) +
  publication_theme() +
  facet_wrap(~id) +
  labs(x = "lon", y = "lat", title = "Shy Albatross from Mewstone (deployed Dec 2022)")


p1


save_plot_results(p1, "albatross_telonics.png", width = 15, height = 12)



anim <- animate_tracks(d1_ssm, facet = TRUE, facet_scales = "free") +
  ggtitle("Shy Albatross from Mewstone (deployed Dec 2022")

animate(anim, fps = 10, nframes = 365, width = 5, height = 5, units = "in", res = 150)

anim_save("./exports/shys_20Feb2023.gif")
