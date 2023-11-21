source("./scripts/1_clean/clean tracks__shys__telonics.R")

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

if(exists("fit")) {
  rm(fit)
}

fit <- load_latest_rds("ssm_albatross.rds", ssm_export_dir)

# If no fit was loaded (fit is NULL), generate a new one and save it
if (is.null(fit)) {
  fit <- fit_ssm(d1,
                 time.step = 1,
                 vmax = 22.22,
                 control = ssm_control(verbose = 0))

  save_rds(fit, "ssm_albatross.rds", ssm_export_dir)
}

# Get locations from fit

d1_ssm <- grab(fit, what = "predicted")

# Calculate speed
d1_ssm <- d1_ssm %>%
  group_by(id) %>%
  mutate(speed = track_speed(lon, lat, date))

# Plot ssm tracks

# Overlay tfmp
p <- plot_tracks(d1_ssm, tfmp)

p1 <- p +
  geom_path(data = d1_ssm, aes(x = lon, y = lat, color = speed), lwd = .5) +
  scale_color_viridis(option = "inferno", guide = guide_colorbar(title = "Speed (m/s)")) +
  # scale_color_paletteer_c("ggthemes::Orange-Blue Diverging", guide = guide_colorbar(title = "Speed (m/s)")) +
  publication_theme() +
  facet_wrap(~id) +
  labs(
    x = "lon", y = "lat", title = "Shy Albatross from Mewstone (deployed Dec 2022)",
    subtitle = paste("last tx date", last_date(d1_ssm))
  )


p1

p <- plot_tracks(d1_ssm, tfmp, facet=FALSE)

p2 <- p +
  geom_path(data = d1_ssm, aes(x = lon, y = lat, color = speed), lwd = .5) +
  scale_color_viridis(option = "inferno", guide = guide_colorbar(title = "Speed (m/s)")) +
  # scale_color_paletteer_c("ggthemes::Orange-Blue Diverging", guide = guide_colorbar(title = "Speed (m/s)")) +
  publication_theme() +
  labs(
    x = "lon", y = "lat", title = "Shy Albatross from Mewstone (deployed Dec 2022)",
    subtitle = paste("last tx date", last_date(d1_ssm))
  )


p2


save_plot_results(p1, "albatross_telonics_individual.png", width = 15, height = 12)
save_plot_results(p2, "albatross_telonics.png", width = 15, height = 12)


# Animate
library(gganimate)
anim <- p1 +
  labs(subtitle = "Date: {frame_along}") +
  transition_reveal(date)

animate(anim,  width = 1500, height = 1200)
save_animation_results('shy albatross-telonics.gif')
