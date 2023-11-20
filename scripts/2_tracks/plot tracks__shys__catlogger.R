source("./scripts/1_clean/clean tracks__shys__catlogger.R")

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

# Filter and smooth tracks

if(exists("fit")) {
  rm(fit)
}

fit <- load_latest_rds("ssm_albatross-catlogger.rds", ssm_export_dir)

# If no fit was loaded (fit is NULL), generate a new one and save it
if (is.null(fit)) {
  fit <- fit_ssm(d1,
                 time.step = 6,
                 vmax = 32,
                 control = ssm_control(verbose = 0))

  save_rds(fit, "ssm_albatross-catlogger.rds", ssm_export_dir)
}

# Get locations from fit

d1_ssm <- grab(fit, what = "predicted")

d1_ssm <- left_join(d1_ssm, d1)

d1_ssm <- d1_ssm %>%
  group_by(id) %>%
  mutate(speed = track_speed(lon, lat, date))

# save ssm as rds
message("saving ssm tracks")
save(d1_ssm, file = paste0(ssm_export_dir, 'shy albatross-catlogger', "_ssm.RData"))


# Plot ssm tracks
# Overlay tfmp


p <- plot_tracks(d1_ssm, tfmp)

p1 <- p +
  geom_path(
    data = d1_ssm,
    aes(x = lon, y = lat, color = speed),
    lwd = .75
  ) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
                          guide = guide_colorbar(title = "Speed (km/h)")
  ) +
  publication_theme() +
  facet_wrap(~id) +
  labs(
    x = "lon", y = "lat", title = "Albatross from Mewstone (deployed Oct 2022)",
    subtitle = paste("last tx date", last_date(d1_ssm))
  )


p <- plot_tracks(d1_ssm, tfmp, facet=FALSE)

p2 <- p +
  geom_path(
    data = d1_ssm,
    aes(x = lon, y = lat, color = speed),
    lwd = .75
  ) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
                          guide = guide_colorbar(title = "Speed (km/h)")
  ) +
  publication_theme() +
  labs(
    x = "lon", y = "lat", title = "Albatross from Mewstone (deployed Oct 2022)",
    subtitle = paste("last tx date", last_date(d1_ssm))
  )

save_plot_results(p1, 'shy albatross-catlogger-oct 2022_individual.png')
save_plot_results(p2, 'shy albatross-catlogger-oct 2022.png')


animate_and_save(p1, filename = 'shy_albatross-catlogger.gif')
