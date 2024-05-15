source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")

# Load shearwater data
files <- dir("./data/original/shearwaters/catlogger/", full.names = TRUE)
d <- read_csv(files)

# Clean data
d <- janitor::clean_names(d)
d <- d %>%
  mutate(datetime = mdy_hms(paste(date, time))) %>%
  select(-date, -time) %>%
  rename(lat = latitude, lon = longitude, date = datetime) %>%
  filter(lon > 0)
d$id <- as.factor(d$id)

# calculate speed
d <- d %>%
  group_by(id) %>%
  mutate(speed = track_speed(lon, lat, date))

stsw_gpsc <- d

save(stsw_gpsc, file = paste0(clean_dir, "stsw_gpsc.RData"))

# Plot tracks


p <- plot_tracks(d, tfmp)


p1 <- p +
  geom_path(
    data = d,
    aes(x = lon, y = lat, color = speed),
    lwd = .75
  ) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
                          guide = guide_colorbar(title = "Speed (km/h)")
  ) +
  publication_theme() +
  facet_wrap(~id) +
  labs(
    x = "lon", y = "lat", title = "Short-tailed shearwater - December",
    subtitle = paste("last tx date", last_date(d))
  )


save_plot_results(p1, 'stsw-catlogger.png')


animate_and_save(p1, filename = 'stsw-catlogger.gif')




