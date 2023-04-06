source("load packages__tracks.R")
source("tfmp_functions.R")
source("ids.R")
source("utils.R")

# Load wildlife computers data
d <- readWcLocationsCsv("./data/fur seals/", "-\\d+-Locations\\.csv")

# Clean data from WC portal
d <- cleanWcLocationsData(d)

# Get juvenile / adult female data
d1 <- d %>% filter(id %in% juvenile_ids)
# d1 <- d %>% filter(!id %in% juvenile_ids)



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
# Prepare data for ssm
d1$lc[d1$type == "FastGPS"] <- "G"
d1 <- d1 %>% filter(!is.na(lc))

fit <- fit_ssm(d1,
  time.step = 6,
  vmax = 4,
  control = ssm_control(verbose = 0)
)

d1_ssm <- grab(fit, what = "predicted")


# Plot fur seal tracks
# Add demographic data to tracks
d1_ssm <- left_join(d1_ssm, furseals)

d1_ssm <- d1_ssm %>%
  group_by(id) %>%
  mutate(speed = track_speed(lon, lat, date))

# Overlay tfmp
p <- plot_tracks(d1_ssm, tfmp)

p1 <- p +
  geom_path(data = d1_ssm, aes(x = lon, y = lat, color = speed), lwd = .7) +
  scale_color_paletteer_c("ggthemes::Orange-Blue Diverging", guide = guide_colorbar(title = "Speed (m/s)")) +
  publication_theme() +
  facet_wrap(species ~ id, ncol = 4) +
  labs(x = "lon", y = "lat", title = "Juvenile Fur seals")

p1


# save_plot_results(p1, 'female_furseals.png', width = 14, height = 12)
save_plot_results(p1, "juvenile_furseals.png", width = 14, height = 12)


#
# anim <- animate_tracks(d1_ssm) +
#   ggtitle('Juvenile Furseal Tracks from SW Tas')
#
# animate(anim, fps = 10, nframes = 365, width = 5, height = 5, units = 'in', res = 150)
#
# anim_save('./exports/juvenile_furseals_anim.gif')
#
