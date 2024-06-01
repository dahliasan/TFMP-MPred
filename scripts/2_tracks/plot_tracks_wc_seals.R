# plot tracks

source("scripts/1_clean/clean_tracks_wc_furseals.R")

df_names <- c("juv", "adult")
output_list <- list()

# loop through each age class
for (df in df_names) {
  message("start ", df)

  d1 <- get(df)


  # Extract basic trip metrics
  message("extract trip metrics")

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

  output_list[[df]]$metric <- metric

  # Filter and smooth tracks
  # Prepare data for ssm
  message("filter and smooth tracks")

  d1$lc[d1$type == "FastGPS"] <- "G"

  if(exists("fit")) {
    rm(fit)
  }

  fit <- load_latest_rds(paste0("ssm_", df, ".rds"), ssm_export_dir, force_fit = TRUE)

  # If no fit was loaded (fit is NULL), generate a new one and save it
  if (is.null(fit)) {
    fit <- fit_ssm(d1,
                   time.step = 1,
                   vmax = 8,
                   control = ssm_control(verbose = 0)
    )
    save_rds(fit, paste0("ssm_", df, ".rds"), ssm_export_dir)
  }


  d1_ssm <- grab(fit, what = "predicted")

  d1_ssm <- left_join(d1_ssm, furseals)

  d1_ssm <- d1_ssm %>%
    group_by(id) %>%
    mutate(speed = track_speed(lon, lat, date))

  # save ssm as rds
  message("saving ssm tracks")
  save(d1_ssm, file = paste0(ssm_export_dir, df, "_ssm.RData"))


  # Plot fur seal tracks
  # Add demographic data to tracks
  message("generating track plot")

  # use switch to change the plot title name depending on the age class
  plot_title <- switch(df,
    "juv" = "Juvenile fur seals",
    "adult" = "Adult fur seals"
  )

  # Overlay tfmp
  p <- plot_tracks(d1_ssm, tfmp)

  p1 <- p +
    geom_path(data = d1_ssm, aes(x = lon, y = lat, color = speed), lwd = .7) +
    scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
      guide = guide_colorbar(title = "Speed (m/s)")
    ) +
    publication_theme() +
    facet_wrap(species ~ id, ncol = 4) +
    labs(
      x = "lon", y = "lat", title = plot_title,
      subtitle = paste("last tx date", last_date(d1_ssm))
    )

  output_list[[df]]$plot <- p1

  p <- plot_tracks(d1_ssm, tfmp, facet = FALSE)

  p2 <- p +
    geom_path(data = d1_ssm, aes(x = lon, y = lat, color = speed), lwd = .7) +
    scale_color_paletteer_c("ggthemes::Orange-Blue Diverging",
                            guide = guide_colorbar(title = "Speed (m/s)")
    ) +
    publication_theme() +
    facet_wrap(~species)
    labs(
      x = "lon", y = "lat", title = plot_title,
      subtitle = paste("last tx date", last_date(d1_ssm))
    )

  output_list[[df]]$plot2 <- p2

  message("success!")
}



save_plot_results(output_list$adult$plot, "female_furseals_individual.png", width = 14, height = 12)
save_plot_results(output_list$juv$plot, "juvenile_furseals_individual.png", width = 14, height = 12)
save_plot_results(output_list$adult$plot2, "female_furseals.png", width = 14, height = 12)
save_plot_results(output_list$juv$plot2, "juvenile_furseals.png", width = 14, height = 12)



# Animate
# anim <- animate_tracks(d1_ssm) +
#   ggtitle("Juvenile Furseal Tracks from SW Tas")


anim <- output_list$adult$plot +
  labs(subtitle = "Date: {frame_along}") +
  transition_reveal(date)

# animate(anim, fps = 10, nframes = 365, width = 5, height = 5, units = "in", res = 150) # to create smooth animation
animate(anim,  width = 700, height = 700)
save_animation_results('female_anim.gif')

anim <- output_list$juv$plot +
  labs(subtitle = "Date: {frame_along}") +
  transition_reveal(date)
animate(anim,  width = 700, height = 700)
save_animation_results('juv_anim.gif')
