# Define function
animate_tracks <- function(df, facet = FALSE, facet_scales = "fixed") {
  # Check if required packages are installed and load them
  required_pkgs <- c("dplyr", "ggplot2", "gganimate", "transformr", "lubridate", "animation", "av", "mapdata")
  missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()]
  if (length(missing_pkgs) > 0) {
    # Some packages are missing, install them
    install.packages(missing_pkgs)
  }
  lapply(required_pkgs, require, character.only = TRUE)

  # Check if input columns exist in data frame
  tryCatch(
    {
      c("id", "date", "lat", "lon") %in% colnames(df) %>% all()
    },
    error = function(e) {
      # Print error message if columns do not exist
      stop(paste(
        "Error: One or more input columns do not exist in the data frame.\n",
        "Make sure the column names are spelled correctly and case-sensitive."
      ))
    }
  )

  # Convert date variable to Date class
  if (!is.POSIXct(df$date)) {
    df$date <- ymd_hms(df$date)
  }

  # Remove NAs
  df <- df %>% filter(!is.na(lat))

  # Get world map
  map <- map_data("world")

  # Create ggplot
  p <- ggplot(df, aes(lon, lat, group = id)) +
    geom_map(map_id = "Australia", map = map, fill = "grey90") +
    theme_bw()

  if (facet) {
    p <- p + geom_path() +
      geom_point() +
      facet_wrap(~id, scales = facet_scales)
  } else {
    p <- p +
      geom_path(aes(color = id)) +
      geom_point(aes(color = id))
  }

  # Add date as subtitle
  p <- p + labs(subtitle = "Date: {frame_along}")

  # Animate ggplot
  p <- p + transition_reveal(date)

  # Plot ggplot
  p
  # animate(p)
}

animate_and_save <- function(plot, filename, ...) {
  
  anim <- plot + 
    labs(subtitle = "Date: {frame_along}") +
    transition_reveal(date)
  anim_render <- animate(anim, width = 700, height = 700, ...)
  save_animation_results(filename, animation = anim_render)
  
  return(anim_render)
  
}

plot_tracks <- function(df, shapefile, facet = TRUE, facet_scales = "fixed", show_bathy_contour=TRUE, show_bathy_fill=TRUE, x_buffer = 0.5, y_buffer = 0.5) {
  # Check if required packages are installed and load them
  required_pkgs <- c("ggplot2", "mapdata", "sf", "marmap", "cmocean")
  missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()]
  if (length(missing_pkgs) > 0) {
    # Some packages are missing, install them
    install.packages(missing_pkgs)
  }
  lapply(required_pkgs, require, character.only = TRUE)

  # Set map limits
  min_lon <- min(df$lon) - x_buffer
  max_lon <- max(df$lon) + x_buffer
  min_lat <- min(df$lat) - y_buffer
  max_lat <- max(df$lat) + y_buffer

  # Get world map
  map <- map_data("world")

  # Filter for australia map
  australia_map <- subset(map, region == "Australia")

  # Get bathymetry data for the region
  bathy <- getNOAA.bathy(
    lon1 = min_lon, lon2 = max_lon, lat1 = min_lat, lat2 = max_lat,
    keep = TRUE,
    path = "./data/bathymetry_marmap"
  )
  bathy[bathy > 500] <- NA

  bathy_df <- fortify.bathy(bathy)

  if(show_bathy_fill){
    p <- ggplot() +
      geom_tile(data = bathy_df, aes(x = x, y = y, fill = z)) + 
      # scale_fill_gradient2(
      #   low = "#2a363bff", mid = "snow", high = "white",
      #   guide = guide_colorbar(title = "Depth (m)"),
      #   na.value = "transparent"
      # ) 
      scale_fill_cmocean(
        name = "deep",
        na.value = "transparent",
        direction = "-1"
      ) 
  }
  
    
  if(show_bathy_contour){
    p <- p + geom_contour(data = bathy_df, aes(x, y, z = z), color = "gray50")
  }


  # Plot ggplot
  if (missing(shapefile)) {
    p <- p
  } else {
    p <- p +
      geom_sf(data = shapefile, fill = "#8CBCB9", color = 'lightblue', alpha = 0.5)
  }

  p <- p +
    # if you want to use coords format
    geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), fill = "grey50") +
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) +
    # if you want to use the "normal" format
    # geom_map(map_id ='Australia', map = map, fill = 'grey50') +
    # lims(x = c(min_lon, max_lon), y = c(min_lat, max_lat)) +
    # plot track
    geom_path(data = df, aes(lon, lat), alpha = 0.5) +
    theme_bw()

  if (facet) {
    p <- p + facet_wrap(~id, scales = facet_scales)
  }

  return(p)
}


add_map_limits <- function(p, df, type="coord_sf", x_buffer=0.5, y_buffer=0.5) {
  min_lon <- min(df$lon) - x_buffer
  max_lon <- max(df$lon) + x_buffer
  min_lat <- min(df$lat) - y_buffer
  max_lat <- max(df$lat) + y_buffer
  
  if(type == "coord_sf") {
    p <- p + coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat))
  } else if(type == "lims") {
    p <- p + lims(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
  }
  return(p)
}



plot_tracks_tmap <- function(df, shapefile, facet = TRUE) {
  # Install and load the pacman package
  if (!require("pacman")) {
    install.packages("pacman")
  }

  library(pacman)

  # Check and load required packages
  p_load(tmap)

  # Get world map
  world_map <- tm_shape(tm_basemap("Esri.WorldGrayCanvas")) +
    tm_polygons()

  # Filter for Australia map
  australia_map <- tm_shape(world_map) +
    tm_polygons("region",
      name = "Australia",
      palette = "grey90"
    )

  # Plot tmap
  p <- tm_shape(df) +
    tm_lines(col = "date", alpha = 0.5) +
    tm_symbols(col = "date", size = 0.7) +
    tm_layout(frame = FALSE, bg.color = "white")

  # Set plot limits
  p <- p + tm_view("map", bbox = st_bbox(df))

  # Add shapefile if provided
  if (!missing(shapefile)) {
    p <- p + tm_shape(shapefile) +
      tm_polygons(alpha = 0.3, fill = "#8CBCB9")
  }

  # Add facet if required
  if (facet) {
    p <- p + tm_facets(by = "id")
  }

  p
}

read_wc_locations_csv <- function(path, pattern) {
  # Install and load the pacman package
  if (!require("pacman")) {
    install.packages("pacman")
  }

  library(pacman)

  # Check and load required packages
  p_load(readr)

  files <- list.files(
    path = path,
    recursive = TRUE,
    pattern = pattern,
    full.names = TRUE
  )

  message(paste0(length(files), " files found"))
  message(paste(files, "\n"))

  d <- read_csv(files, id = "file_name")

  return(d)
}

read_locations_csv <- function(path, pattern) {
  # Install and load the pacman package
  if (!require("pacman")) {
    install.packages("pacman")
  }

  library(pacman)

  # Check and load required packages
  p_load(dplyr, readr)

  files <- list.files(
    path = path,
    recursive = TRUE,
    pattern = pattern,
    full.names = TRUE
  )

  message(paste0(length(files), " files found"))
  message(paste(files, "\n"))

  # Read the first csv file to get the column names
  first_file <- files[1]
  first_df <- read_csv(first_file, id = "file")
  col_names <- colnames(first_df)

  # Read all csv files and bind them together,
  # filling in missing columns with NA
  d <- list()
  error_files <- NULL
  for (file in files) {
    tryCatch(
      {
        df <- read_csv(file, id = "file")
        missing_cols <- setdiff(col_names, colnames(df))
        for (col in missing_cols) {
          df[[col]] <- NA
        }
        d <- bind_rows(d, df)
      },
      error = function(e) {
        error_files <<- c(error_files, file)
        message(paste0("Error reading file: \n", file, ". \nSkipping file."))
      }
    )
  }

  if (length(error_files) > 0) {
    message(paste0("Finished reading files. ", length(error_files), " files skipped due to errors:"))
    message(error_files)
  } else {
    message("Finished reading files. No errors detected.")
  }

  return(list(d, error_files))
}


clean_wc_locations <- function(d) {
  # Install and load the pacman package
  if (!require("pacman")) {
    install.packages("pacman")
  }

  library(pacman)

  # Check and load required packages
  p_load(janitor, lubridate)

  # Format column names
  d <- janitor::clean_names(d)

  original_nrow <- nrow(d)

  # Rename columns
  d <- d %>%
    rename(
      id = deploy_id, lat = latitude, lon = longitude, lc = quality,
      smaj = error_semi_major_axis,
      smin = error_semi_minor_axis,
      eor = error_ellipse_orientation
    ) %>%
    filter(!is.na(lat))

  remaining_nrow <- nrow(d)

  message(paste0(remaining_nrow / original_nrow * 100, "% of rows remaining"))

  # Parse column types
  d$id <- as.character(d$id)
  d$date <- parse_date_time(d$date, "HMSdbY")


  return(d)
}

clean_catlogger_data <- function(d) {
  # Install and load the pacman package
  if (!require("pacman")) {
    install.packages("pacman")
  }

  library(pacman)

  # Check and load required packages
  p_load(janitor, lubridate)

  # remove na columns
  keep <- !apply(is.na(d), 2, all)

  # Subset data frame to keep only non-empty columns
  d <- d[, keep]

  # Format column names
  d <- janitor::clean_names(d)

  original_nrow <- nrow(d)

  d <- d %>%
    mutate(datetime = mdy_hms(paste0(date, time)), lc = "G") %>%
    select(
      file,
      id,
      datetime,
      latitude,
      longitude,
      lc,
      altitude,
      satellites
    ) %>%
    rename(date = datetime, lat = latitude, lon = longitude) %>%
    filter(!is.na(lat))

  remaining_nrow <- nrow(d)

  message(paste0(remaining_nrow / original_nrow * 100, "% of rows remaining"))

  d$id <- as.character(d$id)

  return(d)
}


publication_theme <- function() {
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
}





# Ecotone shearwater functions --------------------------------------------

# Define function to process each group of birds (this is just a helper function)
process_birds <- function(birds, data) {
  if (nrow(birds) == 0) {
    return(NULL)
  }

  if (nrow(birds) == 1) {
    data$id <- birds$bird_name
    data <- data %>% filter(date >= birds$date)
    message(birds$bird_name)

    if (birds$date != min(data$date) %>% as.Date()) {
      stop("Error: birds date is not equal to the minimum date in data")
    }
    return(data)
  }

  if (nrow(birds) > 1) {
    tag <- first(birds$tag_id)
    message("...", tag, " has multiple birds")
    birds_desc <- birds %>% arrange(desc(date))

    purrr::map(seq_along(birds_desc$date), ~ {
      # set up new params for current bird
      i <- .x
      new_tag <- paste(tag, i, sep = "_") # format eg. SHE01_1
      startdate <- birds_desc$date[i]
      birdname <- birds_desc$bird_name[i]

      # create df for current bird
      this_bird <- data %>%
        filter(date >= startdate) %>%
        mutate(id = birdname, tag_id = new_tag)

      # remove this_bird rows from original df
      data <<- anti_join(data, this_bird, by = "date")

      # return this_bird
      message(birdname)

      if (startdate != min(this_bird$date) %>% as.Date()) {
        stop("Error: birds date is not equal to the minimum date in data")
      }

      # if not last deployed bird, check if this bird's max date overlaps with the earlier bird's startdate
      if ((i > 1) &&
        (max(this_bird$date) %>% as.Date() >= birds_desc$date[i - 1])) {
        stop("Error: this bird's date overlaps with the next bird")
      }

      return(this_bird)
    })
  }
}


process_ecotone_data <- function(deploy_df, gps_df) {
  distinct(deploy_df, tag_id) %>%
    mutate(
      birds = purrr::map(tag_id, ~ filter(deploy_df, tag_id == .x)),
      data = purrr::map(tag_id, ~ filter(gps_df, tag_id == .x))
    ) %>%
    filter(!is.na(tag_id)) %>%
    mutate(cleaned_data = purrr::map2(birds, data, process_birds)) %>%
    pull(cleaned_data) %>%
    reduce(bind_rows)
}


save_plot_results <- function(plot, filename, width = 12, height = 10) {
  today <- Sys.Date()
  output_filename <- filename 
  dir.create(paste0("./results/", today))
  ggsave(paste0("./results/", today, "/", output_filename), plot = plot, units = "in", width = width, height = height, dpi = 300)
}

save_animation_results <- function(filename, ...) {
  today <- format(Sys.Date(), "%Y-%m-%d")
  output_filename <- filename
  dir.create(paste0("./results/", today))
  anim_save(paste0("./results/", today, "/", output_filename), ...)
}



last_date <- function(x) {
  x$date %>% max() %>% as.Date()
}



# file functions -----------------------------------------------------------

# Function to load the latest file
load_latest_rds <- function(filename, parent_dir = "./results/", force_fit = FALSE) {
  if (force_fit) {
    return (NULL)
  }
  
  # Generate the base directory path
  base_dir <- parent_dir 
    
  # List all subdirectories within the base directory
  date_dirs <- list.dirs(base_dir, recursive = FALSE)
  
  # Extract dates from directory names and convert to Date objects
  dir_dates <- as.Date(basename(date_dirs))
  
  # Select the latest date directory
  latest_date_dir <- date_dirs[which.max(dir_dates)]
  
  # Generate the full file path
  file_path <- file.path(latest_date_dir, filename)
  
  # Check if the file already exists
  if (file.exists(file_path)) {
    # If the file exists, load the fit
    file <- read_rds(file_path)
    message(paste("Existing file loaded from", file_path))
    return(file)
  } else {
    # If the file does not exist, return NULL
    message("No existing file found.")
    return(NULL)
  }
}

# Function to save a file
save_rds <- function(file, filename, parent_dir = "./results/") {
  # Generate the directory path
  date_dir <- paste0(parent_dir, Sys.Date(), "/")
  
  # Check if the directory exists, if not, create it
  if (!dir.exists(date_dir)) {
    dir.create(date_dir, recursive = TRUE)
  }
  
  # Generate the full file path
  file_path <- paste0(date_dir, filename)
  
  # Save the file
  saveRDS(file, file_path)
  message(paste("File saved to", file_path))
}

# Function to get the path of the latest file
get_latest_filepath <- function(filename, parent_dir = "./results/") {
  # Generate the base directory path
  base_dir <- parent_dir 
  
  # List all subdirectories within the base directory
  date_dirs <- list.dirs(base_dir, recursive = FALSE)
  
  # Extract dates from directory names and convert to Date objects
  dir_dates <- as.Date(basename(date_dirs))
  
  # Select the latest date directory
  latest_date_dir <- date_dirs[which.max(dir_dates)]
  
  # Generate the full file path
  file_path <- file.path(latest_date_dir, filename)
  
  # Check if the file already exists
  if (file.exists(file_path)) {
    message(paste("Latest file path:", file_path))
    return(file_path)
  } else {
    message("No existing file found.")
    return(NULL)
  }
}

# Function to generate the path for saving a file
generate_filepath <- function(filename, parent_dir = "./results/") {
  # Generate the directory path
  date_dir <- paste0(parent_dir, Sys.Date(), "/")
  
  # Check if the directory exists, if not, create it
  if (!dir.exists(date_dir)) {
    dir.create(date_dir, recursive = TRUE)
  }
  
  # Generate the full file path
  file_path <- paste0(date_dir, filename)
  
  message(paste("File path for saving:", file_path))
  return(file_path)
}


