# Process and plot ecotone shearwater tracks
source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/utils.R")

# set the working directory to the folder containing the data files
dir <- "./data/original/shearwaters/ecotone"

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


# save coordinates of maatsuyker -43.6679° S, 146.3122° E
maatsuyker <- c(-43.6679, 146.3122)

# calculate distance from maatsuyker (in metres)
gps_clean <- gps_clean %>%
    mutate(
        distance_from_maatsuyker =
            track_distance_to(lon, lat, maatsuyker[2], maatsuyker[1])
    )


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

# print to console 'check if trip_id is correct'
print("check if number of trips is correct")
print(table(gps_clean$tag_id, gps_clean$trip_id))

# save and export gps_clean as stsw_gps to use in other scripts
stsw_gps <- gps_clean
save(stsw_gps,
    file = paste0(clean_dir, "stsw_gps_ecotone.RData")
)
