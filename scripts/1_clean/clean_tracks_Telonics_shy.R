# read and clean shy albatross argos data

source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/utils.R")

# Load wildlife computers data
d <- read_wc_locations_csv("./data/original/albatross", "-Locations\\.csv")

# Clean data from WC portal
d1 <- clean_wc_locations(d)

# Get actual deployment start and end dates
deploy <- shy_deployment %>%
    filter(logger_type == "Telonics Sat Tag") %>%
    select(ptt_id, deployment_date, retrieval_date) %>%
    rename(id = ptt_id) %>%
    mutate(id = id %>% as.character())

# convert deployment dates in australian timezone to UTC
deploy$deployment_date <- as.POSIXct(deploy$deployment_date, tz = "Australia/Hobart")

# for 238318, force tz to be UTC to remove outlier (visual assessment) using lubridate
deploy$deployment_date[deploy$id == "238318"] <- ymd_hms("2022-12-16 11:00:00", tz = "UTC")


d1 <- left_join(d1, deploy)
d1 <- d1 %>% filter(date >= deployment_date)
d1 <- d1 %>% filter(!is.na(lc))







shy_gps <- d1

save(shy_gps, file = paste0(clean_dir, "shy_gps.RData"))
