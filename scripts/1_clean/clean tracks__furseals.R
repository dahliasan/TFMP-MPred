source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/ids.R")
source("./scripts/utils.R")

# Load wildlife computers data
d <- read_wc_locations_csv("./data/original/fur seals/", "-\\d+-Locations\\.csv")

# Clean data from WC portal
d <- clean_wc_locations(d)

# Get juvenile / adult female data and add age class column
d <- d %>% filter(!is.na(lc))

juv <- d %>%
    filter(id %in% juvenile_ids) %>%
    mutate(age_class = "juvenile")

adult <- d %>%
    filter(!id %in% juvenile_ids) %>%
    mutate(age_class = "adult")

# Combine data
d <- bind_rows(juv, adult)

seal_gps <- d

save(seal_gps, file = paste0(clean_dir, "seal_gps.RData"))
